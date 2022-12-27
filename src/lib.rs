//! A simple ENDF-6 I/O Library
//!
//! [ENDF-6](https://www-nds.iaea.org/public/endf/endf-manual.pdf) is the
//! standard format for distribution of nuclear data files. `endfio` aims to be
//! a small library to allow reading, modifying and creating the nuclear data files
//! in that format.
//!
//! `endfio` does not aim to be a full ENDF-6 parser, which can load and
//! interpret data in different files in section. It aims to need the smallest
//! amount of knowledge about the contents required to be able to load and print
//! correctly formatted ENDF-6 files.
//!
//! As such it cannot and does not aim to perform checks that verify physical
//! validity of the data. This will be a task for different library.
//!
pub mod conversions;
pub mod data_structures;
pub mod error;
pub mod metadata;
pub mod records;

use core::fmt;
use itertools::{Either, Itertools};
use std::collections::BTreeMap;
use std::io::{self, BufRead, Write};
use std::ops::Index;
use std::str::FromStr;
use std::vec::Vec;

use crate::conversions::{to_f64, to_i32};
use crate::data_structures::{InterpolationType, Table};
use crate::error::{EndfError, FormatError, FormatErrorKind, ParseError};
use crate::metadata::{
    DataType, EvaluationInfo, IncidentParticle, LibraryInfo, NuclideInfo, ResonanceInfo,
};
use crate::records::{ContRecord, DirRecord, ListRecord, Record, Table2Record, TableRecord};

#[derive(Debug, Hash, PartialEq, Eq)]
struct Control {
    pub mat: i32,
    pub mf: i32,
    pub mt: i32,
}

impl Control {
    fn read(line: &str) -> Result<Control, ParseError> {
        let mat = to_i32(&line[66..70])?;
        let mf = to_i32(&line[70..72])?;
        let mt = to_i32(&line[72..75])?;
        Ok(Control { mat, mf, mt })
    }

    fn is_valid(&self) -> bool {
        self.mat > 0 && self.mf > 0 && self.mt > 0
    }
}

/// Type alias for File type specifier MF
pub type MF = i32;
/// Type alias for Section type specifier MT
pub type MT = i32;

///
/// Struct that represents a MAT number
///
#[derive(Hash, Eq, PartialEq, Debug, Clone, Copy, PartialOrd, Ord)]
pub struct MAT {
    number: i32,
}

impl MAT {
    ///
    /// Return human-readable name of nuclide/compound
    ///
    /// Gives `???` if the name cannot be determined with [metadata::mat_to_str].
    ///
    pub fn name(&self) -> String {
        metadata::mat_to_str(self.number).unwrap_or("???".to_string())
    }
}

impl FromStr for MAT {
    type Err = FormatError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let number = metadata::str_to_mat(s)?;
        Ok(Self { number })
    }
}

impl TryFrom<i32> for MAT {
    type Error = FormatError;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        if !(0..9999).contains(&value) {
            Err(FormatError::new(
                FormatErrorKind::InvalidMATRange { mat: value },
                "",
            ))
        } else {
            Ok(Self { number: value })
        }
    }
}

impl fmt::Display for MAT {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} [{}]", self.number, self.name())
    }
}

///
/// An item of nuclear data
///
/// Section is a sequence of ENDF [Record]s which contain nuclear data.
/// The meaning and type of the data is dependent on a particular [File] type
/// and specifier of the section [MT].
///
/// Each section has a *modification sequence* number. Basically a counter
/// that counts number of times a data in a section was changed since originally
/// published.
///
/// Section can be taken from a [Tape] that has been read from a file or created
/// from scratch as follows:
/// ```
/// use endfio::Section;
/// use endfio::records::{Record, DirRecord};
///
/// let mut sec = Section::default();
/// sec.set_mod_seq(1);
///
/// // Data can be add via an iterator over Records
/// sec.extend(vec![DirRecord{l1: 1, l2: 1, n1:1, n2:1}].into_iter());
///
/// // Or Record by Record
/// sec.append_record(&DirRecord{l1: 2, l2: 2, n1: 2, n2: 2});
///
///
/// // We can iterate over section to display its contents
/// for line in &sec {
///   println!("{line}");
/// }
/// ```
///
#[derive(Debug, Default)]
pub struct Section {
    text: String,
    mod_seq: u32,
}

impl Section {
    fn append(&mut self, line: &str) {
        self.text.push_str(line)
    }

    ///
    /// Extend the section with an extra [Record]
    ///
    pub fn append_record(&mut self, record: &impl records::Record) {
        self.text.push_str(&record.write())
    }

    ///
    /// Get iterator over lines of the Section
    ///
    pub fn iter(&self) -> SectionIterator {
        SectionIterator {
            section: self,
            pos: 0,
        }
    }

    ///
    /// Create a new, empty section
    ///
    pub fn new() -> Self {
        Self {
            text: String::new(),
            mod_seq: 0,
        }
    }

    ///
    /// Return number of records in the Section
    ///
    pub fn len(&self) -> usize {
        self.text.len() / 66
    }

    ///
    /// Return True is section is empty
    ///
    pub fn is_empty(&self) -> bool {
        self.text.len() == 0
    }

    ///
    /// Set the value of modification sequence
    ///
    pub fn set_mod_seq(&mut self, new_seq: u32) {
        self.mod_seq = new_seq;
    }

    ///
    /// Get modification sequence
    ///
    /// Modification sequence counts the number of times a section was changed
    /// since the first evaluation
    ///
    pub fn get_mod_seq(&self) -> u32 {
        self.mod_seq
    }

    ///
    /// Write a text of the section
    ///
    /// Requires control MAT, MF and MT in order to write the control part on each line
    ///
    fn print(&self, stream: &mut impl Write, mat: MAT, mf: MF, mt: MT) -> Result<(), EndfError> {
        // Print contents
        for (i, line) in self.iter().enumerate() {
            writeln!(
                stream,
                "{}{:>4}{:>2}{:>3}{:>5}",
                line,
                mat.number,
                mf,
                mt,
                i + 1
            )?;
        }
        // Finish with a SEND record
        writeln!(stream, "{:>66}{:>4}{:>2}{:>3}99999", "", mat.number, mf, 0)?;
        Ok(())
    }
}

///
/// Implement conversion into iterator over references
///
impl<'a> IntoIterator for &'a Section {
    type IntoIter = SectionIterator<'a>;
    type Item = &'a str;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

///
/// Allow section to be extended by a sequence of [Record]s
///
impl<A> std::iter::Extend<A> for Section
where
    A: Record,
{
    fn extend<T: IntoIterator<Item = A>>(&mut self, iter: T) {
        for record in iter {
            self.append_record(&record);
        }
    }
}

///
/// Iterator over the lines of ENDF [`Section`][Section]
///
pub struct SectionIterator<'a> {
    section: &'a Section,
    pos: usize,
}

impl<'a> Iterator for SectionIterator<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        self.pos += 66;
        if self.pos > self.section.text.len() {
            return None;
        }
        Some(&self.section.text[self.pos - 66..self.pos])
    }
}

///
/// Read a TEXT record and advance iterator by a single line
///
pub fn get_text<'a>(iter: &mut impl std::iter::Iterator<Item = &'a str>) -> Option<&'a str> {
    iter.next()
}

///
/// Read a CONT record and advance iterator by a single line
///
pub fn get_cont<'a>(
    iter: &mut impl std::iter::Iterator<Item = &'a str>,
) -> Result<ContRecord, EndfError> {
    let line = iter.next();
    match line {
        Some(l) => Ok(ContRecord::from_string(l)?),
        None => Err(EndfError::EndOfSection),
    }
}

///
/// Read a HEAD record and advance iterator by a single line
///
pub fn get_head<'a>(
    iter: &mut impl std::iter::Iterator<Item = &'a str>,
) -> Result<ContRecord, EndfError> {
    get_cont(iter)
}

///
/// Read a LIST record and advance iterator by multiple lines
///
pub fn get_list<'a>(
    iter: &mut impl std::iter::Iterator<Item = &'a str>,
) -> Result<ListRecord, EndfError> {
    // Read Header
    let header = get_cont(iter)?;
    let n = header.n1;

    // Create iterator over the number fields
    let n_lines = n / 6 + i32::from(n % 6 != 0);
    let lines = iter.take(n_lines.try_into().unwrap());
    let numbers = lines
        .flat_map(|x| {
            x.as_bytes()
                .chunks(11)
                .map(|x| std::str::from_utf8(x).unwrap())
        })
        .take(n as usize)
        .map(|x| to_f64(x));

    // Convert to a vector and move result to front
    let list = numbers.collect::<Result<Vec<_>, _>>();

    Ok(ListRecord::new(
        header.c1, header.c2, header.l1, header.l2, header.n2, list?,
    ))
}

///
/// Read TAB1 record and advance iterator by multiple lines
///
pub fn get_tab1<'a>(
    iter: &mut impl std::iter::Iterator<Item = &'a str>,
) -> Result<TableRecord, EndfError> {
    // Read Header
    let head = get_cont(iter)?;
    let n_regions = head.n1;
    let n_points = head.n2;

    let (regions, interpolation) = read_interpolation_regions(iter, n_regions)?;

    if n_points < 0 {
        return Err(FormatError::misc("Negative number of data pointsin the table").into());
    }

    // Read the data
    // Create iterator over numbers
    let n_lines = 2 * n_points / 6 + i32::from(2 * n_points % 6 != 0);
    let lines = iter.take(n_lines.try_into().unwrap());
    let numbers = lines
        .flat_map(|x| {
            x.as_bytes()
                .chunks(11)
                .map(|x| std::str::from_utf8(x).unwrap())
        })
        .take((2 * n_points) as usize)
        .map(|x| to_f64(x));

    // Partition into x and y parts
    let (x, y): (Vec<_>, Vec<_>) = numbers.enumerate().partition_map(|(i, num)| {
        if i % 2 == 0 {
            Either::Left(num)
        } else {
            Either::Right(num)
        }
    });

    // We need to process errors that may be present in x & y
    Ok(TableRecord {
        c1: head.c1,
        c2: head.c2,
        l1: head.l1,
        l2: head.l2,
        table: Table::from(
            x.into_iter().collect::<Result<Vec<_>, _>>()?,
            y.into_iter().collect::<Result<Vec<_>, _>>()?,
            interpolation,
            regions,
        )?,
    })
}

///
/// Read interpolation region entry from the iterator
///
/// Used in TAB1 and TAB2 records
/// Changes the indexing of regions from 1-indexing of ENDF into 0-indexing of RUST
///
/// # Arguments
/// - `iter` - Iterator over ENDF 66-character long lines
/// - `n` - Number of interpolation regions to read (data contains `2n` integers)
///
fn read_interpolation_regions<'a>(
    iter: &mut impl std::iter::Iterator<Item = &'a str>,
    n: i32,
) -> Result<(Vec<usize>, Vec<InterpolationType>), EndfError> {
    if n < 0 {
        return Err(FormatError::misc("Number of interpolation regions is negative").into());
    }
    // Create iterator over 11-character fields with the numbers
    let n = n as usize;
    let n_lines = 2 * n / 6 + usize::from(2 * n % 6 != 0);
    let lines = iter.take(n_lines);
    let numbers = lines
        .flat_map(|x| {
            x.as_bytes()
                .chunks(11)
                .map(|x| std::str::from_utf8(x).unwrap())
        })
        .take(2 * n)
        .map(|x| to_i32(x));

    // Partition iterator into regions an Interpolation parts
    // In ENDF regions are given with 1-indexing
    // we need to convert them to RUST 0-indexing
    let (regions, interpolation): (Vec<_>, Vec<_>) =
        numbers.enumerate().partition_map(|(i, num)| {
            if i % 2 == 0 {
                Either::Left(num.and_then(|x| Ok((x - 1) as usize)))
            } else {
                Either::Right(num)
            }
        });

    // Deal with read error on interpolation flags
    let interpolation = interpolation.into_iter().collect::<Result<Vec<_>, _>>()?;

    // Collect the iterators
    let interpolation = interpolation
        .into_iter()
        .map(|n| InterpolationType::from_flag(n))
        .collect::<Result<Vec<_>, _>>()?;
    let regions = regions.into_iter().collect::<Result<Vec<_>, _>>()?;

    // Verify sizes and return result
    if regions.len() != interpolation.len() || regions.len() != n {
        Err(FormatError::misc("Read interpolation data has invalid size").into())
    } else {
        Ok((regions, interpolation))
    }
}

///
/// Read TAB2 record and advance iterator by multiple lines
///
pub fn get_tab2<'a>(
    iter: &mut impl std::iter::Iterator<Item = &'a str>,
) -> Result<Table2Record, EndfError> {
    let head = get_cont(iter)?;
    let n_regions = head.n1;
    let nz = head.n2;

    let (regions, interpolation) = read_interpolation_regions(iter, n_regions)?;

    Ok(Table2Record {
        c1: head.c1,
        c2: head.c2,
        l1: head.l1,
        l2: head.l2,
        nz: nz as usize,
        interpolation,
        regions,
    })
}

///
/// Represents a group of related nuclear data (e.g. cross-sections)
///
/// Acts like a dictionary that maps between section specifier [MT] and its
/// contents.
/// ```
/// use endfio::{File, Section};
/// use std::collections::BTreeMap;
///
/// // We can create a file from a Map
/// let mut file = BTreeMap::from([
///     (1, Section::default()),
///     (501, Section::default()),
///     (2, Section::default()),
///     ]);
///
/// // But we can insert elements as well
/// file.insert(3, Section::default());
///
/// // As well as remove them
/// file.remove(&501);
///
/// // When we wish to process a file we can use it as an iterator
/// assert_eq!(
///     vec![1, 2, 3],
///     file.into_iter().map(|(k, v)| k).collect::<Vec<_>>()
/// );
/// ```
///
#[derive(Debug, Default)]
pub struct File {
    contents: BTreeMap<MT, Section>,
}

///
/// Add indexing into the Material
///
impl Index<MT> for File {
    type Output = Section;
    fn index(&self, index: MT) -> &Self::Output {
        self.get(index).unwrap()
    }
}

///
/// Implement conversion of a file reference into iterator over references
///
impl<'a> IntoIterator for &'a File {
    type Item = (&'a MT, &'a Section);
    type IntoIter = std::collections::btree_map::Iter<'a, MT, Section>;

    fn into_iter(self) -> Self::IntoIter {
        self.contents.iter()
    }
}

///
/// Implement conversion into consuming iterator as well
///
impl IntoIterator for File {
    type Item = (MT, Section);
    type IntoIter = std::collections::btree_map::IntoIter<MT, Section>;

    fn into_iter(self) -> Self::IntoIter {
        self.contents.into_iter()
    }
}

impl<'a> IntoIterator for &'a mut File {
    type Item = (&'a MT, &'a mut Section);
    type IntoIter = std::collections::btree_map::IterMut<'a, MT, Section>;

    fn into_iter(self) -> Self::IntoIter {
        self.contents.iter_mut()
    }
}

impl File {
    ///
    /// Create new empty file
    ///
    pub fn new() -> Self {
        Self {
            contents: BTreeMap::new(),
        }
    }

    ///
    /// Get a Section from File
    ///
    /// `None` if the given `mt` is not present
    ///
    pub fn get(&self, mt: MT) -> Option<&Section> {
        self.contents.get(&mt)
    }

    ///
    /// Get mutable reference to a section
    ///
    /// `None` if the given `mt` is not present
    ///
    pub fn get_mut(&mut self, mt: MT) -> Option<&mut Section> {
        self.contents.get_mut(&mt)
    }

    ///
    /// Insert [MT], [Section] pair
    ///
    /// If the File did not have `mt`, `None` is returned.
    /// If the File did have this `mt`, the value is updated and old value
    /// is returned.
    ///
    pub fn insert(&mut self, mt: MT, value: Section) -> Option<Section> {
        self.contents.insert(mt, value)
    }

    ///
    /// Remove a Section from a File
    ///
    pub fn remove(&mut self, key: MT) {
        self.contents.remove(&key);
    }

    ///
    /// Write all sections to text
    ///
    /// Writes sections in arbitrary order.
    ///
    fn print(&self, stream: &mut impl Write, mat: MAT, mf: MF) -> Result<(), EndfError> {
        // Print contents
        for (mt, section) in self {
            section.print(stream, mat, mf, *mt)?
        }
        // Finish up with FEND record
        writeln!(stream, "{:>66}{:>4}{:>2}{:>3}    0", "", mat.number, 0, 0)?;
        Ok(())
    }
}

///
/// Allow the file to be build from a map
///
impl From<BTreeMap<MT, Section>> for File {
    fn from(value: BTreeMap<MF, Section>) -> Self {
        Self { contents: value }
    }
}

///
/// Evaluated Nuclear Data for a single Isotope/Element
///
/// Each material is a collection of [File]s, each representing
/// a different type of data. However, unlike a File, Material also stores
/// extra data about the Library, evaluation and physical properties. These
/// are contained in a special `MF=1 MT=451` section which needs to be present
/// in every Material, which is loaded from a file.
///
/// If the Material is created from scratch the extra data needs to be provided
/// on creation. Sadly it makes the process quite involved.
/// ```
/// // Library Type
/// let lib: endfio::metadata::LibraryInfo = "JEFF 3.1".parse()?;
/// // Physical Data
/// let data = endfio::metadata::NuclideInfo::new(
///     1,
///     2,
///     2.0,
///     endfio::metadata::ResonanceInfo::NoResonance,
///     false,
///     0.0,
///     true,
///     0,
///     0,
/// )?;
/// // Data about evaluation
/// let eval = endfio::metadata::EvaluationInfo::new(
///     0,
///     0,
///     "Best lab",
///     "Me",
///     "None",
///     0.0,
///     2.0e7,
///     ("1997-03-2".parse().unwrap(), None, None, None),
/// )?;
///
/// // Contents HSUB section
/// let comment = "\
/// Contents of the comment will be printed in the descriptive
/// text section inside MF=1 MT=451.
/// Each line can contain up to 66 ascii characters. If it is longer, as this one,
/// it will be folded.
/// Otherwise newline characters will be respected
/// ";
///
/// // Finally we can create the material
/// let mat = endfio::Material::new(
///     128.try_into()?,
///     lib,
///     data,
///     eval,
///     endfio::metadata::IncidentParticle::Photon,
///     endfio::metadata::DataType::NuclearInteractionData,
///     comment)?;
///
/// // But note that the MF=1 MT=451 section is not present
/// // It is contained in the header only and cannot be accessed like other data
/// assert!(mat.get(1).and_then(|file| file.get(451)).is_none());
///
///
/// # Ok::<(), endfio::error::EndfError>(())
/// ```

#[derive(Debug)]
pub struct Material {
    header: MaterialHeader,
    contents: BTreeMap<MF, File>,
}

///
/// Add indexing into the Material
///
impl Index<MF> for Material {
    type Output = File;
    fn index(&self, index: MF) -> &Self::Output {
        self.get(index).unwrap()
    }
}

///
/// Implement conversion of a Material reference into iterator over references to its contents
///
impl<'a> IntoIterator for &'a Material {
    type Item = (&'a MF, &'a File);
    type IntoIter = std::collections::btree_map::Iter<'a, MF, File>;

    fn into_iter(self) -> Self::IntoIter {
        self.contents.iter()
    }
}

///
/// Consuming iterator as well
///
impl IntoIterator for Material {
    type Item = (MF, File);
    type IntoIter = std::collections::btree_map::IntoIter<MF, File>;

    fn into_iter(self) -> Self::IntoIter {
        self.contents.into_iter()
    }
}

impl Material {
    ///
    /// Create a new instance of material
    ///
    /// Creates new material from scratch. Generates the header (MF=1 MT=451)
    /// section automatically from the provided data.
    ///
    /// # Arguments
    /// * `mat`- [MAT] number for the material. It is necessary to know it to
    ///           generate the HSUB *overture*.
    /// * `library` - Library info
    /// * `data` - Physical info about the nuclide
    /// * `evaluation` - Info about this particular evaluation
    /// * `part` - Type of the incident particle
    /// * `kind` - Type of the data stored in this Material
    /// * `comment` - A text comment contained in the header. Respects newlines
    ///               folds lines longer than 66 characters. Must contain only
    ///               ASCII characters.
    pub fn new(
        mat: MAT,
        library: LibraryInfo,
        data: NuclideInfo,
        evaluation: EvaluationInfo,
        part: IncidentParticle,
        kind: DataType,
        comment: &str,
    ) -> Result<Self, FormatError> {
        use std::fmt::Write; // Need this import to use write! on String

        // Create the comment header lines according to the manual
        // Note that ENDF Manual may use an old term IREV when referring to IREL
        let (nver, lrel) = library.get_version();
        let rev_text = if *lrel == 0 {
            String::new()
        } else {
            format!("REVISION {lrel}")
        };
        let lib_text = format!("{}-{}", library.get_name(), nver);

        let mut text = String::new();
        write!(
            text,
            "----{:<18}MATERIAL {:>4}        {:<23}",
            lib_text, mat.number, rev_text
        )
        .unwrap();
        write!(text, "-----{:<61}", metadata::sublibrary_name(&part, &kind)).unwrap();
        write!(
            text,
            "------ENDF-6                                                      "
        )
        .unwrap();

        // Assert that string contains ascii characters only
        if !comment.is_ascii() {
            return Err(FormatError::new(FormatErrorKind::NonASCII, comment));
        }

        // Print the user provided comment
        // Replace newlines with padding to 66 characters
        // Break lines longer than 66 characters
        for line in comment.lines() {
            for chunk in line
                .as_bytes()
                .chunks(66)
                .map(|x| std::str::from_utf8(x).unwrap())
            {
                write!(text, "{:<66}", chunk).unwrap();
            }
        }

        // Process the comment
        // Create the header
        let header = MaterialHeader {
            library,
            evaluation,
            data,
            part,
            kind,
            comment: text,
        };

        // Return the material
        Ok(Self {
            header,
            contents: BTreeMap::<_, _>::default(),
        })
    }

    ///
    /// Get a File from Material
    ///
    /// `None` if a file is not present.
    ///
    pub fn get(&self, mf: MF) -> Option<&File> {
        self.contents.get(&mf)
    }

    ///
    /// Get a mutable File from Material
    ///
    /// `None` if a file is not present.
    ///
    pub fn get_mut(&mut self, mf: MF) -> Option<&mut File> {
        self.contents.get_mut(&mf)
    }

    ///
    /// Get reference to the [MaterialHeader]
    ///
    pub fn get_header(&self) -> &MaterialHeader {
        &self.header
    }

    ///
    /// Get mutable reference to the [MaterialHeader]
    ///
    pub fn get_header_mut(&mut self) -> &mut MaterialHeader {
        &mut self.header
    }

    ///
    /// Insert [MF], [File] pair
    ///
    /// If the Material did not have `mf`, `None` is returned.
    /// If the Material did have this `mf`, the value is updated and old value
    /// is returned.
    ///
    pub fn insert(&mut self, mf: MF, value: File) -> Option<File> {
        self.contents.insert(mf, value)
    }

    ///
    /// Write all files to text
    ///
    fn print(&self, stream: &mut impl std::io::Write, mat: MAT) -> Result<(), EndfError> {
        // Count number of sections
        // We need '1 +' to account for the header section!
        let num_sections = 1 + self
            .contents
            .iter()
            .flat_map(|(_, file)| file.into_iter())
            .count();

        // Print the header
        let mut count = self.header.print(stream, mat, num_sections as i32)?;

        // Print the contents
        // Print the entry for the header section
        let num_records = count + num_sections as u32 - 1;
        writeln!(
            stream,
            "{:<66}{:>4}{:>2}{:>3}{:>5}",
            DirRecord {
                l1: 1,
                l2: 451,
                n1: num_records as i32,
                n2: *self.header.evaluation.get_mod_seq() as i32
            }
            .write(),
            mat.number,
            1,
            451,
            count
        )?;
        count += 1;

        // Print the rest of the sections
        for (mf, mt, sec, mod_seq) in self.into_iter().flat_map(|(mf, file)| {
            file.into_iter()
                .map(|(mt, sec)| (*mf, *mt, sec, sec.get_mod_seq()))
        }) {
            let records = sec.len();
            writeln!(
                stream,
                "{:<66}{:>4}{:>2}{:>3}{:>5}",
                DirRecord {
                    l1: mf,
                    l2: mt,
                    n1: records as i32,
                    n2: mod_seq as i32
                }
                .write(),
                mat.number,
                1,
                451,
                count
            )?;
            count += 1;
        }

        // Finish with a SEND record
        writeln!(stream, "{:>66}{:>4}{:>2}{:>3}99999", "", mat.number, 1, 0)?;

        // Print FEND if necessary as well
        if self.get(1).is_none() {
            writeln!(stream, "{:>66}{:>4}{:>2}{:>3}    0", "", mat.number, 0, 0)?;
        }

        // Print all files
        for (mf, file) in self {
            file.print(stream, mat, *mf)?;
        }
        // Finish up with MEND record
        writeln!(stream, "{:>66}{:>4}{:>2}{:>3}    0", "", 0, 0, 0)?;
        Ok(())
    }
}

///
/// Allow Material to be build from a Map
///
impl TryFrom<BTreeMap<MF, File>> for Material {
    type Error = EndfError;

    fn try_from(value: BTreeMap<MF, File>) -> Result<Self, Self::Error> {
        let mut v_in = value;
        let (header, map) = MaterialHeader::from_section(&mut v_in[&1][451].iter())?;

        // Verify that all entries are present and of correct length
        for (mf, mt, sec) in &mut v_in
            .iter_mut()
            .flat_map(|(mf, file)| file.into_iter().map(|(mt, sec)| (*mf, *mt, sec)))
        {
            let entry = map.get(&(mf, mt)).ok_or(FormatError::misc(&format!(
                "Failed to find section in the file: ({},{}) : in the directory from header {:#?}",
                mf, mt, map
            )))?;
            sec.set_mod_seq(entry.1 as u32);

            // We need to disable this check as meaning of NC is a bit inconsistent
            // It can either be a number of records or lines in a section
            // here we use number of lines as it seems to make more sense.

            // if entry.0 != sec.len() as i32 {
            //     // Error message placeholder
            //     Err::<(), String>(format!(
            //         "For MF={mf} MT={mt}, section is declared to contain {} records, but {} were read!",
            //         entry.0,
            //         sec.len()
            //     ))
            //     .unwrap();
            // }
        }

        v_in.get_mut(&1).unwrap().remove(451);
        Ok(Self {
            header,
            contents: v_in,
        })
    }
}

///
/// Represents a file with nuclear data and contains multiple Materials
///
#[derive(Debug, Default)]
pub struct Tape {
    contents: BTreeMap<MAT, Material>,
}

impl Tape {
    ///
    /// Create new empty Tape
    ///
    pub fn new() -> Self {
        Self {
            contents: BTreeMap::new(),
        }
    }

    ///
    /// Get a Material from a tape
    ///
    /// # Arguments
    /// * 'mat' -> ENDF MAT identifier of the element or isotope
    ///
    pub fn get(&self, mat: MAT) -> Option<&Material> {
        self.contents.get(&mat)
    }

    ///
    /// Get a mutable Material from a tape
    ///
    /// # Arguments
    /// * 'mat' -> ENDF MAT identifier of the element or isotope
    ///
    pub fn get_mut(&mut self, mat: MAT) -> Option<&mut Material> {
        self.contents.get_mut(&mat)
    }

    ///
    /// Insert [MAT], [Material] pair
    ///
    /// If the Tape did not have `mat`, `None` is returned.
    /// If the Tape did have this `mat`, the value is updated and old value
    /// is returned.
    ///
    pub fn insert(&mut self, mat: MAT, value: Material) -> Option<Material> {
        self.contents.insert(mat, value)
    }

    ///
    /// Read an ENDF tape from a buffered reader
    ///
    /// This private function is required so unit tests of reading can be
    /// performed without a need of having a 'test file' on the filesystem
    ///
    /// #  Arguments
    /// * 'file' - An instance which implements io::BufRead trait, which allows
    ///            to obtain an iterator over the lines
    ///
    fn from_reader(file: &mut dyn io::BufRead) -> Result<Self, EndfError> {
        // When reading a tape we need to work with maps
        type PseudoTape = BTreeMap<MAT, BTreeMap<MF, BTreeMap<MT, Section>>>;
        type PseudoMaterial = BTreeMap<MF, BTreeMap<MT, Section>>;
        type PseudoFile = BTreeMap<MT, Section>;

        let mut tape: PseudoTape = PseudoTape::default();

        // We need to skip a first line as it often is not well formatted
        for line in file.lines().skip(1) {
            let l = line?;

            // Check line length and encoding
            if l.len() < 75 {
                return Err(FormatError::new(
                    FormatErrorKind::WrongLineLength {
                        expected: 75,
                        length: l.len(),
                    },
                    &l,
                )
                .into());
            } else if !l.is_ascii() {
                return Err(FormatError::new(FormatErrorKind::NonASCII, &l).into());
            }

            // Try to read control (MAT, MF, MT) values and append line
            // to the correct Material, File and Section
            match Control::read(&l) {
                Ok(ctrl) => {
                    if ctrl.is_valid() {
                        let material = tape
                            .entry(ctrl.mat.try_into()?)
                            .or_insert(PseudoMaterial::default());
                        let file = material.entry(ctrl.mf).or_insert(PseudoFile::default());
                        let section = file.entry(ctrl.mt).or_insert(Section::default());
                        section.append(&l[0..66]);
                    }
                }
                Err(_) => return Err(FormatError::new(FormatErrorKind::InvalidControl, &l).into()),
            }
        }

        let mut contents = BTreeMap::<MAT, Material>::default();

        for (mat, pseudo_mat) in tape.into_iter() {
            let mut temp = BTreeMap::<MF, File>::default();
            for (mf, pseudo_file) in pseudo_mat.into_iter() {
                temp.insert(mf, File::from(pseudo_file));
            }
            contents.insert(mat, Material::try_from(temp)?);
        }

        Ok(Self { contents })
    }

    ///
    /// Read a text file containing an ENDF tape
    ///
    /// # Arguments
    /// * 'filename' - String view indicating a path to the file
    ///
    pub fn from_file(filename: &str) -> Result<Tape, EndfError> {
        let file = std::fs::File::open(filename)?;

        Self::from_reader(&mut std::io::BufReader::new(file))
    }

    ///
    /// Write ENDF tape
    ///
    /// # Arguments
    ///
    /// * `stream` -> Stream to which output will be written
    /// * `tpid` -> String to be printed in the first line of ENDF file. It is TPID record without
    ///             specified format in ENDF Manual. Maximum of 80 characters.
    ///
    fn print(&self, stream: &mut impl Write, tpid: &str) -> Result<(), EndfError> {
        // Start with TPID record
        writeln!(stream, "{:>80}", tpid)?;

        for (mat, material) in self {
            material.print(stream, *mat)?;
        }

        // Finish up with TEND record;
        writeln!(stream, "{:>66}{:>4}{:>2}{:>3}    0", "", -1, 0, 0)?;
        Ok(())
    }

    ///
    /// Print the contents of the tape to the file
    ///
    /// # Arguments
    /// * 'filename' - String view indicating a path to the file
    /// * `tpid`  - String to be printed in the first line of ENDF file. It is TPID record without
    ///             specified format in ENDF Manual. Maximum of 80 characters.
    ///
    pub fn to_file(&self, filename: &str, tpid: &str) -> Result<(), EndfError> {
        let file = std::fs::File::create(filename)?;
        self.print(&mut std::io::BufWriter::new(file), tpid)
    }

    ///
    /// Print the contents of the tape to a writer
    ///
    /// # Arguments
    /// * 'writer'- Any kind of writer
    /// * `tpid`  - String to be printed in the first line of ENDF file. It is TPID record without
    ///             specified format in ENDF Manual. Maximum of 80 characters.
    ///
    pub fn to_writer(&self, writer: &mut impl std::io::Write, tpid: &str) -> Result<(), EndfError> {
        self.print(writer, tpid)
    }
}

///
/// Implement index access
///
impl Index<MAT> for Tape {
    type Output = Material;
    fn index(&self, index: MAT) -> &Self::Output {
        self.get(index).unwrap()
    }
}

///
/// Implement conversion of a tape into iterator over references
///
impl<'a> IntoIterator for &'a Tape {
    type Item = (&'a MAT, &'a Material);
    type IntoIter = std::collections::btree_map::Iter<'a, MAT, Material>;

    fn into_iter(self) -> Self::IntoIter {
        self.contents.iter()
    }
}

///
/// Implement conversion into consuming iterator as well
///
impl IntoIterator for Tape {
    type Item = (MAT, Material);
    type IntoIter = std::collections::btree_map::IntoIter<MAT, Material>;

    fn into_iter(self) -> Self::IntoIter {
        self.contents.into_iter()
    }
}

//
// Extend one type by another
//
impl Extend<(MAT, Material)> for Tape {
    fn extend<T: IntoIterator<Item = (MAT, Material)>>(&mut self, iter: T) {
        self.contents.extend(iter);
    }
}

//
// Create a new tape from an iterator
//
impl FromIterator<(MAT, Material)> for Tape {
    fn from_iter<T: IntoIterator<Item = (MAT, Material)>>(iter: T) -> Self {
        Self {
            contents: BTreeMap::<MAT, Material>::from_iter(iter),
        }
    }
}

///
/// Parsed contents of MF=1 MT=451 which contain data about library and evaluation
///
#[derive(Debug)]
pub struct MaterialHeader {
    library: LibraryInfo,
    evaluation: EvaluationInfo,
    data: NuclideInfo,
    part: IncidentParticle,
    kind: DataType,
    comment: String,
}

///
/// Map with the contents of the Material Header (MT=451) Directory part
///
/// For each MF and MT present in the file it stores:
///  - number of lines in each section (without SEND record)
///  - value of the modification sequence for the section
///
type HeaderDirectory = BTreeMap<(MF, MT), (i32, i32)>;

///
/// Private struct to group together stings describing Author, Reference and
/// relevant dates in the MT=451 Section
///
struct HeaderAuthDateRef<'a> {
    pub labratory: &'a str,
    pub author: &'a str,
    pub reference: &'a str,
    pub evaluation_date: &'a str,
    pub distribution_date: &'a str,
    pub revision_date: &'a str,
    pub master_date: &'a str,
}

impl MaterialHeader {
    fn parse_description<'a>(
        iter: &mut impl std::iter::Iterator<Item = &'a str>,
    ) -> Result<HeaderAuthDateRef<'a>, EndfError> {
        let line1 = get_text(iter).ok_or(EndfError::EndOfSection)?;
        let line2 = get_text(iter).ok_or(EndfError::EndOfSection)?;

        // Get fields
        let labratory = &line1[11..22];
        let evaluation_date = &line1[22..33];
        let author = &line1[33..66];

        let reference = &line2[1..22];
        let distribution_date = &line2[22..33];
        let revision_date = &line2[33..44];
        let master_date = &line2[44..66];
        Ok(HeaderAuthDateRef::<'a> {
            labratory,
            author,
            reference,
            evaluation_date,
            distribution_date,
            revision_date,
            master_date,
        })
    }

    ///
    /// Performs conversion from ENDF formatted date to NaiveDate
    ///
    /// Since ENDF does not specify a day, we assume it to be 1st.
    ///
    /// e.g. "NOV77" -> "1-11-1977"
    ///
    fn parse_date(date_str: &str) -> Result<Option<chrono::NaiveDate>, FormatError> {
        if date_str.is_empty() {
            return Ok(None);
        }
        let temp_str = "1".to_string() + date_str.trim();
        Ok(Some(
            chrono::NaiveDate::parse_from_str(&temp_str, "%d%b%y")
                .map_err(|_| FormatError::new(FormatErrorKind::InvalidDateFormat, date_str))?,
        ))
    }

    ///
    /// Parse directory entry of the MF=1 MT=451 section
    ///
    fn parse_directory<'a>(
        iter: &mut impl std::iter::Iterator<Item = &'a str>,
        nxc: u32,
    ) -> Result<HeaderDirectory, EndfError> {
        let mut map = BTreeMap::<(MF, MT), (i32, i32)>::default();

        let mut count = 0;
        for line in iter.take(nxc as usize) {
            let rec: DirRecord = line.parse()?;
            map.insert((rec.l1, rec.l2), (rec.n1, rec.n2));
            count += 1;
        }
        // Check that there the iterator did not end before all elements were taken
        if count != nxc {
            return Err(FormatError::misc(&format!(
                "Reached the end of directory before reading all {nxc} entries"
            ))
            .into());
        }

        Ok(map)
    }

    ///
    /// Build header by parsing the contents of MF=1 MT=451 section
    ///
    pub fn from_section<'a>(
        iter: &mut impl std::iter::Iterator<Item = &'a str>,
    ) -> Result<(Self, HeaderDirectory), EndfError> {
        // Read the crazy & confusing zoo of ENDF numbers and flags

        // Line 1
        let ContRecord {
            c1: za,
            c2: awr,
            l1: lrp,
            l2: lfi,
            n1: nlib,
            n2: nmod,
        } = get_head(iter)?;

        // Line 2
        let ContRecord {
            c1: elis,
            c2: sta,
            l1: lis,
            l2: liso,
            n2: _nfor, // todo: Check that it is 6
            ..
        } = get_head(iter)?;

        // Line 3
        let ContRecord {
            c1: awi,
            c2: emax,
            l1: lrel,
            n1: nsub,
            n2: nver,
            ..
        } = get_head(iter)?;

        // Line 4
        let ContRecord {
            c1: temp,
            l1: ldvr,
            n1: nwd,
            n2: nxc,
            ..
        } = get_head(iter)?;

        // Parse the description entries
        let text_lines = Self::parse_description(iter)?;

        // Load the comment lines
        let comment = iter
            .take((nwd - 2).try_into().unwrap())
            .fold(String::new(), |acc, el| acc + el);

        // Parse the directory
        let map = Self::parse_directory(iter, nxc as u32)?;

        // Verify that end of the file is reached
        if iter.next().is_some() {
            return Err(
                FormatError::misc("Did not reach end of header. There are extra entries").into(),
            );
        }

        // Build Nuclide data
        let atomic_number = (za as u32) / 1000;
        let mass_number = (za as u32) % 1000;
        let data = NuclideInfo::new(
            atomic_number,
            mass_number,
            awr,
            ResonanceInfo::from_flag(lrp)?,
            lfi == 1,
            elis,
            sta == 0.0,
            lis.try_into().unwrap(),
            liso.try_into().unwrap(),
        )?;

        // Build library data
        let library = LibraryInfo::new(nlib, nver, lrel)?;

        // Get particle and data type
        let (part, kind) = metadata::parse_sublibrary_number(nsub, awi)?;

        // Convert dates
        // We use the fact that in ALL cases prefix 'DIST-', 'REV1-', 'EVAL-'
        // uses 5 characters
        let eval_date = Self::parse_date(text_lines.evaluation_date[5..11].trim());
        let dist_date = Self::parse_date(text_lines.distribution_date[5..11].trim());
        let rev_date = Self::parse_date(text_lines.revision_date[5..11].trim());
        let master_date = if text_lines.master_date.trim().is_empty() {
            None
        } else {
            Some(
                chrono::NaiveDate::parse_from_str(text_lines.master_date.trim(), "%Y%m%d")
                    .map_err(|_| {
                        FormatError::misc(&format!(
                            "Master file date must follow YYYYMMDD format: '{}'",
                            text_lines.master_date
                        ))
                    })?,
            )
        };

        let evaluation = EvaluationInfo::new(
            nmod as u32,
            ldvr,
            text_lines.labratory.trim(),
            text_lines.author.trim(),
            text_lines.reference.trim(),
            temp,
            emax,
            (eval_date?.unwrap(), dist_date?, rev_date?, master_date),
        )?;

        Ok((
            Self {
                library,
                evaluation,
                data,
                part,
                kind,
                comment,
            },
            map,
        ))
    }

    ///
    /// Write the contents of the header to a file
    ///
    ///
    fn print(
        &self,
        stream: &mut impl std::io::Write,
        mat: MAT,
        nxc: i32,
    ) -> Result<u32, EndfError> {
        // Create first numerical records
        let zaid = (self.data.get_atomic_number() * 1000 + self.data.get_mass_number()) as f64;
        let line1 = ContRecord {
            c1: zaid,
            c2: *self.data.get_atomic_weight_ratio(),
            l1: self.data.get_lrp().to_flag(),
            l2: self.data.is_fissile() as i32,
            n1: *self.library.get_nlib(),
            n2: *self.evaluation.get_mod_seq() as i32,
        };

        let (lis, liso) = self.data.get_state_numbers();
        let line2 = ContRecord {
            c1: *self.data.get_excitation_energy(),
            c2: !self.data.is_stable() as i32 as f64, // Yeah... ENDF...
            l1: lis as i32,
            l2: liso as i32,
            n1: 0,
            n2: 6,
        };

        let (nver, lrel) = self.library.get_version();
        let nsub = metadata::sublibrary_number(&self.part, &self.kind);
        let line3 = ContRecord {
            c1: self.part.get_weight(),
            c2: *self.evaluation.get_max_energy(),
            l1: *lrel,
            l2: 0,
            n1: nsub,
            n2: *nver,
        };

        let line4 = ContRecord {
            c1: *self.evaluation.get_temperature(),
            c2: 0.0,
            l1: *self.evaluation.get_ldvr(),
            l2: 0,
            n1: (self.comment.len() / 66) as i32 + 2,
            n2: nxc,
        };
        // Print numerical lines
        let mut count: u32 = 1;
        for r in [line1, line2, line3, line4] {
            writeln!(
                stream,
                "{}{:>4}{:>2}{:>3}{:>5}",
                r.write(),
                mat.number,
                1,
                451,
                count
            )?;
            count += 1;
        }

        // Print string lines
        // 1st line
        let atomic_number = *self.data.get_atomic_number();
        let mass_number = *self.data.get_mass_number();
        let zsymm = format!(
            "{:>3}-{:<2}-{:>3}",
            atomic_number,
            metadata::element_symbol(atomic_number as usize)?,
            mass_number
        );
        let edate = format!(
            "EVAL-{}",
            self.evaluation.get_evaluation_date().format("%h%y")
        );
        writeln!(
            stream,
            "{:<10} {:<11}{:<11}{:<33}{:>4}{:>2}{:>3}{:>5}",
            zsymm,
            self.evaluation.get_labratory(),
            edate,
            self.evaluation.get_author(),
            mat.number,
            1,
            451,
            count
        )?;
        count += 1;

        // 2nd line
        let ddate = match self.evaluation.get_distribution_date() {
            Some(date) => format!("DIST-{}", date.format("%h%y")),
            None => String::new(),
        };
        // Note that ENDF is again a bit broken since number of revisions may
        // be grater than 9, but only one character is allowed when printing the
        // date. This is why we need to take 'nmod mod 10'
        let rdate = match self.evaluation.get_revision_date() {
            Some(date) => format!(
                "REV{:>1}-{}",
                self.evaluation.get_mod_seq() % 10,
                date.format("%h%y")
            ),
            None => String::new(),
        };
        let endate = match self.evaluation.get_mater_file_date() {
            Some(date) => date.format("%Y%m%d").to_string(),
            None => String::new(),
        };
        writeln!(
            stream,
            " {:<21}{:<11}{:<11}{:>22}{:>4}{:>2}{:>3}{:>5}",
            self.evaluation.get_reference(),
            ddate,
            rdate,
            endate,
            mat.number,
            1,
            451,
            count
        )?;
        count += 1;

        // Now finally we can print the comment
        for line in self
            .comment
            .as_bytes()
            .chunks(66)
            .map(|x| std::str::from_utf8(x).unwrap())
        {
            writeln!(
                stream,
                "{:<66}{:>4}{:>2}{:>3}{:>5}",
                line, mat.number, 1, 451, count
            )?;
            count += 1;
        }

        Ok(count)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_control() {
        let line =
            "ENDF-6 Format. Translated from the Livermore ENDL format          8400 1451   12";
        let ctrl = Control::read(line).unwrap();
        assert_eq!(ctrl.mat, 8400);
        assert_eq!(ctrl.mf, 1);
        assert_eq!(ctrl.mt, 451);
        assert!(ctrl.is_valid());
    }

    #[test]
    fn test_get_list() {
        // Note we need to input the file in a weird syntax to make sure trailing spaces are not removed
        let file = " 7.70000000 1.3                 8          9         14          2\n\
 1.0        2.0        3.0        4.0        5.0        6.0       \n\
 7.0        8.0        9.0        10.0       11.0       12.0      \n\
 13.0       14.0                                                  \n\
";
        let mut iter = file.lines();
        let list = get_list(&mut iter).unwrap();

        // Verify Header entries
        assert_eq!(list.c1, 7.7);
        assert_eq!(list.c2, 1.3);
        assert_eq!(list.l1, 8);
        assert_eq!(list.l2, 9);
        assert_eq!(list.n2, 2);

        // Verify List
        println!("{:?}", list.list);
        itertools::assert_equal(list.list, (1..15).map(|x| x as f64));
    }

    #[test]
    fn tes_get_list_wrong_numbers() {
        let file = vec![
            " 7.70000000 1.3                 8          9         14          2",
            " 1.0        2.0        3.0        4.0        5.0        6.0       ",
            " 7.0        8.0        9.0        10.0       NotAFloat  12.0      ",
            " 13.0       14.0                                                  ",
        ];
        let mut iter = file.into_iter();
        assert!(get_list(&mut iter).is_err());
    }

    #[test]
    fn test_get_table() {
        // Note we need to input the file in a weird syntax to make sure trailing spaces are not removed
        let file = " 1.3        1.1                 7          3          2          4\n\
          2          1           4          2                     \n\
 1.00000000 2.00000000 2.00000000 1.00000000 3.00000000 2.00000000\n\
 4.00000000 1.00000000                                            \n";

        let mut iter = file.lines();
        let table = get_tab1(&mut iter).unwrap();

        assert_eq!(table.c1, 1.3);
        assert_eq!(table.c2, 1.1);
        assert_eq!(table.l1, 7);
        assert_eq!(table.l2, 3);

        // Table verification
        itertools::assert_equal(table.table.regions, vec![1, 3]);
        itertools::assert_equal(
            table.table.interpolation,
            vec![InterpolationType::Histogram, InterpolationType::LinLin],
        );
        itertools::assert_equal(table.table.x, (1..5).map(|x| x as f64));
        itertools::assert_equal(table.table.y, vec![2.0, 1.0, 2.0, 1.0]);
    }

    #[test]
    fn test_get_table_invalid_floats() {
        let file = vec![
            " 1.3        1.1                 7          3          2          4",
            "          2          1          4          2                     ",
            " 1.00000000 2.00000000NOT A FLOAT 1.00000000 3.00000000 2.00000000",
            " 4.00000000 1.00000000                                            ",
        ];

        let mut iter = file.into_iter();
        assert!(get_tab1(&mut iter).is_err());
    }

    #[test]
    fn test_get_table_invalid_num_of_points() {
        let file = vec![
            " 1.3        1.1                 7          3          2         -4",
            "          2          1          4          2                     ",
            " 1.00000000 2.00000000 2.00000000 1.00000000 3.00000000 2.00000000",
            " 4.00000000 1.00000000                                            ",
        ];

        let mut iter = file.into_iter();
        assert!(get_tab1(&mut iter).is_err());
    }

    #[test]
    fn test_get_table2() {
        // Note we need to input the file in a weird syntax to make sure trailing spaces are not removed
        let file = " 1.3        1.1                 7          3          2         98\n\
          12          2         98          5                      \n";

        let mut iter = file.lines();
        let table2 = get_tab2(&mut iter).unwrap();

        assert_eq!(table2.c1, 1.3);
        assert_eq!(table2.c2, 1.1);
        assert_eq!(table2.l1, 7);
        assert_eq!(table2.l2, 3);

        // Interpolation adn table parameters
        assert_eq!(table2.nz, 98);
        itertools::assert_equal(
            table2.interpolation,
            vec![InterpolationType::LinLin, InterpolationType::LogLog],
        );
        itertools::assert_equal(table2.regions, vec![11, 97]);
    }

    #[test]
    fn test_get_table2_invalid_numbers() {
        // Invalid integer
        let file1 = vec![
            " 1.3        1.1                 7          3          2         98",
            "     notInt          2         98          5                      ",
        ];
        let mut iter = file1.into_iter();
        assert!(get_tab2(&mut iter).is_err());

        // Invalid flag value
        let file2 = vec![
            " 1.3        1.1                 7          3          2         98",
            "         12          2         98         -3                      ",
        ];
        let mut iter = file2.into_iter();
        assert!(get_tab2(&mut iter).is_err());
    }

    #[test]
    fn test_header_parsing() {
        // Minimal header section
        let lines = vec![
            " 1.000000+3 9.992420-1         -1          0          0          3",
            "        0.0        0.0          0          0          0          6",
            "        0.0 1.00000+11          0          0          3          6",
            "        0.0        0.0          1          0          5          4",
            "  1-H -  0 Contin     EVAL-Dec07 John Wick                        ",
            " GLOCK19              DIST-Dec97                          19870412",
            "---- ENDF/B-VI        MATERIAL  100                               ",
            "----- PHOTO-ATOMIC INTERACTION DATA                               ",
            "------ ENDF/B-6                                                   ",
            "                                1        451        164          3",
            "                               23        501        677          3",
            "                               23        502        124          3",
            "                               23        504        135          3",
        ];
        let mut iter = lines.clone().into_iter();
        let (header, dict) = MaterialHeader::from_section(&mut iter).unwrap();

        // Verify dictionary contents
        assert_eq!(dict[&(1, 451)], (164, 3));
        assert_eq!(dict[&(23, 501)], (677, 3));
        assert_eq!(dict[&(23, 502)], (124, 3));
        assert_eq!(dict[&(23, 504)], (135, 3));

        // Verify header contents
        // Library info
        assert_eq!(header.library.get_name(), "ENDF/B");
        assert_eq!(header.library.get_version(), (&6, &0));

        // Physical info
        assert_eq!(*header.data.get_atomic_number(), 1);
        assert_eq!(*header.data.get_mass_number(), 0);
        assert_eq!(*header.data.get_atomic_weight_ratio(), 0.999242);
        assert_eq!(header.data.get_state_numbers(), (0, 0));
        assert_eq!(*header.data.get_excitation_energy(), 0.0);
        assert_eq!(*header.data.get_lrp(), metadata::ResonanceInfo::NoResonance);
        assert!(!header.data.is_fissile());
        assert!(header.data.is_stable());

        // Evaluation info
        assert_eq!(*header.evaluation.get_mod_seq(), 3);
        assert_eq!(*header.evaluation.get_ldvr(), 1);
        assert_eq!(header.evaluation.get_labratory(), "Contin");
        assert_eq!(header.evaluation.get_author(), "John Wick");
        assert_eq!(header.evaluation.get_reference(), "GLOCK19");
        assert_eq!(*header.evaluation.get_temperature(), 0.0);

        assert_eq!(
            *header.evaluation.get_evaluation_date(),
            chrono::NaiveDate::from_ymd_opt(2007, 12, 1).unwrap()
        );
        assert_eq!(
            *header.evaluation.get_distribution_date(),
            chrono::NaiveDate::from_ymd_opt(1997, 12, 1)
        );

        assert_eq!(*header.evaluation.get_revision_date(), None);
        assert_eq!(
            *header.evaluation.get_mater_file_date(),
            chrono::NaiveDate::from_ymd_opt(1987, 04, 12)
        );

        // Incident particle
        assert_eq!(header.part, IncidentParticle::Photon);

        // Print back to a string and verify against input
        // Note that it does not print the Dict entry!
        let mut file = Vec::new();
        header.print(&mut file, 100.try_into().unwrap(), 4).unwrap();

        // Print the reference
        let mut reference = Vec::new();
        for (i, line) in lines.iter().take(9).enumerate() {
            writeln!(reference, "{:<66} 100 1451{:>5}", line, i + 1).unwrap();
        }

        let file = std::str::from_utf8(&file).unwrap();
        let reference = std::str::from_utf8(&reference).unwrap();

        for (l1, l2) in std::iter::zip(file.lines(), reference.lines()) {
            assert_eq!(l1, l2)
        }
    }

    #[test]
    fn test_header_parsing_with_revision() {
        // Minimal header section
        let lines = vec![
            " 1.000000+3 9.992420-1         -1          0          0          3",
            "        0.0        0.0          0          0          0          6",
            "        0.0 1.00000+11          0          0          3          6",
            "        0.0        0.0          1          0          5          4",
            "  1-H -  0 Contin     EVAL-Dec07 John Wick                        ",
            " GLOCK19              DIST-Dec97 REV3-Apr13               19870412",
            "---- ENDF/B-VI        MATERIAL  100                               ",
            "----- PHOTO-ATOMIC INTERACTION DATA                               ",
            "------ ENDF/B-6                                                   ",
            "                                1        451        164          3",
            "                               23        501        677          3",
            "                               23        502        124          3",
            "                               23        504        135          3",
        ];
        let mut iter = lines.clone().into_iter();
        let (header, _) = MaterialHeader::from_section(&mut iter).unwrap();

        // Check revision data
        assert_eq!(
            *header.evaluation.get_revision_date(),
            chrono::NaiveDate::from_ymd_opt(2013, 04, 1)
        );

        // Print back to a string and verify against input
        // Note that it does not print the Dict entry!
        let mut file = Vec::new();
        header.print(&mut file, 100.try_into().unwrap(), 4).unwrap();

        // Print the reference
        let mut reference = Vec::new();
        for (i, line) in lines.iter().take(9).enumerate() {
            writeln!(reference, "{:<66} 100 1451{:>5}", line, i + 1).unwrap();
        }

        let file = std::str::from_utf8(&file).unwrap();
        let reference = std::str::from_utf8(&reference).unwrap();

        for (l1, l2) in std::iter::zip(file.lines(), reference.lines()) {
            assert_eq!(l1, l2)
        }
    }

    #[test]
    fn test_mat_number() {
        // Normal conversion from number
        let mat: MAT = 125.try_into().unwrap();
        assert_eq!(mat, MAT { number: 125 });

        assert_eq!("125 [H-1]", mat.to_string());

        // Failed conversions from number
        assert!(MAT::try_from(-125).is_err());
        assert!(MAT::try_from(10098).is_err());

        // Normal conversion from string
        let mat: MAT = "H-1".parse().unwrap();
        assert_eq!(mat, MAT { number: 125 });
        assert_eq!(mat.name(), "H-1");

        // Failed conversions from string
        assert!(MAT::from_str("hhh").is_err());
    }

    #[test]
    fn test_new_material_error() {
        let lib: LibraryInfo = "JEFF 3.1".parse().unwrap();
        let data = NuclideInfo::new(
            1,
            2,
            2.0,
            ResonanceInfo::NoResonance,
            false,
            0.0,
            true,
            0,
            0,
        )
        .unwrap();
        let eval = EvaluationInfo::new(
            0,
            0,
            "Best lab",
            "Me",
            "None",
            0.0,
            2.0e7,
            ("1997-03-2".parse().unwrap(), None, None, None),
        )
        .unwrap();

        // Return error if comment contains non-ASCII characters
        assert!(Material::new(
            128.try_into().unwrap(),
            lib,
            data,
            eval,
            IncidentParticle::Ion(4.0, 2, 4),
            DataType::NuclearInteractionData,
            "αβ"
        )
        .is_err());
    }
}
