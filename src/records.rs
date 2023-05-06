//! Contains structures that represent various ENDF records
//!
//! Record is a basic data package used in ENDF-6 format. Number of records
//! consist of a single line only ([ContRecord],[DirRecord]), other can span
//! multiple lines ([TableRecord], [ListRecord], [Table2Record]).
//!
//! Note that we have no separate structures for HEAD record as it is identical
//! to the CONT record.
//!
//! Presently no structs exist for any of the END records (MEND, FEND, SEND, TEND)
//! since they contain no data.
//!
use itertools::Itertools;

use crate::conversions::{print_endf_float, to_f64, to_i32};
use crate::data_structures::{InterpolationType, Table};
use crate::error::{EndfError, FormatError, FormatErrorKind, ParseError};

///
/// Trait to group together all types of ENDF records
///
/// Allows generic printing of records to strings in polymorphic fashion.
///
pub trait Record {
    ///
    /// Print an ENDF record to string
    ///
    fn stringify(&self) -> String;

    ///
    /// Returns the record as a string making sure that it contains valid ENDF lines
    ///
    /// Each line in ENDF file is made up of 66 characters. String length must be
    /// a multiple of 66.
    ///
    /// # Panics
    /// If string returned would not have a length which is a multiple of 66.
    /// Or if the string contains non-ASCII characters
    ///
    fn write(&self) -> String {
        let out = self.stringify();
        if out.len() % 66 != 0 {
            panic!(
                "Record did not print full 66 character ENDF lines: '{}'!",
                out
            );
        } else if !out.is_ascii() {
            panic!("Non ASCII characters in: '{}'", out);
        }
        out
    }
}

///
/// CONT (Control) ENDF Record
///
/// Contains 6 fields, 2 floats and 4 integers. The actual meaning of parameters
/// is context dependent.
///
/// Is also used to represent HEAD record.
///
#[derive(Debug)]
pub struct ContRecord {
    pub c1: f64,
    pub c2: f64,
    pub l1: i32,
    pub l2: i32,
    pub n1: i32,
    pub n2: i32,
}

impl ContRecord {
    pub fn from_string(line: &str) -> Result<Self, EndfError> {
        if line.len() != 66 {
            Err(FormatError::new(
                FormatErrorKind::WrongLineLength {
                    expected: 66,
                    length: line.len(),
                },
                line,
            )
            .into())
        } else {
            // Read each entry and change the error type if conversion has failed
            Ok(Self {
                c1: to_f64(&line[0..11])?,
                c2: to_f64(&line[11..22])?,
                l1: to_i32(&line[22..33])?,
                l2: to_i32(&line[33..44])?,
                n1: to_i32(&line[44..55])?,
                n2: to_i32(&line[55..66])?,
            })
        }
    }
}

impl Record for ContRecord {
    fn stringify(&self) -> String {
        format!(
            "{:>11}{:>11}{:>11}{:>11}{:>11}{:>11}",
            print_endf_float(self.c1),
            print_endf_float(self.c2),
            self.l1,
            self.l2,
            self.n1,
            self.n2
        )
    }
}

///
/// DIR (Directory) record
///
/// Contains 4 integer fields only.
/// Like [ContRecord], but float fields are blank
///
pub struct DirRecord {
    pub l1: i32,
    pub l2: i32,
    pub n1: i32,
    pub n2: i32,
}

impl std::str::FromStr for DirRecord {
    type Err = EndfError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() != 66 {
            Err(FormatError::new(
                FormatErrorKind::WrongLineLength {
                    expected: 66,
                    length: s.len(),
                },
                s,
            )
            .into())
        } else {
            Ok(Self {
                l1: to_i32(&s[22..33])?,
                l2: to_i32(&s[33..44])?,
                n1: to_i32(&s[44..55])?,
                n2: to_i32(&s[55..66])?,
            })
        }
    }
}

impl Record for DirRecord {
    fn stringify(&self) -> String {
        format!(
            "{:>11}{:>11}{:>11}{:>11}{:>11}{:>11}",
            "", "", self.l1, self.l2, self.n1, self.n2
        )
    }
}

///
/// List ENDF record
///
/// Contains C1, C2, L1, L2 and N2 numerical fields from the header
/// and a list of floats
///
pub struct ListRecord {
    pub c1: f64,
    pub c2: f64,
    pub l1: i32,
    pub l2: i32,
    pub n2: i32,
    pub list: Vec<f64>,
}

impl ListRecord {
    pub fn new(c1: f64, c2: f64, l1: i32, l2: i32, n2: i32, list: Vec<f64>) -> Self {
        Self {
            c1,
            c2,
            l1,
            l2,
            n2,
            list,
        }
    }
}

impl Record for ListRecord {
    fn stringify(&self) -> String {
        let size = self.list.len() as i32;
        let mut out = ContRecord {
            c1: self.c1,
            c2: self.c2,
            l1: self.l1,
            l2: self.l2,
            n1: size,
            n2: self.n2,
        }
        .stringify();

        // Print the list
        out.push_str(
            &self
                .list
                .iter()
                .fold::<String, _>(String::new(), |mut acc, e| {
                    acc.push_str(&print_endf_float(*e));
                    acc
                }),
        );

        // Append so the list line is composed of 66 characters
        let remainder = size % 6;
        if remainder != 0 {
            for _ in 0..6 - remainder {
                out.push_str(&" ".repeat(11));
            }
        }
        out
    }
}

///
/// TAB1 ENDF record
///
/// Contains C1, C2, L1 and L2 numerical field in addition to [`ENDF Table`][Table]
///
pub struct TableRecord {
    pub c1: f64,
    pub c2: f64,
    pub l1: i32,
    pub l2: i32,
    pub table: Table,
}

impl Record for TableRecord {
    fn stringify(&self) -> String {
        let table = &self.table;
        let size = table.x.len() as i32;
        let inter_size = table.regions.len() as i32;

        // Print header
        let cont = ContRecord {
            c1: self.c1,
            c2: self.c2,
            l1: self.l1,
            l2: self.l2,
            n1: inter_size,
            n2: size,
        };
        let mut out = cont.write();

        // Follow by interpolation regions
        out.push_str(
            &table
                .regions
                .iter()
                .map(|x| (*x + 1) as i32) // We need to bring region boundaries back to 1-indexing
                .interleave(table.interpolation.iter().map(|x| x.to_flag()))
                .fold(String::new(), |mut acc, r| {
                    acc.push_str(&format!("{r:>11}"));
                    acc
                }),
        );
        // Pad remaining whitespace
        let remainder = 2 * inter_size % 6;
        if remainder != 0 {
            for _ in 0..6 - remainder {
                out.push_str(&" ".repeat(11));
            }
        }

        // Print the values in the table
        out.push_str(&table.x.iter().interleave(table.y.iter()).fold(
            String::new(),
            |mut acc, n| {
                acc.push_str(&print_endf_float(*n));
                acc
            },
        ));

        // Pad remaining whitespace
        let remainder = 2 * size % 6;
        if remainder != 0 {
            for _ in 0..6 - remainder {
                out.push_str(&" ".repeat(11));
            }
        }

        out
    }
}

///
/// TAB2 ENDF record
///
/// Is used to specify interpolation regions of 2D data.
///
pub struct Table2Record {
    pub c1: f64,
    pub c2: f64,
    pub l1: i32,
    pub l2: i32,
    pub nz: usize,
    pub interpolation: Vec<InterpolationType>,
    pub regions: Vec<usize>,
}

impl Record for Table2Record {
    fn stringify(&self) -> String {
        let size = self.regions.len() as i32;

        // Print header
        let mut out = ContRecord {
            c1: self.c1,
            c2: self.c2,
            l1: self.l1,
            l2: self.l2,
            n1: size,
            n2: self.nz as i32,
        }
        .write();

        // Print the interpolation regions
        out.push_str(
            &self
                .regions
                .iter()
                .map(|x| (*x + 1) as i32) // We need to bring region boundaries back to 1-indexing
                .interleave(self.interpolation.iter().map(|x| x.to_flag()))
                .fold(String::new(), |mut acc, r| {
                    acc.push_str(&format!("{r:>11}"));
                    acc
                }),
        );
        // Pad remaining whitespace
        let remainder = 2 * size % 6;
        if remainder != 0 {
            for _ in 0..6 - remainder {
                out.push_str(&" ".repeat(11));
            }
        }
        out
    }
}

///
/// A Text record
///
/// Small temporary struct to wrap around a string view and verify that it can
/// be printed inside an ENDF text record
///
#[derive(Debug)]
pub struct TextRecord<'a> {
    text: &'a str,
}

///
/// Allow to try to create the record from string
///
impl<'a> TextRecord<'a> {
    pub fn new(s: &'a str) -> Result<Self, FormatError> {
        if s.len() > 66 {
            Err(FormatError::new(
                FormatErrorKind::WrongLineLength {
                    expected: 66,
                    length: s.len(),
                },
                s,
            ))
        } else if !s.is_ascii() {
            Err(FormatError::new(FormatErrorKind::NonASCII, s))
        } else {
            Ok(Self { text: s })
        }
    }
}

///
/// Allow string views to be interpreted as TEXT records
///
impl<'a> Record for TextRecord<'a> {
    fn stringify(&self) -> String {
        format!("{:<66}", self.text)
    }
}

///
/// INTG (Integer) Record
///
/// Is composed of two 5-digit integers ii and jj followed by number of
/// integers composed of maximum `digits` number of digits.
///
/// In text form it spans only a single row. Number of digits lies in
/// 2-6 (inclusive) range.
///
#[derive(Debug)]
pub struct IntgRecord {
    ii: i32,
    jj: i32,
    digits: u32,
    max_ints: usize,
    kij: [i32; 18],
}

impl IntgRecord {
    ///
    /// Local helper map between number of digits and expected number of numbers
    /// in a line
    ///
    /// Since we need to cover only 5 cases allowed in ENDF it is cleaner to just
    /// list them all
    ///
    /// # Panics
    /// If the digits are outside of 2-6 range (inclusive).
    ///
    fn num_of_ints(digits: u32) -> usize {
        match digits {
            2 => 18,
            3 => 13,
            4 => 11,
            5 => 9,
            6 => 8,
            _ => panic!("Invalid digit range"),
        }
    }

    ///
    /// Create new [IntgRecord] from a line of ENDF
    ///
    /// `digits` must be in 2-6 (inclusive) range.
    ///
    pub fn from_endf_line(s: &str, digits: u32) -> Result<Self, EndfError> {
        // Check length
        if s.len() != 66 {
            return Err(FormatError::new(
                FormatErrorKind::WrongLineLength {
                    expected: 66,
                    length: s.len(),
                },
                s,
            )
            .into());
        }

        // Check the digits
        if !(2..7).contains(&digits) {
            return Err(FormatError::misc(&format!(
                "Number of digits {} must be in 2-6 range (inclusive).",
                digits
            ))
            .into());
        }

        let ii = to_i32(&s[0..5])?;
        let jj = to_i32(&s[5..10])?;

        // If number of digits is 6 there is no space after 2nd index
        let start: usize = if digits != 6 { 11 } else { 10 };

        // Calculate number of entries and length of the number field
        let n = Self::num_of_ints(digits);
        let length = ((digits as usize + 1) * n) as usize;

        // We construct temporary vector sadly
        let nums = s[start..start + length]
            .as_bytes()
            .chunks((digits + 1) as usize)
            .map(|v| std::str::from_utf8(v).unwrap())
            .map(|v| to_i32(v))
            .collect::<Result<Vec<_>, ParseError>>()?;
        Ok(Self::new(ii, jj, digits, nums.into_iter())?)
    }

    ///
    /// Construct new [IntgRecord] from components
    ///
    /// # Arguments
    /// - `ii` - First index. Maximum of 5 decimal digits
    /// - `ii` - Second index. Maximum of 5 decimal digits
    /// - `digits` - Number of digits for numbers that follow. Range 2-6 and
    ///              does not include sign (i.e. `digits=2` expects 3-character
    ///              fields)
    /// - `nums` - Iterator over the integers to fill the record. Must fit into
    ///            `digits` decimal digits plus sign. May be shorter than maximum
    ///            number of entries. The remainder will be padded with 0s.
    ///
    pub fn new(
        ii: i32,
        jj: i32,
        digits: u32,
        nums: impl Iterator<Item = i32>,
    ) -> Result<Self, FormatError> {
        // Check the range of the indices
        if !(-9999..99999).contains(&ii) || !(-9999..99999).contains(&jj) {
            return Err(FormatError::misc(&format!(
                "Indices {}, {} do not fit in 5-digit fields",
                ii, jj
            )));
        }
        // Check the digits
        if !(2..7).contains(&digits) {
            return Err(FormatError::misc(&format!(
                "Number of digits {} must be in 2-6 range (inclusive).",
                digits
            )));
        }

        // Construct the range of admissible integers
        let range = -10_i32.pow(digits)..10_i32.pow(digits);
        let max_ints = Self::num_of_ints(digits);
        let mut res = Self {
            ii,
            jj,
            digits,
            max_ints,
            kij: [0; 18],
        };

        // Fill the array with numbers
        let mut iter = nums;

        for i in 0..res.max_ints {
            match iter.next() {
                Some(num) => {
                    if !range.contains(&num) {
                        return Err(FormatError::misc(&format!(
                            "Number {} does not fit ino {}",
                            num, digits
                        )));
                    }
                    res.kij[i] = num;
                }
                None => break,
            }
        }
        if iter.next().is_some() {
            return Err(FormatError::misc(
                "Some elements in the iterator are unused",
            ));
        }

        Ok(res)
    }

    ///
    /// Get ii index
    ///
    pub fn ii(&self) -> i32 {
        self.ii
    }

    ///
    /// Get jj index
    ///
    pub fn jj(&self) -> i32 {
        self.jj
    }

    ///
    /// Get maximum number of decimal digits of the numbers
    ///
    pub fn digits(&self) -> u32 {
        self.digits
    }

    ///
    /// Get the numbers
    ///
    pub fn kij(&self) -> &[i32] {
        &self.kij[..self.max_ints]
    }
}

impl Record for IntgRecord {
    fn stringify(&self) -> String {
        // Case of 6-digit is special
        // there is no space after 2nd index
        let pad = match self.digits {
            6 => "",
            _ => " ",
        };
        let mut res = format!("{:>5}{:>5}{}", self.ii, self.jj, pad);

        // Add the numbers
        let nums = self.kij().into_iter().map(|n| match self.digits {
            2 => format!("{:>3}", n),
            3 => format!("{:>4}", n),
            4 => format!("{:>5}", n),
            5 => format!("{:>6}", n),
            6 => format!("{:>7}", n),
            _ => panic!("Should never be the case!"),
        });

        for num in nums {
            res.push_str(&num);
        }

        // Pad the string to 66 character
        format!("{:<66}", res)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cont_erros() {
        // Test errors from all number conversions
        assert!(
            ContRecord::from_string(
                " 100x.00000 .999242000          0          0          0          0"
            )
            .is_err(),
            "Failed to throw error reading C1"
        );
        assert!(
            ContRecord::from_string(
                " 1000.00000 .x99242000          0          0          0          0"
            )
            .is_err(),
            "Failed to throw error reading C2"
        );
        assert!(
            ContRecord::from_string(
                " 1000.00000 .999242000       a  0          0          0          0"
            )
            .is_err(),
            "Failed to throw error reading L1"
        );
        assert!(
            ContRecord::from_string(
                " 1000.00000 .999242000          0          k          0          0"
            )
            .is_err(),
            "Failed to throw error reading L2"
        );
        assert!(
            ContRecord::from_string(
                " 1000.00000 .999242000          0          0        s 0          0"
            )
            .is_err(),
            "Failed to throw error reading N1"
        );
        assert!(
            ContRecord::from_string(
                " 1000.00000 .999242000          0          0          0        a 0"
            )
            .is_err(),
            "Failed to throw error reading N2"
        );

        assert!(
            ContRecord::from_string(" 1000.00000 .999242000          0").is_err(),
            "Accepted wrong length string"
        );
    }

    #[test]
    fn test_cont() {
        let cont = ContRecord::from_string(
            " 1000.00000 .999242000          1          2         30         90",
        )
        .unwrap();
        assert_eq!(1000.0, cont.c1);
        assert_eq!(0.999242, cont.c2);
        assert_eq!(1, cont.l1);
        assert_eq!(2, cont.l2);
        assert_eq!(30, cont.n1);
        assert_eq!(90, cont.n2);
    }

    #[test]
    fn test_cont_print() {
        let result = "-1.000000+0 2.000000+1          1          2          3          4";
        assert_eq!(
            result,
            ContRecord {
                c1: -1.0,
                c2: 20.0,
                l1: 1,
                l2: 2,
                n1: 3,
                n2: 4
            }
            .write()
        );
    }

    #[test]
    fn test_dir_print() {
        let result = "                                1          2          3          4";
        assert_eq!(
            result,
            DirRecord {
                l1: 1,
                l2: 2,
                n1: 3,
                n2: 4
            }
            .write()
        );
    }

    #[test]
    fn test_list_print() {
        let result = "-1.000000+0 2.000000+1          1          2          7          4\
                   \x201.000000+0 2.000000+0 3.000000+0 4.000000+0 5.000000+0 6.000000+0\
                   \x207.000000+0                                                       ";
        let list = ListRecord {
            c1: -1.0,
            c2: 20.0,
            l1: 1,
            l2: 2,
            n2: 4,
            list: (1..8).map(|n| n as f64).collect(),
        };
        assert_eq!(result, list.write());
    }

    #[test]
    fn test_tab1_print() {
        let result = "-1.000000+0 2.000000+1          1          2          2          4\
                   \x20         2          1          4          2                      \
                   \x201.000000+0 4.000000+0 2.000000+0 2.000000+0 3.000000+0 1.000000+0\
                   \x204.000000+0        0.0                                            ";

        let table = Table::from(
            vec![1., 2., 3., 4.],
            vec![4., 2., 1., 0.],
            vec![InterpolationType::Histogram, InterpolationType::LinLin],
            vec![1, 3],
        )
        .unwrap();
        let record = TableRecord {
            c1: -1.,
            c2: 20.0,
            l1: 1,
            l2: 2,
            table,
        };

        assert_eq!(result, record.write());
    }

    #[test]
    fn test_tab2_print() {
        let result = "-1.000000+0 2.000000+1          1          2          2         14\
                    \x20         7          1         14          5                      ";
        let record = Table2Record {
            c1: -1.0,
            c2: 20.0,
            l1: 1,
            l2: 2,
            nz: 14,
            regions: vec![6, 13],
            interpolation: vec![InterpolationType::Histogram, InterpolationType::LogLog],
        };
        assert_eq!(result, record.write());
    }

    #[test]
    fn test_text_print() {
        let result = "This is a string printed                                          ";
        let text = TextRecord::new("This is a string printed").unwrap();
        assert_eq!(result, text.write());
    }

    #[test]
    fn test_text_errors() {
        assert!(TextRecord::new(
            "This string contains more than 66 character and does not fit into ENDF line"
        )
        .is_err());

        assert!(TextRecord::new("This string contains is not ascii χρ").is_err());
    }

    #[test]
    fn test_intg_record() {
        let cases: &[(u32, &str)] = &[
            (
                2_u32,
                "    1    2 -11-12 12  3 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 ",
            ),
            (
                3_u32,
                "    1    2 -111-111 -11 -11 -11 -11 -11 -11 -11 -11 -11 -11 -11   ",
            ),
            (
                4_u32,
                "    1    2 -1111-1111 -111 -111 -111 -111 -111 -111 -111 -111 -111",
            ),
            (
                5_u32,
                "    1    2 -11111-11111-11111-11111-11111-11111-11111-11111-11111 ",
            ),
            (
                6_u32,
                "    1    2-123456-123456-123456-123456-123456-123456-123456-123456",
            ),
        ];

        // Round trip test
        for (d, s) in cases {
            assert_eq!(*s, IntgRecord::from_endf_line(*s, *d).unwrap().write());
        }
    }

    #[test]
    fn test_intg_from_line_errors() {
        let valid = "    1    2 -11-12 12  3 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 ";
        let invalid = "    1    2 -11-12 12  3 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1";

        // Check invalid range of digits
        assert!(IntgRecord::from_endf_line(valid, 1).is_err());
        assert!(IntgRecord::from_endf_line(valid, 7).is_err());

        // Check wrong line length
        assert!(IntgRecord::from_endf_line(invalid, 2).is_err());
    }

    #[test]
    fn test_new_intg_record() {
        // Allow to short lists and pad with 0s
        let record = IntgRecord::new(1, 2, 3, [1; 10].into_iter()).unwrap();

        assert_eq!(1, record.ii());
        assert_eq!(2, record.jj());
        assert_eq!(3, record.digits());

        assert_eq!([1; 10], record.kij()[..10]);
        assert_eq!([0; 3], record.kij()[10..13]);
    }

    #[test]
    fn test_new_intg_errors() {
        // Too large indices
        assert!(IntgRecord::new(123456, 2, 3, [1; 10].into_iter()).is_err());
        assert!(IntgRecord::new(1, 123456, 3, [1; 10].into_iter()).is_err());

        // Invalid number of digits
        assert!(IntgRecord::new(1, 2, 1, [1; 10].into_iter()).is_err());
        assert!(IntgRecord::new(1, 2, 7, [1; 10].into_iter()).is_err());

        // To large values in the number list
        assert!(IntgRecord::new(1, 2, 2, [100].into_iter()).is_err());
        assert!(IntgRecord::new(1, 2, 3, [1000].into_iter()).is_err());
        assert!(IntgRecord::new(1, 2, 4, [10000].into_iter()).is_err());
        assert!(IntgRecord::new(1, 2, 5, [100000].into_iter()).is_err());
        assert!(IntgRecord::new(1, 2, 6, [1000000].into_iter()).is_err());

        // Too long array
        assert!(IntgRecord::new(123456, 2, 3, [1; 20].into_iter()).is_err());
    }
}
