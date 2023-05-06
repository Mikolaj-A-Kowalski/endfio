//! Structures and functions to deal with information about physical properties
//! and library type
//!
//!
use chrono::naive::NaiveDate;
use regex::Regex;
use std::{collections::HashMap, str::FromStr};

use lazy_static::lazy_static;

use crate::error::{FormatError, FormatErrorKind};

const ELEMENT_SYMBOLS: &[&str] = &[
    "H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne", "Na", "Mg", "Al", "Si", "P", "S", "Cl",
    "Ar", "K", "Ca", "Sc", "Ti", "V", "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn", "Ga", "Ge", "As",
    "Se", "Br", "Kr", "Rb", "Sr", "Y", "Zr", "Nb", "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In",
    "Sn", "Sb", "Te", "I", "Xe", "Cs", "Ba", "La", "Ce", "Pr", "Nd", "Pm", "Sm", "Eu", "Gd", "Tb",
    "Dy", "Ho", "Er", "Tm", "Yb", "Lu", "Hf", "Ta", "W", "Re", "Os", "Ir", "Pt", "Au", "Hg", "Tl",
    "Pb", "Bi", "Po", "At", "Rn", "Fr", "Ra", "Ac", "Th", "Pa", "U", "Np", "Pu", "Am", "Cm", "Bk",
    "Cf", "Es", "Fm",
];

lazy_static! {
    /// Map of element symbols (upper case) to atomic number
    static ref ELEMENT_ATOMIC_NUMBERS: HashMap<String, usize> = {
        let out: HashMap<String, usize> = ELEMENT_SYMBOLS
            .iter()
            .enumerate()
            .map(|(idx, sym)| (sym.to_ascii_uppercase(), idx + 1))
            .collect();
        out
    };
}

///
/// Return element symbol given its atomic number
///
/// Only elements with atomic number in 1-100 range are supported.
/// First letter is capitalised. That is:
/// ```
/// use endfio::metadata::element_symbol;
///
/// assert_eq!("H", element_symbol(1).unwrap());
/// assert_eq!("Pu",element_symbol(94).unwrap());
///
/// // Is error outside the range
/// assert!(element_symbol(101).is_err());
/// ```
///
///
pub fn element_symbol(atomic_number: usize) -> Result<&'static str, FormatError> {
    if atomic_number == 0 || atomic_number > 100 {
        return Err(FormatError::misc(&format!(
            "Atomic number ({}) must be in 1-100 range.",
            atomic_number
        )));
    }
    Ok(ELEMENT_SYMBOLS[atomic_number - 1])
}

///
/// Return atomic number of an element given its symbol
///
/// Symbol is case insensitive. All "Pu", "pu", "PU" and "pU" should be accepted.
/// ```
/// use endfio::metadata::atomic_number;
///
/// assert_eq!(94, atomic_number("Pu").unwrap());
/// assert_eq!(94, atomic_number("pu").unwrap());
/// assert_eq!(94, atomic_number("PU").unwrap());
/// assert_eq!(94, atomic_number("pU").unwrap());
///
/// // Only Elements in 1-100 atomic number range are known
/// assert!(atomic_number("Lr").is_err());
/// ```
///
pub fn atomic_number(symbol: &str) -> Result<usize, FormatError> {
    let key = symbol.to_ascii_uppercase();
    match ELEMENT_ATOMIC_NUMBERS.get(&key) {
        Some(atomic_number) => Ok(*atomic_number),
        None => Err(FormatError::new(
            FormatErrorKind::Misc,
            &format!("'{}' is not avalid element symbol", symbol),
        )),
    }
}

///
/// Return Library name given NLIB number
///
/// Returns "Unknown" if NLIB is not defined in ENDF Manual:
/// ```
/// use endfio::metadata::nlib_to_str;
///
/// assert_eq!("JEFF", nlib_to_str(2));
/// assert_eq!("Unknown", nlib_to_str(99));
/// ```
///
pub fn nlib_to_str(nlib: i32) -> &'static str {
    match nlib {
        0 => "ENDF/B",
        1 => "ENDF/A",
        2 => "JEFF",
        3 => "EFF",
        4 => "ENDF/B High Energy",
        5 => "CENDL",
        6 => "JENDL",
        17 => "TENDL",
        18 => "ROSFOND",
        21 => "SG-23",
        31 => "INDL/V",
        32 => "INDL/A",
        33 => "FENDL",
        34 => "IRDF",
        35 => "BROND",
        36 => "INGDB-90",
        37 => "FENDL/A",
        41 => "BROND",
        _ => "Unknown",
    }
}

///
/// Return Name of the file given mf number
///
/// Returns "Unknown" if MF is not defined in ENDF Manual
///
/// ```
/// use endfio::metadata::mf_to_str;
///
/// assert_eq!("General information", mf_to_str(1));
/// assert_eq!("Unknown", mf_to_str(-2));
/// ```
///
pub fn mf_to_str(mf: i32) -> &'static str {
    match mf {
        1 => "General information",
        2 => "Resonance parameter data",
        3 => "Reaction cross sections",
        4 => "Angular distributions for emitted particles",
        5 => "Energy distributions for emitted particles",
        6 => "Energy-angle distributions for emitted particles",
        7 => "Thermal neutron scattering law data",
        8 => "Radioactivity and fission-product yield data",
        9 => "Multiplicities for radioactive nuclide production",
        10 => "Cross sections for radioactive nuclide production",
        12 => "Multiplicities for photon production",
        13 => "Cross sections for photon production",
        14 => "Angular distributions for photon production",
        15 => "Energy distributions for photon production",
        23 => "Photo- or electro-atomic interaction cross sections",
        26 => "Electro-atomic angle and energy distribution",
        27 => "Atomic form factors or scattering functions for photo-atomic interactions",
        28 => "Atomic relaxation data",
        30 => "Data covariances obtained from parameter covariances and sensitivities",
        31 => "Data covariances for ν(bar)",
        32 => "Data covariances for resonance parameters",
        33 => "Data covariances for reaction cross sections",
        34 => "Data covariances for angular distributions",
        35 => "Data covariances for energy distributions",
        39 => "Data covariances for radionuclide production yields",
        40 => "Data covariances for radionuclide production cross sections",
        _ => "Unknown",
    }
}

///
/// Return human readable description of the contained data
///
pub fn sublibrary_name(part: &IncidentParticle, kind: &DataType) -> String {
    match kind {
        DataType::NuclearInteractionData => format!("Incident-{} Data", part.name()),
        DataType::FissionYeldsData => format!("{}-Induced Fission Product Yields", part.name()),
        DataType::ThermalScatteringData => format!("Thermal {} Scattering Data", part.name()),
        DataType::EMInteractionData => format!("{}-Atomic Interaction Data", part.name()),
        DataType::DecayData => "Radioactive Decay data".to_string(),
        DataType::SpontaneousFissionYields => "Spontaneous Fission Product Yields".to_string(),
        DataType::AtomicRelaxation => "Atomic Relaxation Data".to_string(),
    }
}

///
/// List of 'key' isotopes for elements 1-98 which are assigned with MAT number ZZ25.
/// Obtained by parsing TENDL2019 library. We assume that other libraries follow
/// the same convention
///
const KEY_ISOTOPES: &[i32] = &[
    1, 3, 6, 9, 10, 12, 14, 16, 19, 20, 23, 24, 27, 28, 31, 32, 35, 36, 39, 40, 45, 46, 50, 50, 55,
    54, 59, 58, 63, 64, 69, 70, 75, 74, 79, 78, 85, 84, 89, 90, 93, 92, 99, 96, 103, 102, 107, 106,
    113, 112, 121, 120, 127, 124, 133, 130, 138, 136, 141, 142, 139, 144, 151, 152, 159, 156, 165,
    162, 169, 168, 175, 174, 180, 180, 185, 184, 191, 190, 197, 196, 203, 204, 209, 206, 203, 211,
    212, 223, 225, 227, 229, 234, 230, 235, 235, 240, 240, 240,
];

lazy_static! {
    static ref ISOTOPE_PATTERN: Regex =
        Regex::new(r"^([a-zA-Z]{1,3})-?(\d{1,3})?(m|m2|M|M2)?$").unwrap();
    static ref SPECIAL_MAT: HashMap<i32, &'static str> = {
        let res: HashMap<i32, &'static str> = [
            (1, "Water"),
            (2, "Para Hydrogen"),
            (3, "Ortho Hydrogen"),
            (7, "H in ZrH"),
            (11, "Heavy Water"),
            (12, "Para Deuterium"),
            (13, "Ortho Deuterium"),
            (26, "Beryllium"),
            (27, "BeO"),
            (28, "Be₂C"),
            (29, "Be in BeO"),
            (31, "Graphite"),
            (33, "Liquid Methane"),
            (34, "Solid Methane"),
            (37, "Polyethylene"),
            (40, "Benzene"),
            (46, "O in BeO"),
            (47, "O in SiO₂"),
            (48, "O in UO₂"),
            (53, "Al metal"),
            (56, "Fe metal"),
            (58, "Zr in ZeH"),
            (75, "UO₂"),
            (76, "UC"),
            (9900, "Es"),
            (9911, "Es-251"),
            (9912, "Es-252"),
            (9913, "Es-253"),
            (9914, "Es-254"),
            (9915, "Es-254m"),
            (9916, "Es-255"),
            (9920, "Fm"),
            (9936, "Fm-255"),
        ]
        .into_iter()
        .collect();
        res
    };
    static ref SPECIAL_MAT_INV: HashMap<&'static str, i32> =
        SPECIAL_MAT.iter().map(|(k, v)| (*v, *k)).collect();
}

///
/// Convert MAT number to a string
///
/// String follows format like: Pu-239m2, Pu-239m1, Pu-239, Pu etc.
///
/// Rules for assigning MAT numbers are outlined in documentation of
/// [`str_to_mat`][str_to_mat].
///
/// For ordinary isotopes (Z ∈ \[1;98\]) uses ENDF rules, where the key isotope
/// for which the mass part of the MAT number is 25 was inferred from TENDL2019
/// library:
/// ```
/// use endfio::metadata::mat_to_str;
///
/// assert_eq!("U-238", mat_to_str(9237).unwrap());
/// assert_eq!("U-230", mat_to_str(9213).unwrap());
/// assert_eq!("Am-242m", mat_to_str(9547).unwrap());
/// ```
/// Mass number is not printed if MAT number refers to an Element:
/// ```
/// use endfio::metadata::mat_to_str;
///
/// assert_eq!("U", mat_to_str(9200).unwrap());
/// assert_eq!("Pu", mat_to_str(9400).unwrap());
/// ```
/// The cases of the compounds and heavy elements with Z >= 99 are handled by a
/// dictionary. Only the cases included in ENDF VIII/B.0 library are included
/// (so for e.g. TENDL some of the isotopes will not be convertible to strings)
/// ```
/// use endfio::metadata::mat_to_str;
///
/// assert_eq!("Graphite", mat_to_str(31).unwrap());
/// assert_eq!("Water", mat_to_str(1).unwrap());
/// assert_eq!("Fm-255", mat_to_str(9936).unwrap());
/// assert_eq!("Fm", mat_to_str(9920).unwrap());
///
/// // Present in TENDL2019 but will not convert
/// assert!(mat_to_str(9931).is_err());
///
/// ```
pub fn mat_to_str(mat: i32) -> Result<String, FormatError> {
    if !(0..9999).contains(&mat) {
        return Err(FormatError::new(
            FormatErrorKind::InvalidMATRange { mat },
            "When converting from mat to string",
        ));
    }
    // Split MAT number into atomic and mass number parts
    let atomic_number = (mat / 100) as usize;
    let remainder = mat % 100;

    if atomic_number == 0 || atomic_number >= 99 {
        // Material is a compound or a heavy element(Z >= 99)
        match SPECIAL_MAT.get(&mat) {
            Some(s) => Ok(s.to_string()),
            None => Err(FormatError::misc(&format!(
                "Did not found compound corresponding to MAT: {}",
                mat
            ))),
        }
    } else if remainder == 0 {
        // Pure element case
        Ok(element_symbol(atomic_number)?.to_string())
    } else {
        let key = KEY_ISOTOPES[atomic_number - 1];

        // We need to transpose the numbers so we are correctly rounding
        // towards -inf even if remainder < 25
        let mass = ((remainder - 25 + 9 * 3) / 3) - 9 + key;
        let metastate = match (remainder - 25 + 9 * 3) % 3 {
            0 => "",
            1 => "m",
            2 => "m2",
            _ => panic!("Impossible remainder of modulo 3"),
        };
        Ok(format!(
            "{}-{}{}",
            element_symbol(atomic_number)?,
            mass,
            metastate
        ))
    }
}

///
/// Convert string view to a MAT number
///
/// Tries to perform conversion from a human-readable name (symbol) of an isotope
/// to a MAT number.
/// ```
/// use endfio::metadata::str_to_mat;
///
/// // Normal syntax
/// assert_eq!(9400, str_to_mat("Pu").unwrap());
/// assert_eq!(9237, str_to_mat("U-238").unwrap());
/// assert_eq!(9547, str_to_mat("Am-242m").unwrap());
/// assert_eq!(9548, str_to_mat("Am-242m2").unwrap());
///
/// // Dash is optional
/// assert_eq!(9237, str_to_mat("U238").unwrap());
///
/// // Is case-insensitive
/// assert_eq!(9437, str_to_mat("pu-239").unwrap());
/// assert_eq!(9437, str_to_mat("pU-239").unwrap());
/// assert_eq!(9437, str_to_mat("PU239").unwrap());
/// assert_eq!(9548, str_to_mat("AM242M2").unwrap());
///
/// // Supports some compounds as well (but not all!)
/// assert_eq!(1, str_to_mat("Water").unwrap());
/// assert_eq!(26, str_to_mat("Beryllium").unwrap());
///
/// ```
/// # Assigning MAT numbers
///
/// Rules to specify MAT number are defined in section 0.4.1 of
/// [ENDF Manual](https://www-nds.iaea.org/exfor/x4guide/manuals/endf-manual.pdf)
/// and are sadly rather convoluted.
/// For nuclides with atomic number Z < 99, MAT number is composed of 3-4 digits.
/// First two indicate atomic number. The last two are a mass part. For natural
/// elements mass part is `00` that is:
///
/// | Element | MAT number |
/// | ------- | ---------- |
/// | H       | 100        |
/// | U       | 9200       |
/// | Pu      | 9400       |
///
/// For specific nuclides the mass part is '25' for *the lightest stable nuclide*.
/// Clearly this is extremely ambiguous rule (what half-life constitutes *stable*
/// in e.g. Uranium?), made worse by the discovery of new,
/// lighter elements since the MAT numbers were first assigned. In practice
/// one has to find the isotopes for which mass part is '25' by inspection.
/// Here we did it by parsing TENDL2019 library. Some *key* isotopes are:
///
/// | Nuclide | MAT number |
/// | ------- | ---------- |
/// | U-234   | 9225       |
/// | H-1     | 125        |
/// | Pu-235  | 9425       |
///
/// For other isotopes mass part is calculated as `3 * (A - A_key) + 25`. Thus,
/// for each isotope there are 3 possible mat numbers that allow to specify 1st
/// and 2nd metastable state. e.g. see the Americium MAT numbers:
///
/// | Nuclide  | MAT number |
/// | -------- | ---------- |
/// | Am-235   | 9525       |
/// | Am-242   | 9546       |
/// | Am-242m  | 9549       |
/// | Am-242m2 | 9550       |
///
/// For elements with Z >= 99 MAT numbers are assigned without following any
/// clear rules and need to be handled as a special case using a dictionary of
/// known MAT numbers.
///
pub fn str_to_mat(name: &str) -> Result<i32, FormatError> {
    // Check the special MAT list
    if let Some(mat) = SPECIAL_MAT_INV.get(name) {
        return Ok(*mat);
    }
    // Try to match the pattern
    match ISOTOPE_PATTERN.captures(name) {
        Some(groups) => {
            // Get atomic and mass  numbers together with meta-state offset
            let atomic_number = atomic_number(&groups[1])?;
            let mass_number = &groups
                .get(2)
                .map(|nums| nums.as_str().parse::<i32>().unwrap());
            let meta_increment = &groups.get(3).and_then(|meta| match meta.as_str() {
                "m" | "M" => Some(1),
                "m2" | "M2" => Some(2),
                _ => None,
            });
            let key_mass = KEY_ISOTOPES.get(atomic_number - 1).ok_or(
                FormatError::misc(
                    &format!(
                        "Failed to find key isotope for Z={}",
                        atomic_number
                        )))?;
            let mass_part = match mass_number {
                Some(a) => 3 * (*a - key_mass) + 25 + meta_increment.unwrap_or(0),
                None => 0,
            };

            // Check that the mass part of MAT number is in valid range
            if !(0..99).contains(&mass_part) {
                return Err(FormatError::misc(
                    &format!("Failed to convert '{}' to MAT number", name)
                ));
            }

            Ok((atomic_number * 100) as i32 + mass_part)
        }
        None => Err(FormatError::new(
                    FormatErrorKind::Misc,
                    &format!("'{}' name does not name an isotope. If it has Z<99 try e.g. Pu-239m. If Z>=99 or is a \
                        compound try using the number MAT instead.",
                        name
                     )
                )),
    }
}

lazy_static! {
    static ref NLIB_TO_NAME: HashMap<i32, &'static str> = {
        let res: HashMap<i32, &'static str> = [
            (0, "ENDF/B"),
            (1, "ENDF/A"),
            (2, "JEFF"),
            (3, "EFF"),
            (4, "ENDF/B High Energy"),
            (5, "CENDL"),
            (6, "JENDL"),
            (21, "SG-23"),
            (31, "INDL/V"),
            (32, "INDL/A"),
            (33, "FENDL"),
            (34, "IRDF"),
            (35, "BROND"),
            (36, "INGDB-90"),
            (37, "FENDL/A"),
            (41, "BROND"),
        ]
        .into_iter()
        .collect();
        res
    };
    static ref NAME_TO_NLIB: HashMap<&'static str, i32> =
        NLIB_TO_NAME.iter().map(|(k, v)| (*v, *k)).collect();
    static ref LIBRARY_NAME_PATTERN: Regex = Regex::new(r"^([[:ascii:]]+)\s+(\d+).(\d+)$").unwrap();
}

///
/// Stores information about the library
///
/// Contains information about the name and version of the library.
/// Allows to parse it from string.
/// ```
/// use endfio::metadata::LibraryInfo;
/// use std::str::FromStr;
///
/// let lib = LibraryInfo::from_str("ENDF/B 8.1").unwrap();
/// assert_eq!(0, *lib.nlib());
/// let (&ver, &release) = lib.version();
/// assert_eq!(8, ver);
/// assert_eq!(1, release);
/// assert_eq!("ENDF/B", lib.name());
/// ```
/// But it can also be constructed from components
/// ```
/// use endfio::metadata::LibraryInfo;
///
/// let lib = LibraryInfo::new(0, 8, 1).unwrap();
/// let s = lib.to_string();
/// assert_eq!("ENDF/B 8.1", s);
/// ```
///
#[derive(Debug)]
pub struct LibraryInfo {
    nlib: i32,
    nver: i32,
    lrel: i32,
}

impl LibraryInfo {
    ///
    /// Try building a new LibraryInfo from ENDF flags
    ///
    pub fn new(nlib: i32, nver: i32, lrel: i32) -> Result<Self, FormatError> {
        if NLIB_TO_NAME.get(&nlib).is_none() {
            return Err(FormatError::misc(&format!("Unknown NLIB value: {}", nlib)));
        } else if lrel < 0 || nver < 0 {
            return Err(FormatError::misc(&format!(
                "Incorrect library version: {}.{}",
                nver, lrel
            )));
        }
        Ok(Self { nlib, nver, lrel })
    }

    ///
    /// Get value of the NLIB number, which specifies library
    ///
    pub fn nlib(&self) -> &i32 {
        &self.nlib
    }

    ///
    /// Get name of the library
    ///
    pub fn name(&self) -> &'static str {
        NLIB_TO_NAME[&self.nlib]
    }

    ///
    /// Get version tuple
    ///
    /// Return tuple of (library_version, library_release)
    ///
    pub fn version(&self) -> (&i32, &i32) {
        (&self.nver, &self.lrel)
    }
}

impl FromStr for LibraryInfo {
    type Err = FormatError;

    ///
    /// Convert from string
    ///
    /// Name follows e.g. "SG-23 1.8.2"
    ///
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match LIBRARY_NAME_PATTERN.captures(s) {
            None => Err(FormatError::misc(&format!(
                "{} did not match library name version pattern e.g: ENDF/B 8.1.0",
                s
            ))),
            Some(groups) => {
                let nlib = *NAME_TO_NLIB
                    .get(&groups[1])
                    .ok_or(FormatError::misc("Unknown library"))?;
                let nver = i32::from_str(&groups[2])
                    .map_err(|_| FormatError::misc("Invalid Library format"))?;
                let lrel = i32::from_str(&groups[3])
                    .map_err(|_| FormatError::misc("Invalid Library format"))?;
                Ok(Self { nlib, nver, lrel })
            }
        }
    }
}

impl ToString for LibraryInfo {
    ///
    /// Print the Library name and version in the String form
    ///
    fn to_string(&self) -> String {
        format!("{} {}.{}", NLIB_TO_NAME[&self.nlib], self.nver, self.lrel)
    }
}

///
/// Enumerate possible types of data in the library
#[derive(PartialEq, Eq, Debug)]
pub enum DataType {
    NuclearInteractionData,
    FissionYeldsData,
    ThermalScatteringData,
    EMInteractionData,
    DecayData,
    SpontaneousFissionYields,
    AtomicRelaxation,
}

impl DataType {
    fn from_itype(itype: i32) -> Result<Self, FormatError> {
        match itype {
            0 => Ok(Self::NuclearInteractionData),
            1 => Ok(Self::FissionYeldsData),
            2 => Ok(Self::ThermalScatteringData),
            3 => Ok(Self::EMInteractionData),
            4 => Ok(Self::DecayData),
            5 => Ok(Self::SpontaneousFissionYields),
            6 => Ok(Self::AtomicRelaxation),
            _ => Err(FormatError::misc(&format!("Unknown ITYPE: {}", itype))),
        }
    }

    fn to_itype(&self) -> i32 {
        match self {
            Self::NuclearInteractionData => 0,
            Self::FissionYeldsData => 1,
            Self::ThermalScatteringData => 2,
            Self::EMInteractionData => 3,
            Self::DecayData => 4,
            Self::SpontaneousFissionYields => 5,
            Self::AtomicRelaxation => 6,
        }
    }
}

///
/// Enumerate possible types of incident particles
///
#[derive(Debug, PartialEq)]
pub enum IncidentParticle {
    Photon,
    Electron(f64),
    Neutron(f64),
    Ion(f64, u32, u32),
}

impl IncidentParticle {
    pub fn to_ipart(&self) -> i32 {
        match self {
            Self::Photon => 0,
            Self::Electron(_) => 11,
            Self::Neutron(_) => 1,
            Self::Ion(_, z, a) => (*z as i32) * 1000 + (*a as i32),
        }
    }

    pub fn weight(&self) -> f64 {
        match self {
            Self::Photon => 0.0,
            Self::Electron(w) => *w,
            Self::Neutron(w) => *w,
            Self::Ion(w, _, _) => *w,
        }
    }

    ///
    /// Get human readable name of the particle
    ///
    pub fn name(&self) -> String {
        match self {
            IncidentParticle::Photon => "Photon".to_string(),
            IncidentParticle::Electron(_) => "Electron".to_string(),
            IncidentParticle::Neutron(_) => "Neutron".to_string(),
            IncidentParticle::Ion(_, 1, 1) => "Proton".to_string(),
            IncidentParticle::Ion(_, 1, 2) => "Deuteron".to_string(),
            IncidentParticle::Ion(_, 1, 3) => "Triton".to_string(),
            IncidentParticle::Ion(_, 2, 3) => "Helion".to_string(),
            IncidentParticle::Ion(_, 2, 4) => "Alpha".to_string(),
            IncidentParticle::Ion(_, z, a) => {
                format!("{}-{}", element_symbol(*z as usize).unwrap(), a)
            }
        }
    }

    ///
    /// Create new Incident Particle from ENDF flag and weight
    ///
    /// Weight is expressed in neutron masses i.e. weight=1.0 for neutrons.
    ///
    pub fn new(ipart: i32, weight: f64) -> Result<Self, FormatError> {
        if weight < 0.0 {
            return Err(FormatError::misc(&format!(
                "Weight of the incident particle is : {} nutrons",
                weight
            )));
        }
        match ipart {
            0 => Ok(Self::Photon),
            1 => Ok(Self::Neutron(weight)),
            11 => Ok(Self::Electron(weight)),
            1001..=100999 => Ok(Self::Ion(
                weight,
                (ipart / 1000) as u32,
                (ipart % 1000) as u32,
            )),
            _ => Err(FormatError::misc(&format!("Unknown IPART {}", ipart))),
        }
    }
}

///
/// Generate NSUB ENDF number for a given incident particle and data type
///
pub fn sublibrary_number(part: &IncidentParticle, kind: &DataType) -> i32 {
    part.to_ipart() * 10 + kind.to_itype()
}

///
/// Convert NSUB ENDF number to particle and data type
///
pub fn parse_sublibrary_number(
    nsub: i32,
    weight: f64,
) -> Result<(IncidentParticle, DataType), FormatError> {
    let ipart = nsub / 10;
    let itype = nsub % 10;
    Ok((
        IncidentParticle::new(ipart, weight)?,
        DataType::from_itype(itype)?,
    ))
}

///
/// Struct that holds information about a particular evaluation
///
#[derive(Debug)]
pub struct EvaluationInfo {
    mod_seq: u32,
    ldvr: i32,
    laboratory: String,
    author: String,
    reference: String,
    temperature: f64,
    max_energy: f64,
    eval_date: NaiveDate,
    dist_date: Option<NaiveDate>,
    revision_date: Option<NaiveDate>,
    master_date: Option<NaiveDate>,
}

impl EvaluationInfo {
    ///
    /// Create new instance of the evaluation information pack
    ///
    /// # Arguments
    ///
    /// * `mod_seq` - Number in the modification sequence for this evaluation
    /// * `ldvr` - Weird ENDF number. Use 0 if in doubt.
    /// * `laboratory` - String view with originating laboratory name. Up to 11 characters.
    /// * 'author` - String view with the author(s) name(s). Up to 33 characters.
    /// * 'reference` - String view with the primary reference for the evaluation Up to 20 characters.
    /// * 'temperature` - Temperature of the target nuclide \[K\]
    /// * 'max_energy` - Maximum energy in the evaluation \[eV\]
    /// * `dates` - Tuple of (Evaluation, Distribution, Release, Master file) dates
    ///             given as NaiveDates or None if not known
    ///
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        mod_seq: u32,
        ldvr: i32,
        laboratory: &str,
        author: &str,
        reference: &str,
        temperature: f64,
        max_energy: f64,
        dates: (
            NaiveDate,
            Option<NaiveDate>,
            Option<NaiveDate>,
            Option<NaiveDate>,
        ),
    ) -> Result<Self, FormatError> {
        if laboratory.len() > 11 {
            return Err(FormatError::misc(&format!(
                "Laboratory name '{}' has more the 11 characters",
                laboratory
            )));
        } else if author.len() > 33 {
            return Err(FormatError::misc(&format!(
                "Author name '{}' has more the 33 characters",
                author
            )));
        } else if reference.len() > 20 {
            return Err(FormatError::misc(&format!(
                "Reference '{}' has more the 20 characters",
                reference
            )));
        } else if temperature < 0.0 {
            return Err(FormatError::misc(&format!(
                "Temperature is negative: {} K",
                temperature
            )));
        } else if max_energy < 0.0 {
            return Err(FormatError::misc(&format!(
                "Maximum energy is negative: {} eV",
                max_energy
            )));
        }
        Ok(Self {
            mod_seq,
            ldvr,
            laboratory: laboratory.to_string(),
            author: author.to_string(),
            reference: reference.to_string(),
            temperature,
            max_energy,
            eval_date: dates.0,
            dist_date: dates.1,
            revision_date: dates.2,
            master_date: dates.3,
        })
    }

    ///
    /// NMOD number
    ///
    /// Sequence number do distinguish modifications within the same release.
    ///
    pub fn mod_seq(&self) -> &u32 {
        &self.mod_seq
    }

    ///
    /// LDVR Number
    ///
    /// Strange ENDF number which is said to allow to distinguish evaluations
    /// with the same MAT, NMOD and NSUB numbers.
    ///
    pub fn ldvr(&self) -> &i32 {
        &self.ldvr
    }

    ///
    /// A mnemonic of the originating laboratory (up to 11 characters long)
    ///
    /// E.g.: "IAEA", "CEA", "LANL"
    ///
    pub fn labratory(&self) -> &str {
        &self.laboratory
    }

    ///
    /// Name of the evaluation author
    ///
    /// Can contain up to 33 characters.
    ///
    pub fn author(&self) -> &str {
        &self.author
    }

    ///
    /// Name of the evaluation author
    ///
    /// Can contain up to 33 characters.
    ///
    pub fn reference(&self) -> &str {
        &self.reference
    }

    ///
    /// Temperature of the nucleus in Kelvin
    ///
    pub fn temperature(&self) -> &f64 {
        &self.temperature
    }

    ///
    /// Maximum incident particle energy in eV
    ///
    pub fn max_energy(&self) -> &f64 {
        &self.max_energy
    }

    ///
    /// Date of the evaluation
    ///
    pub fn evaluation_date(&self) -> &NaiveDate {
        &self.eval_date
    }

    ///
    /// Date of distribution
    ///
    pub fn distribution_date(&self) -> &Option<NaiveDate> {
        &self.dist_date
    }

    ///
    /// Date of release
    ///
    pub fn revision_date(&self) -> &Option<NaiveDate> {
        &self.revision_date
    }

    ///
    /// Date of 'Master File'
    ///
    /// This is another strange ENDF thing. Manual claims this date is assigned
    /// by NNDC
    ///
    pub fn mater_file_date(&self) -> &Option<NaiveDate> {
        &self.master_date
    }
}

///
/// Enumerator of states of LRP flag
///
/// LRP flag tells how the resonance data is to be used in ENDF file
///
#[derive(Debug, PartialEq, Eq)]
pub enum ResonanceInfo {
    /// No resonance data is given, not allowed for neutrons
    NoResonance,
    /// No resonance data is given except for scattering radius
    OnlyScatteringRadius,
    /// File 3 contains 'background' cross-section that need to be added to
    /// ones obtained from resonances
    UseBackground,
    /// File 3 contains some other data (e.g. reconstructed XSS). It is not
    /// to be used in the reconstruction
    IgnoreBackground,
}

impl ResonanceInfo {
    pub fn from_flag(flag: i32) -> Result<Self, FormatError> {
        match flag {
            -1 => Ok(Self::NoResonance),
            0 => Ok(Self::OnlyScatteringRadius),
            1 => Ok(Self::UseBackground),
            2 => Ok(Self::IgnoreBackground),
            _ => Err(FormatError::misc(&format!("Unknown flag: {}", flag))),
        }
    }

    pub fn to_flag(&self) -> i32 {
        match self {
            Self::NoResonance => -1,
            Self::OnlyScatteringRadius => 0,
            Self::UseBackground => 1,
            Self::IgnoreBackground => 2,
        }
    }
}

///
/// Struct that contains all physical data about a nucleus
///
#[derive(Debug)]
pub struct NuclideInfo {
    atomic_number: u32,
    mass_number: u32,
    atomic_weight_ratio: f64,
    lrp: ResonanceInfo,
    is_fissile: bool,
    excitation_energy: f64,
    is_stable: bool,
    state_number: u32,
    isomeric_state_number: u32,
}

impl NuclideInfo {
    ///
    /// Create Nuclide Info from components
    ///
    /// Checks the constraints of the given arguments
    ///
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        atomic_number: u32,
        mass_number: u32,
        atomic_weight_ratio: f64,
        lrp: ResonanceInfo,
        is_fissile: bool,
        excitation_energy: f64,
        is_stable: bool,
        state_number: u32,
        isomeric_state_number: u32,
    ) -> Result<Self, FormatError> {
        if atomic_weight_ratio <= 0.0 {
            return Err(FormatError::misc(&format!(
                "Atomic weight ratio must be +ve: {}",
                atomic_weight_ratio
            )));
        }
        if state_number < isomeric_state_number {
            return Err(FormatError::misc(&format!(
                "It is not allowed for isomeric state number ({isomeric_state_number}) \
                                to greater then state number ({state_number})"
            )));
        }
        Ok(Self {
            atomic_number,
            mass_number,
            atomic_weight_ratio,
            lrp,
            is_fissile,
            excitation_energy,
            is_stable,
            state_number,
            isomeric_state_number,
        })
    }

    ///
    /// Atomic number of the nuclide
    ///
    pub fn atomic_number(&self) -> &u32 {
        &self.atomic_number
    }

    ///
    /// Mass number of the nuclide
    ///
    /// Is 0 if the target is an element
    pub fn mass_number(&self) -> &u32 {
        &self.mass_number
    }

    ///
    /// Get atomic weight ration
    ///
    /// Mass of the nuclide/element in neutron mass units
    ///
    pub fn atomic_weight_ratio(&self) -> &f64 {
        &self.atomic_weight_ratio
    }

    ///
    /// Get Resonance data flag
    ///
    pub fn lrp(&self) -> &ResonanceInfo {
        &self.lrp
    }

    ///
    /// True is nuclide is fissile
    ///
    pub fn is_fissile(&self) -> bool {
        self.is_fissile
    }

    ///
    /// Get excitation energy relative to ground state \[eV\]
    ///
    pub fn excitation_energy(&self) -> &f64 {
        &self.excitation_energy
    }

    ///
    /// True is nuclide is stable
    ///
    pub fn is_stable(&self) -> bool {
        self.is_stable
    }

    ///
    /// Get state numbers
    ///
    /// # Result
    /// (State Number, Isomeric State Number)
    ///
    pub fn state_numbers(&self) -> (u32, u32) {
        (self.state_number, self.isomeric_state_number)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mat_roundtrip_endf8() {
        // List of all MAT numbers from ENDF.VIII.0/B (neutron) and EPICS2017
        const ENDF_MAT: &[i32] = &[
            125, 128, 131, 225, 228, 325, 328, 419, 425, 525, 528, 625, 628, 725, 728, 825, 828,
            831, 925, 1025, 1028, 1031, 1122, 1125, 1225, 1228, 1231, 1323, 1325, 1425, 1428, 1431,
            1434, 1437, 1525, 1625, 1628, 1631, 1634, 1637, 1725, 1728, 1731, 1825, 1828, 1831,
            1834, 1837, 1840, 1925, 1928, 1931, 2025, 2028, 2031, 2034, 2037, 2040, 2043, 2046,
            2049, 2125, 2225, 2228, 2231, 2234, 2237, 2322, 2325, 2328, 2425, 2428, 2431, 2434,
            2437, 2522, 2525, 2625, 2628, 2631, 2634, 2637, 2722, 2723, 2725, 2825, 2828, 2831,
            2834, 2837, 2840, 2843, 2925, 2928, 2931, 3025, 3028, 3031, 3034, 3037, 3040, 3043,
            3125, 3128, 3131, 3225, 3228, 3231, 3234, 3237, 3240, 3243, 3319, 3322, 3325, 3425,
            3428, 3431, 3434, 3437, 3440, 3443, 3446, 3449, 3525, 3528, 3531, 3625, 3628, 3631,
            3634, 3637, 3640, 3643, 3646, 3649, 3725, 3728, 3731, 3825, 3828, 3831, 3834, 3837,
            3840, 3843, 3925, 3928, 3931, 4025, 4028, 4031, 4034, 4037, 4040, 4043, 4125, 4128,
            4131, 4225, 4228, 4231, 4234, 4237, 4240, 4243, 4246, 4249, 4322, 4325, 4425, 4428,
            4431, 4434, 4437, 4440, 4443, 4446, 4449, 4452, 4455, 4525, 4528, 4531, 4625, 4628,
            4631, 4634, 4637, 4640, 4643, 4646, 4649, 4725, 4728, 4731, 4735, 4737, 4740, 4743,
            4746, 4749, 4752, 4755, 4759, 4825, 4828, 4831, 4834, 4837, 4840, 4843, 4846, 4849,
            4853, 4855, 4925, 4928, 4931, 5025, 5028, 5031, 5034, 5037, 5040, 5043, 5046, 5049,
            5053, 5055, 5058, 5061, 5064, 5067, 5125, 5128, 5131, 5134, 5137, 5140, 5225, 5228,
            5229, 5231, 5234, 5237, 5240, 5243, 5247, 5249, 5253, 5255, 5258, 5259, 5261, 5325,
            5328, 5331, 5334, 5337, 5340, 5341, 5343, 5346, 5349, 5422, 5425, 5428, 5431, 5434,
            5437, 5440, 5443, 5446, 5449, 5452, 5455, 5458, 5461, 5525, 5528, 5531, 5534, 5537,
            5625, 5628, 5631, 5634, 5637, 5640, 5643, 5646, 5649, 5652, 5655, 5725, 5728, 5731,
            5825, 5828, 5829, 5831, 5834, 5837, 5840, 5843, 5846, 5849, 5925, 5928, 5931, 6025,
            6028, 6031, 6034, 6037, 6040, 6043, 6046, 6049, 6137, 6140, 6143, 6146, 6149, 6152,
            6153, 6155, 6158, 6161, 6225, 6228, 6231, 6234, 6237, 6240, 6243, 6246, 6249, 6252,
            6255, 6325, 6328, 6331, 6334, 6337, 6340, 6343, 6425, 6428, 6431, 6434, 6437, 6440,
            6443, 6446, 6449, 6522, 6525, 6528, 6531, 6619, 6622, 6625, 6628, 6631, 6634, 6637,
            6640, 6643, 6646, 6649, 6725, 6729, 6825, 6828, 6831, 6834, 6837, 6840, 6843, 6846,
            6849, 6922, 6925, 6928, 6931, 7025, 7028, 7031, 7034, 7037, 7040, 7043, 7046, 7049,
            7125, 7128, 7225, 7228, 7231, 7234, 7237, 7240, 7243, 7246, 7249, 7325, 7328, 7331,
            7425, 7428, 7431, 7434, 7437, 7440, 7443, 7525, 7529, 7531, 7625, 7628, 7631, 7634,
            7637, 7640, 7643, 7646, 7649, 7725, 7728, 7731, 7735, 7825, 7828, 7831, 7834, 7837,
            7840, 7843, 7846, 7849, 7925, 8025, 8028, 8029, 8031, 8034, 8037, 8040, 8043, 8046,
            8049, 8125, 8128, 8131, 8225, 8228, 8231, 8234, 8237, 8325, 8329, 8431, 8434, 8437,
            8825, 8828, 8831, 8834, 8925, 8928, 8931, 9025, 9028, 9031, 9034, 9037, 9040, 9043,
            9046, 9125, 9128, 9131, 9134, 9137, 9213, 9216, 9219, 9222, 9225, 9228, 9231, 9234,
            9237, 9240, 9243, 9246, 9337, 9340, 9343, 9344, 9346, 9349, 9352, 9428, 9431, 9434,
            9437, 9440, 9443, 9446, 9449, 9452, 9455, 9458, 9540, 9543, 9546, 9547, 9549, 9552,
            9553, 9625, 9628, 9631, 9634, 9637, 9640, 9643, 9646, 9649, 9652, 9655, 9740, 9743,
            9746, 9749, 9752, 9755, 9843, 9846, 9849, 9852, 9855, 9858, 9861, 9864, 9867, 9911,
            9912, 9913, 9914, 9915, 9916, 9936, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000,
            1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400,
            2500, 2600, 2700, 2800, 2900, 3000, 3100, 3200, 3300, 3400, 3500, 3600, 3700, 3800,
            3900, 4000, 4100, 4200, 4300, 4400, 4500, 4600, 4700, 4800, 4900, 5000, 5100, 5200,
            5300, 5400, 5500, 5600, 5700, 5800, 5900, 6000, 6100, 6200, 6300, 6400, 6500, 6600,
            6700, 6800, 6900, 7000, 7100, 7200, 7300, 7400, 7500, 7600, 7700, 7800, 7900, 8000,
            8100, 8200, 8300, 8400, 8500, 8600, 8700, 8800, 8900, 9000, 9100, 9200, 9300, 9400,
            9500, 9600, 9700, 9800, 9900, 9920,
        ];

        // Do round-trip for all
        for mat in ENDF_MAT {
            assert_eq!(*mat, str_to_mat(&mat_to_str(*mat).unwrap()).unwrap());
        }
    }

    #[test]
    fn test_evaluation_info() {
        let info = EvaluationInfo::new(
            2,
            0,
            "MyLab",
            "Dr Neutron",
            "Report",
            273.0,
            20e6,
            (
                "1980-12-1".parse().unwrap(),
                Some("1999-3-30".parse().unwrap()),
                Some("2011-11-11".parse().unwrap()),
                Some("1980-2-1".parse().unwrap()),
            ),
        )
        .unwrap();

        assert_eq!(2, *info.mod_seq());
        assert_eq!(0, *info.ldvr());
        assert_eq!("MyLab", info.labratory());
        assert_eq!("Dr Neutron", info.author());
        assert_eq!("Report", info.reference());
        assert_eq!(273.0, *info.temperature());
        assert_eq!(2.0e7, *info.max_energy());
        assert_eq!(
            NaiveDate::from_str("1980-12-1").unwrap(),
            *info.evaluation_date()
        );
        assert_eq!(
            NaiveDate::from_str("1999-3-30").unwrap(),
            info.distribution_date().unwrap()
        );
        assert_eq!(
            NaiveDate::from_str("2011-11-11").unwrap(),
            info.revision_date().unwrap()
        );
        assert_eq!(
            NaiveDate::from_str("1980-2-1").unwrap(),
            info.mater_file_date().unwrap()
        );
    }

    #[test]
    fn test_evaluation_info_errors() {
        assert!(
            EvaluationInfo::new(
                2,
                0,
                "Eyjafjallajokull",
                "Dr Neutron",
                "Report",
                273.0,
                20e6,
                (
                    "1980-12-1".parse().unwrap(),
                    Some("1999-3-30".parse().unwrap()),
                    Some("2011-11-11".parse().unwrap()),
                    Some("1980-2-1".parse().unwrap())
                ),
            )
            .is_err(),
            "Failed to detect too long labratory name"
        );

        assert!(
            EvaluationInfo::new(
                2,
                0,
                "MyLAB",
                "Dr Friedrich Wilhelm Viktor Albert Hohenzollern",
                "Report",
                273.0,
                20e6,
                (
                    "1980-12-1".parse().unwrap(),
                    Some("1999-3-30".parse().unwrap()),
                    Some("2011-11-11".parse().unwrap()),
                    Some("1980-2-1".parse().unwrap())
                ),
            )
            .is_err(),
            "Failed to detect too long author name"
        );

        assert!(
            EvaluationInfo::new(
                2,
                0,
                "MyLAB",
                "Dr Neutron",
                "Full reference to a paper over 20 characters",
                273.0,
                20e6,
                (
                    "1980-12-1".parse().unwrap(),
                    Some("1999-3-30".parse().unwrap()),
                    Some("2011-11-11".parse().unwrap()),
                    Some("1980-2-1".parse().unwrap())
                ),
            )
            .is_err(),
            "Failed to detect too long reference"
        );

        assert!(
            EvaluationInfo::new(
                2,
                0,
                "MyLAB",
                "Dr Neutron",
                "Reference",
                -273.0,
                20e6,
                (
                    "1980-12-1".parse().unwrap(),
                    Some("1999-3-30".parse().unwrap()),
                    Some("2011-11-11".parse().unwrap()),
                    Some("1980-2-1".parse().unwrap())
                ),
            )
            .is_err(),
            "Failed to detect negative temperature"
        );

        assert!(
            EvaluationInfo::new(
                2,
                0,
                "MyLAB",
                "Dr Neutron",
                "Reference",
                273.0,
                -20e6,
                (
                    "1980-12-1".parse().unwrap(),
                    Some("1999-3-30".parse().unwrap()),
                    Some("2011-11-11".parse().unwrap()),
                    Some("1980-2-1".parse().unwrap())
                ),
            )
            .is_err(),
            "Failed to detect negative maximum energy"
        );
    }

    #[test]
    fn test_resonance_info() {
        let flags = vec![-1, 0, 1, 2];

        for flag in flags {
            assert_eq!(flag, ResonanceInfo::from_flag(flag).unwrap().to_flag());
        }
        assert!(
            ResonanceInfo::from_flag(-2).is_err(),
            "Failed to detect invalid flag"
        );
    }

    #[test]
    fn test_nuclide_info() {
        let info = NuclideInfo::new(
            92,
            235,
            232.87,
            ResonanceInfo::UseBackground,
            true,
            0.0,
            false,
            1,
            0,
        )
        .unwrap();

        // Test getters
        assert_eq!(92, *info.atomic_number());
        assert_eq!(235, *info.mass_number());
        assert_eq!(232.87, *info.atomic_weight_ratio());
        assert_eq!(ResonanceInfo::UseBackground, *info.lrp());
        assert!(info.is_fissile());
        assert_eq!(0.0, *info.excitation_energy());
        assert!(!info.is_stable());
        assert_eq!((1, 0), info.state_numbers())
    }

    #[test]
    fn test_nuclide_info_errors() {
        assert!(
            NuclideInfo::new(
                92,
                235,
                -32.87,
                ResonanceInfo::UseBackground,
                true,
                0.0,
                false,
                0,
                1
            )
            .is_err(),
            "Failed to detect -ve atomic weight ratio"
        );

        assert!(
            NuclideInfo::new(
                92,
                235,
                232.87,
                ResonanceInfo::UseBackground,
                true,
                0.0,
                false,
                1,
                2
            )
            .is_err(),
            "Failed to detect state smaller than isomeric state number"
        );
    }

    #[test]
    fn test_incident_particle() {
        // Test conversion from flag
        assert_eq!(
            IncidentParticle::Photon,
            IncidentParticle::new(0, 0.).unwrap()
        );
        assert_eq!(
            IncidentParticle::Electron(0.001),
            IncidentParticle::new(11, 0.001).unwrap()
        );
        assert_eq!(
            IncidentParticle::Neutron(1.0),
            IncidentParticle::new(1, 1.0).unwrap()
        );
        assert_eq!(
            IncidentParticle::Ion(1.0, 1, 1),
            IncidentParticle::new(1001, 1.0).unwrap()
        );
        assert_eq!(
            IncidentParticle::Ion(2.0, 2, 4),
            IncidentParticle::new(2004, 2.0).unwrap()
        );

        // Invalid IPART
        assert!(IncidentParticle::new(-2, 1.0).is_err());
        assert!(IncidentParticle::new(9, 1.0).is_err());
        assert!(IncidentParticle::new(1, -1.0).is_err());

        // Test conversion to flag
        assert_eq!(0, IncidentParticle::Photon.to_ipart());
        assert_eq!(11, IncidentParticle::Electron(0.001).to_ipart());
        assert_eq!(1, IncidentParticle::Neutron(1.0).to_ipart());
        assert_eq!(4008, IncidentParticle::Ion(4.0, 4, 8).to_ipart());
        assert_eq!(92238, IncidentParticle::Ion(92.0, 92, 238).to_ipart());
    }

    #[test]
    fn test_data_type_enum() {
        let itypes = vec![0, 1, 2, 3, 4, 5, 6];
        let data_types = vec![
            DataType::NuclearInteractionData,
            DataType::FissionYeldsData,
            DataType::ThermalScatteringData,
            DataType::EMInteractionData,
            DataType::DecayData,
            DataType::SpontaneousFissionYields,
            DataType::AtomicRelaxation,
        ];

        // Round trip test
        for itype in &itypes {
            assert_eq!(*itype, DataType::from_itype(*itype).unwrap().to_itype());
        }

        // Conversion test
        for (e, itype) in std::iter::zip(&data_types, &itypes) {
            assert_eq!(*e, DataType::from_itype(*itype).unwrap());
        }

        // Invalid ITYPE
        assert!(DataType::from_itype(-2).is_err());
        assert!(DataType::from_itype(7).is_err());
    }

    #[test]
    fn test_sublibrary_number() {
        let nsubs = vec![
            0, 1, 3, 4, 5, 6, 10, 11, 12, 113, 10010, 10011, 10020, 10030,
        ];

        // Round trip test
        for nsub in &nsubs {
            let (part, kind) = parse_sublibrary_number(*nsub, 1.0).unwrap();
            assert_eq!(*nsub, sublibrary_number(&part, &kind));
        }

        // Check some specific cases
        assert_eq!(
            (
                IncidentParticle::Neutron(1.0),
                DataType::NuclearInteractionData
            ),
            parse_sublibrary_number(10, 1.0).unwrap()
        );
        assert_eq!(
            (IncidentParticle::Photon, DataType::NuclearInteractionData),
            parse_sublibrary_number(0, 0.0).unwrap()
        );
        assert_eq!(
            (
                IncidentParticle::Electron(1.0),
                DataType::NuclearInteractionData
            ),
            parse_sublibrary_number(110, 1.0).unwrap()
        );

        assert_eq!(
            (IncidentParticle::Electron(1.0), DataType::EMInteractionData),
            parse_sublibrary_number(113, 1.0).unwrap()
        );

        assert_eq!(
            (
                IncidentParticle::Neutron(1.0),
                DataType::ThermalScatteringData
            ),
            parse_sublibrary_number(12, 1.0).unwrap()
        );
        assert_eq!(
            (IncidentParticle::Photon, DataType::AtomicRelaxation),
            parse_sublibrary_number(6, 0.0).unwrap()
        );
    }
}
