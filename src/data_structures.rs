//! Contains main data structures used in ENDF
//!
//! At the moment contains only one-dimensional table (`y(x)`) which can
//! be composed of multiple interpolation regions.
//!
//! For know only the basic types of interpolation encountered in Photon and
//! Neutron data are collected in [InterpolationType]. The special interpolation
//! (6) for charged particles and, two-dimensional interpolation flags (11-15)
//! and (21-25) are not yet included.
//!
use is_sorted::IsSorted;

use crate::error::FormatError;

///
/// Enumerator of ENDF Interpolation types
///
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InterpolationType {
    /// Adjusts left i.e.: F(x) = F(x_l) for x in [x_l, x_r]
    Histogram,
    /// Linear interpolation in X and Y
    LinLin,
    /// Linear interpolation in Y and Logarithmic in X
    LinLog,
    /// Logarithmic interpolation in Y and Linear in X
    LogLin,
    /// Logarithmic interpolation in X and Y
    LogLog,
}

impl InterpolationType {
    ///
    /// Convert a integer flag from ENDF file to an interpolation type
    ///
    pub fn from_flag(flag: i32) -> Result<Self, FormatError> {
        match flag {
            1 => Ok(Self::Histogram),
            2 => Ok(Self::LinLin),
            3 => Ok(Self::LinLog),
            4 => Ok(Self::LogLin),
            5 => Ok(Self::LogLog),
            _ => Err(FormatError::misc(&format!(
                "Invalid interpolation flag: {}",
                flag
            ))),
        }
    }

    ///
    /// Convert the interpolation type back to ENDF flag
    ///
    pub fn to_flag(&self) -> i32 {
        match self {
            Self::Histogram => 1,
            Self::LinLin => 2,
            Self::LinLog => 3,
            Self::LogLin => 4,
            Self::LogLog => 5,
        }
    }
}

///
/// 1D Tabulated data following ENDF specifications
///
/// Contains tabulated data points together with supplementary information about
/// interpolation.
///
/// # Warning
/// ENDF represents regions using 1-indexing, but here we use 0-indexing to
/// be consistent with Rust
///
#[derive(Debug)]
pub struct Table {
    /// Sorted list of X-grid points, allows repeated values to represent
    /// discontinuous data
    pub x: Vec<f64>,
    /// Associated values on Y-grid
    pub y: Vec<f64>,
    /// [`InterpolationType`][InterpolationType] for each region
    pub interpolation: Vec<InterpolationType>,
    /// Index of top of each interpolation region (Uses 0-Indexing unlike ENDF)
    pub regions: Vec<usize>,
}

impl Table {
    ///
    /// Try to initialise Table from components
    ///
    pub fn from(
        x: Vec<f64>,
        y: Vec<f64>,
        interpolation: Vec<InterpolationType>,
        regions: Vec<usize>,
    ) -> Result<Self, FormatError> {
        // Check that data is not empty
        if x.is_empty() || y.is_empty() || interpolation.is_empty() || regions.is_empty() {
            return Err(FormatError::misc("Input vectors must not be empty."));
        }

        // Check that data is valid
        if x.len() != y.len() {
            return Err(FormatError::misc(&format!(
                "Size of x grid ({}) and y grid ({}) does not match",
                x.len(),
                y.len()
            )));
        } else if interpolation.len() != regions.len() {
            return Err(FormatError::misc(&format!(
                "Number of interpolation regions ({}) and boundaries ({}) does not match",
                interpolation.len(),
                regions.len()
            )));
        } else if !IsSorted::is_sorted(&mut x.iter()) {
            return Err(FormatError::misc("Values on x grid are not sorted."));
        } else if !IsSorted::is_sorted(&mut regions.iter()) {
            return Err(FormatError::misc(
                "Interpolation region boundaries are not sorted.",
            ));
        } else if *regions.last().unwrap() != x.len() - 1 {
            return Err(FormatError::misc(&format!(
                "Last interpolation region ends at {} but data length is {}",
                regions.last().unwrap(),
                x.len()
            )));
        }

        Ok(Self {
            x,
            y,
            interpolation,
            regions,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_interpolationtype() {
        assert_eq!(
            InterpolationType::Histogram,
            InterpolationType::from_flag(1).unwrap()
        );
        assert_eq!(
            InterpolationType::LinLin,
            InterpolationType::from_flag(2).unwrap()
        );
        assert_eq!(
            InterpolationType::LinLog,
            InterpolationType::from_flag(3).unwrap()
        );
        assert_eq!(
            InterpolationType::LogLin,
            InterpolationType::from_flag(4).unwrap()
        );
        assert_eq!(
            InterpolationType::LogLog,
            InterpolationType::from_flag(5).unwrap()
        );

        // Invalid flag
        assert!(
            InterpolationType::from_flag(-2).is_err(),
            "Failed to detect invalid flag."
        )
    }

    #[test]
    fn test_interpolationtype_toflag() {
        assert_eq!(1, InterpolationType::Histogram.to_flag());
        assert_eq!(2, InterpolationType::LinLin.to_flag());
        assert_eq!(3, InterpolationType::LinLog.to_flag());
        assert_eq!(4, InterpolationType::LogLin.to_flag());
        assert_eq!(5, InterpolationType::LogLog.to_flag());
    }

    #[test]
    fn test_table() {
        let x: Vec<f64> = vec![1.0, 2.0, 2.0, 4.0];
        let y: Vec<f64> = vec![7.0, -2.0, 2.0, 1.0];
        let regions: Vec<usize> = vec![1, 3];
        let inter: Vec<InterpolationType> =
            vec![InterpolationType::LinLin, InterpolationType::LogLog];

        let table = Table::from(x.clone(), y.clone(), inter.clone(), regions.clone()).unwrap();

        itertools::assert_equal(x, table.x);
        itertools::assert_equal(y, table.y);
        itertools::assert_equal(inter, table.interpolation);
        itertools::assert_equal(regions, table.regions);
    }

    #[test]
    fn test_table_errors() {
        let x: Vec<f64> = vec![1.0, 2.0, 2.0, 4.0];
        let y: Vec<f64> = vec![7.0, -2.0, 2.0, 1.0];
        let regions: Vec<usize> = vec![1, 3];
        let inter: Vec<InterpolationType> =
            vec![InterpolationType::LinLin, InterpolationType::LogLog];

        // Empty data
        assert!(
            Table::from(vec![], y.clone(), inter.clone(), regions.clone()).is_err(),
            "Failed to detect empty input vectors"
        );

        // Data size mismatch
        assert!(
            Table::from(
                vec![1.0, 2.0, 2.0],
                y.clone(),
                inter.clone(),
                regions.clone()
            )
            .is_err(),
            "Failed to detect x-y size mismatch"
        );
        assert!(
            Table::from(x.clone(), y.clone(), inter.clone(), vec![3]).is_err(),
            "Failed to detect inter-region size mismatch"
        );

        // Unsorted data
        assert!(
            Table::from(
                vec![1.0, 0.9, 2.0, 4.0],
                y.clone(),
                inter.clone(),
                regions.clone()
            )
            .is_err(),
            "Failed to detect unsorted x grid"
        );
        assert!(
            Table::from(x.clone(), y.clone(), inter.clone(), vec![3, 1]).is_err(),
            "Failed to detect unsorted regions"
        );

        // Invalid region data
        assert!(
            Table::from(x.clone(), y.clone(), inter.clone(), vec![1, 4]).is_err(),
            "Failed to detect mismatched last region"
        );
    }
}
