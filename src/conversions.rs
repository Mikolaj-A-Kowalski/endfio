//! Contains functions for reading and writing numbers in ENDF format
//!
//! # Integers
//! Following the ENDF Manual, the integers consist of up to 10 digits and
//! a sign (with `+` sign beeing optional) e.g.: `±1234567890`.
//! Thus the maximum number that can be represented is `9'999'999'999`.
//!
//! Since the very large numbers are unlikely to be present we use [i32] for the
//! ENDF integers (with maximum of 2'147'483'647). The drawback is that we loose
//! a lot of range, the benefit that we do not need to worry about integer
//! fitting into 11-character window.Thus, we can get away with using standard
//! RUST printing for the integers.
//!
//! # Floats
//! Technically the allowed real numbers in ENDF files are the ones that can
//! be read using the `E11.0` Fortran format. It admits any traditionally
//! formatted real number in ,scientific or engineering notation. However, in
//! addition following special 'E-less' format is admitted. e.g:
//!   - `±1.23456789`
//!   - `±1.234567±8`
//!   - `±1.23456±78`
//!
//! Are all valid number. Although not explicitly allowed in ENDF Manual, here
//! we extend the definition to allow numbers with exponent above `38` and
//! also with exponent composed of 3 digits e.g. `±1.2345±100`.
//!
//! Since RUST does not support writing of numbers to fixed width fields or
//! parsing using the 'E-less' format, special procedures [to_f64] and
//! [print_endf_float] are used to read and write the numbers.
//!
//! ## Todo
//! Currently each float which is not `0` or `Inf` or `NaN` is printed using
//! the 'E-less' format. However, this approach removes some precision from
//! the numbers with low exponent.
//!
use crate::error::ParseError;
use lazy_static::lazy_static;
use regex::Regex;

// We need to wrap regex like this so it is compiled only once on first use
lazy_static! {
    static ref ENDF_FLOAT: Regex = Regex::new(r"^([-+]?\d+\.\d*)([-+]\d+)$").unwrap();
}

///
/// Reads an integer in ENDF format from string
///
/// Empty string is interpreted as 0, otherwise normal.
/// ```
/// use endfio::conversions::to_i32;
///
/// assert_eq!(0, endfio::conversions::to_i32("  ").unwrap());
/// assert_eq!(-2, endfio::conversions::to_i32(" -2 ").unwrap());
/// ```
/// Returns ParseIntError for invalid string. That is a string which
/// contains multiple numbers, a float or not a number.
/// ```
/// use endfio::conversions::to_i32;
///
/// assert!(to_i32("  1  2 ").is_err());
/// assert!(to_i32("  1.2 ").is_err());
/// ```
///
///
pub fn to_i32(string: &str) -> Result<i32, ParseError> {
    let trimmed = string.trim();
    if trimmed.is_empty() {
        return Ok(0);
    }
    trimmed.parse::<i32>().map_err(|e| ParseError::Int {
        kind: e,
        context: string.to_string(),
    })
}

///
/// Reads a float in ENDF format from string
///
/// Reads any standard float format and a special ENDF format, where
/// 'exponent' is omitted e.g. '1.2+2' as '120.0'.
///
/// # Example
/// ```
/// use endfio::conversions::to_f64;
///
/// assert_eq!(1.2e-12, to_f64("  1.2E-12 ").unwrap());
/// assert_eq!(1e20, to_f64("  1.+20 ").unwrap());
/// ```
///
pub fn to_f64(string: &str) -> Result<f64, ParseError> {
    let trimmed = string.trim();
    let captures = ENDF_FLOAT.captures(trimmed);
    let num = match captures {
        Some(c) => format!(
            "{}E{}",
            &c.get(1).unwrap().as_str(),
            &c.get(2).unwrap().as_str()
        )
        .parse::<f64>(),
        _ => trimmed.parse::<f64>(),
    };
    num.map_err(|e| ParseError::Float {
        kind: e,
        context: string.to_string(),
    })
}

///
/// Enumerator of result of the rounding.
///
/// There are two cases, in one rounding was successful, in the other the number
/// was composed of '9's only and exponent needs to be increased
///
#[derive(Debug)]
enum RoundResult {
    Ok,
    AllNines,
}

///
/// Round up an unsigned integer string
///
/// It assumes that the 'rounding' digit is not given in `bytes`. For example
/// if rounding `12337` it needs to be given only `1233`!
///
/// # Arguments
/// * 'bytes' - contains utf-8 formatted digits representing an integer number
///
/// # Returns
///   RoundResult::AllNines if exponent needs to be increased after conversion
///
fn round_up_decimal(bytes: &mut [u8]) -> RoundResult {
    for c in bytes.iter_mut().rev() {
        if *c == b'9' {
            *c = b'0';
        } else {
            *c += 1;
            return RoundResult::Ok;
        }
    }
    RoundResult::AllNines
}

///
/// Round an unsigned integer string
///
/// If the last digit is less than 5, it replaces it with 0.
/// Otherwise it rounds up the digits (potentially all).
///
/// In special case when the number is all '9's (with the exception of last
/// digit which is greater or equal to 5), all digits will become '0'
/// and it returns a RoundResult::AllNines to mark that exponent needs to be
/// increased.
///
/// # Arguments
/// * 'bytes' - contains utf-8 formatted digits representing an integer number
///
fn round_decimal(bytes: &mut [u8]) -> RoundResult {
    // Copy the rounding digit
    let round_digit = *bytes.last().unwrap();

    if round_digit >= b'5' {
        let n = bytes.len() - 1;
        // We need to feed bytes without the last one
        round_up_decimal(&mut bytes[0..n])
    } else {
        *bytes.last_mut().unwrap() = b'0';
        RoundResult::Ok
    }
}

///
/// Prints a floating point number in ENDF format
///
/// Technically it is ENDF-like format since it does not limit the exponent
/// magnitude to 38 and will print NaNs and infs.
///
/// Each number is printed into a field of 11 characters as follows:
/// e.g.: `±1.234567±0`; `±1.23456±12`; `±1.2345±123`. The number of significant
/// digits in the mantissa is determined by the number of digits in the
/// exponent.
///
/// ```
/// use endfio::conversions::print_endf_float;
///
/// // Prints exponential notation for exact matches
/// assert_eq!("-1.234567+0", print_endf_float(-1.234567));
/// assert_eq!(" 1.23456+23", print_endf_float( 1.23456E23));
///
/// // Pads with 0s if number of significant digits is small
/// assert_eq!("-1.200000+0", print_endf_float(-1.2));
///
/// // Rounds the number if number of significant digits does not fit
/// assert_eq!("-1.234567+0", print_endf_float(-1.2345666));
/// assert_eq!("-1.234567+0", print_endf_float(-1.2345666));
/// assert_eq!("-1.234566+0", print_endf_float(-1.2345664));
///
/// // Rounding correctly carries the carry
/// assert_eq!(" 1.0000+100", print_endf_float( 9.9999999E+99));
///
/// // Code deals with NaNs and Infs
/// assert_eq!("        NaN", print_endf_float( 0.0/0.0));
/// assert_eq!("       -inf", print_endf_float( -1.0/0.0));
///
/// // Zero is printed like a float for better visibility
/// assert_eq!("        0.0", print_endf_float( 0.0));
/// ```
pub fn print_endf_float(num: f64) -> String {
    // Short circuit special cases
    if num == 0.0 {
        return "        0.0".to_string();
    } else if !num.is_finite() {
        return format!("{:>11}", num);
    }

    // First we convert binary to decimal
    let sign = if num < 0.0 { '-' } else { ' ' };
    let dec = lexical_write_float::algorithm::to_decimal(num);

    // We write the digits to a string
    let mut out = format!("{}", dec.mant);

    // Get exponent in a normalised number (one digit before comma)
    let exp = dec.exp + out.len() as i32 - 1;

    // Get number of digits in the exponent
    let num_exp_digits = if exp != 0 { exp.abs().ilog10() + 1 } else { 1 };

    // Trim the mantissa to the maximum number of digits + 1 (to retain rounding digit)
    let n = (10 - num_exp_digits - 2 + 1) as usize;
    out.truncate(n);

    // If number of significant digits was not enough we need to pad with 0s
    for _ in 0..n - out.len() {
        out.push('0');
    }

    // Now we perform rounding
    // Is Safe as long the `out` strings contains utf-8 formatted digits
    // Since we have written it ourselves we know it to be the case.
    unsafe {
        if let RoundResult::AllNines = round_decimal(out.as_bytes_mut()) {
            // We know that string is all 0s at this point
            // We can just add 1. at the beginning
            out.insert_str(0, "1.");

            // We need to recalculate the number of digits in exponent
            // (it might have changed)
            let exp = exp + 1;
            let num_exp_digits = exp.abs().ilog10() + 1;
            let n = (10 - num_exp_digits - 1) as usize;
            out.truncate(n);
            return format!("{sign}{out}{exp:+}");
        }
    }
    out.insert(1, '.');
    out.truncate(n);
    format!("{sign}{out}{exp:+}")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_i32_conversion() {
        // Success
        assert_eq!(4, to_i32("   4").unwrap());
        assert_eq!(0, to_i32("   0").unwrap());
        assert_eq!(0, to_i32("    ").unwrap());
        assert_eq!(3, to_i32("  3  ").unwrap());

        // Failure
        assert!(to_i32("  1.2 ").is_err());
        assert!(to_i32("  1.E-8 ").is_err());
        assert!(to_i32("  1.0897-8 ").is_err());
        assert!(to_i32(" a word ").is_err());
    }

    #[test]
    fn test_f64_conversion() {
        // Success
        assert_eq!(1.2, to_f64("    1.2 ").unwrap());
        assert_eq!(1.2e20, to_f64("  1.200E+20 ").unwrap());
        assert_eq!(1.2e20, to_f64("  1.200+20 ").unwrap());
    }

    #[test]
    fn test_print_endf_float() {
        // Test the rounding

        // No rounding
        assert_eq!("-1.333333+0", print_endf_float(-1.333333));

        // Round down
        assert_eq!("-1.333333+0", print_endf_float(-1.3333334));

        // Round up
        assert_eq!("-1.333334+0", print_endf_float(-1.3333336));

        // Round a case where carry increases the exponent
        assert_eq!("-1.0000+100", print_endf_float(-9.999997e99));
    }
}
