//! Contains I/O tests
//!
//! Test in this module read some tape from in the 'files' subfolder.
//!
//! If the tape is known to contain defects test check if they are detected
//!
//! If tape is correct they write it back and compare with the input
//! (round-trip test)
//!
use std::io::BufRead;
use std::path::PathBuf;

///
/// Helper function to get a path to file with a particular case
///
fn get_test_filepath(case_name: &str) -> String {
    let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    d.push("tests/files/");
    d.push(case_name);
    String::from(d.to_str().unwrap())
}

#[test]
fn test_read_write_correct_file() {
    let path = get_test_filepath("correct.endf");

    let tape = endfio::Tape::from_file(&path).unwrap();

    // Print to a file in memory
    let mut buf = Vec::new();
    tape.to_writer(&mut buf, "TPID Information").unwrap();

    let buf = std::str::from_utf8(&buf).unwrap();

    let file = std::fs::File::open(path).unwrap();
    let reference_lines = std::io::BufReader::new(file).lines();

    for (l1, l2) in std::iter::zip(buf.lines(), reference_lines) {
        assert_eq!(l1, l2.unwrap());
    }
}

#[test]
fn test_unknown_nlib() {
    let path = get_test_filepath("unknown_nlib.endf");
    let tape = endfio::Tape::from_file(&path);

    match tape {
        Err(endfio::error::EndfError::Format { .. }) => {}
        Err(e) => assert!(false, "Wrong error type {:?}", e),
        Ok(_) => assert!(false, "Tape did not return an error"),
    }
}

#[test]
fn test_wrong_date_format() {
    let path = get_test_filepath("wrong_date_format.endf");
    let tape = endfio::Tape::from_file(&path);

    match tape {
        Err(endfio::error::EndfError::Format { .. }) => {}
        Err(e) => assert!(false, "Wrong error type {:?}", e),
        Ok(_) => assert!(false, "Tape did not return an error"),
    }
}

#[test]
fn test_wrong_master_date_format() {
    let path = get_test_filepath("wrong_master_date_format.endf");
    let tape = endfio::Tape::from_file(&path);

    match tape {
        Err(endfio::error::EndfError::Format { .. }) => {}
        Err(e) => assert!(false, "Wrong error type {:?}", e),
        Ok(_) => assert!(false, "Tape did not return an error"),
    }
}

#[test]
fn test_too_long_directory() {
    let path = get_test_filepath("too_long_directory.endf");
    let tape = endfio::Tape::from_file(&path);

    match tape {
        Err(endfio::error::EndfError::Format { .. }) => {}
        Err(e) => assert!(false, "Wrong error type {:?}", e),
        Ok(_) => assert!(false, "Tape did not return an error"),
    }
}

#[test]
fn test_too_short_directory() {
    let path = get_test_filepath("too_short_directory.endf");
    let tape = endfio::Tape::from_file(&path);

    match tape {
        Err(endfio::error::EndfError::Format { .. }) => {}
        Err(e) => assert!(false, "Wrong error type {:?}", e),
        Ok(_) => assert!(false, "Tape did not return an error"),
    }
}

#[test]
fn test_mismatched_directory() {
    let path = get_test_filepath("mismatched_directory.endf");
    let tape = endfio::Tape::from_file(&path);

    match tape {
        Err(endfio::error::EndfError::Format { .. }) => {}
        Err(e) => assert!(false, "Wrong error type {:?}", e),
        Ok(_) => assert!(false, "Tape did not return an error"),
    }
}

#[test]
fn test_non_ascii() {
    let path = get_test_filepath("non_ascii.endf");
    let tape = endfio::Tape::from_file(&path);

    match tape {
        Err(endfio::error::EndfError::Format { .. }) => {}
        Err(e) => assert!(false, "Wrong error type {:?}", e),
        Ok(_) => assert!(false, "Tape did not return an error"),
    }
}
