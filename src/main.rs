use endfio::metadata::{
    DataType, EvaluationInfo, IncidentParticle, LibraryInfo, NuclideInfo, ResonanceInfo,
};

fn main() {
    // Create empty tape
    let mut tape = endfio::Tape::default();

    // Build a material from components
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

    let mat = endfio::Material::new(128.try_into().unwrap(), lib, data, eval, IncidentParticle::Ion(4.0, 2,4), DataType::NuclearInteractionData,
    "This is a very long string that will span multiple lines and preferably should be loded from an external file
    becouse I can hardly see it beeing useful writen explicitly").unwrap();

    tape.insert("H-2".parse().unwrap(), mat);

    tape.to_file("From scratch", "Nothing important").unwrap();
}
