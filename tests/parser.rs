extern crate crab_scheme;

use std::fs::read_to_string;

use nom::error::VerboseError;
#[test]
fn parse_basic_file() {
    let input =
        read_to_string("test_data/fft.ss").expect("test_data/fft.ss should be a scheme file");
    let program =
        crab_scheme::parser::program::<_, VerboseError<&str>>(&input).expect("should be parsed");
    println!("{:#?}", program);
}
