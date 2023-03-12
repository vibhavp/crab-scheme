use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, one_of, satisfy},
    combinator::{map, recognize},
    error::VerboseError,
    multi::many0_count,
    sequence::tuple,
    IResult,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Identifier {
    InitialSubsequent(String),
    Plus,
    Minus,
    Ellipsis,
}

pub(super) fn identifier(input: &str) -> IResult<&str, Identifier, VerboseError<&str>> {
    alt((
        map(recognize(tuple((initial, many0_count(subsequent)))), |s| {
            Identifier::InitialSubsequent(s.to_lowercase())
        }),
        map(char('+'), |_| Identifier::Plus),
        map(char('-'), |_| Identifier::Minus),
        map(tag("..."), |_| Identifier::Ellipsis),
    ))(input)
}

#[derive(Debug, Clone)]
pub enum Initial {
    Letter(Letter),
    Other(char), // | ! | $ | % | & | * | / | : | < | = | > | ? | ~ | _ | ^
}

fn initial(input: &str) -> IResult<&str, Initial, VerboseError<&str>> {
    alt((
        map(letter, Initial::Letter),
        map(one_of("!$%&*/:<=>?~_^"), Initial::Other),
    ))(input)
}

enum Subsequent {
    Initial(Initial),
    Digit(DigitIdent),
    Other(char), // . | + | -
}

fn subsequent(input: &str) -> IResult<&str, Subsequent, VerboseError<&str>> {
    alt((
        map(initial, Subsequent::Initial),
        map(digit_ident, Subsequent::Digit),
        map(one_of(".+-"), Subsequent::Other),
    ))(input)
}

type Letter = char; // a | b | ... | z

fn letter(input: &str) -> IResult<&str, Letter, VerboseError<&str>> {
    satisfy(|c| c.is_alphabetic())(input)
}

type DigitIdent = char; // 0 | 1 | ... | 9

pub(super) fn digit_ident(input: &str) -> IResult<&str, char, VerboseError<&str>> {
    satisfy(|c| c.is_ascii_digit())(input)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_identifier() {
        assert_eq!(
            identifier("hello"),
            Ok(("", Identifier::InitialSubsequent("hello".into())))
        )
    }
}
