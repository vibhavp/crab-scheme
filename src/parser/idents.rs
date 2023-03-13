use core::fmt;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, one_of, satisfy},
    combinator::{map, recognize},
    error::ParseError,
    multi::many0_count,
    sequence::tuple,
    IResult,
};
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Identifier {
    InitialSubsequent(String),
    Plus,
    Minus,
    Ellipsis,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Identifier::InitialSubsequent(s) => write!(f, "{}", s),
            Identifier::Plus => write!(f, "+"),
            Identifier::Minus => write!(f, "-"),
            Identifier::Ellipsis => write!(f, "..."),
        }
    }
}

pub(super) fn identifier<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Identifier, E> {
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
enum Initial {
    Letter(Letter),
    Other(char), // | ! | $ | % | & | * | / | : | < | = | > | ? | ~ | _ | ^
}

fn initial<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&str, Initial, E> {
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

fn subsequent<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Subsequent, E> {
    alt((
        map(initial, Subsequent::Initial),
        map(digit_ident, Subsequent::Digit),
        map(one_of(".+-"), Subsequent::Other),
    ))(input)
}

type Letter = char; // a | b | ... | z

fn letter<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&str, Letter, E> {
    satisfy(|c| c.is_alphabetic())(input)
}

type DigitIdent = char; // 0 | 1 | ... | 9

pub(super) fn digit_ident<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&str, char, E> {
    satisfy(|c| c.is_ascii_digit())(input)
}

#[cfg(test)]
mod test {
    use nom::error::VerboseError;

    use super::*;

    #[test]
    fn test_identifier() {
        assert_eq!(
            identifier::<VerboseError<&str>>("hello"),
            Ok(("", Identifier::InitialSubsequent("hello".into())))
        )
    }
}
