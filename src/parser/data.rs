use core::fmt;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{anychar, char},
    combinator::{cut, map, map_res, not, opt, recognize, value},
    error::{context, ContextError, FromExternalError, ParseError, VerboseError},
    multi::{many0, many1},
    sequence::{delimited, preceded, separated_pair},
    IResult, Parser,
};
use std::{
    borrow::Cow,
    fmt::Display,
    num::{ParseFloatError, ParseIntError},
};

use super::{
    identifier, num, s_expression, whitespace_delimited, Identifier, Num, OneOrMore, Radix,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Datum<'a> {
    Boolean(Boolean),
    Symbol(Symbol),
    Number(Number<'a>),
    Character(Character),
    String(SchemeString<'a>),
    List(List<'a>),
    Vector(Vector<'a>),
}

pub(super) trait DatumError<'a>:
    ParseError<&'a str>
    + ContextError<&'a str>
    + FromExternalError<&'a str, ParseFloatError>
    + FromExternalError<&'a str, ParseIntError>
{
}

impl<'a, T> DatumError<'a> for T where
    T: ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, ParseFloatError>
        + FromExternalError<&'a str, ParseIntError>
{
}

pub(super) fn datum<'a, E: DatumError<'a>>(input: &'a str) -> IResult<&str, Datum, E> {
    context(
        "datum",
        alt((
            map(boolean, Datum::Boolean),
            map(symbol, Datum::Symbol),
            map(number, Datum::Number),
            map(character, Datum::Character),
            map(string, Datum::String),
            map(list, Datum::List),
            map(vector, Datum::Vector),
        )),
    )(input)
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Boolean(pub bool);

impl Display for Boolean {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.0 {
            write!(f, "#t")
        } else {
            write!(f, "#f")
        }
    }
}

pub(super) fn boolean<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Boolean, E> {
    context(
        "boolean",
        map(preceded(char('#'), alt((char('t'), char('f')))), |c| {
            Boolean(c == 't')
        }),
    )(input)
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Number<'a> {
    Num2(Num<'a>),
    Num8(Num<'a>),
    Num10(Num<'a>),
    Num16(Num<'a>),
}

pub(super) fn number<'a, E: DatumError<'a>>(input: &'a str) -> IResult<&str, Number, E> {
    context(
        "number",
        map(num, |n| match n.prefix.radix {
            Radix::Two => Number::Num2(n),
            Radix::Eight => Number::Num8(n),
            Radix::Ten => Number::Num10(n),
            Radix::Sixteen => Number::Num16(n),
        }),
    )(input)
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Character {
    Any(char),
    Newline,
    Space,
}

impl Display for Character {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, r#"#\"#)?;
        match self {
            Character::Any(c) => write!(f, "{}", c),
            Character::Newline => write!(f, "newline"),
            Character::Space => write!(f, "space"),
        }
    }
}

pub(super) fn character<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Character, E> {
    context(
        "character",
        preceded(
            tag(r#"#\"#),
            alt((
                value(Character::Newline, tag("newline")),
                value(Character::Space, tag("space")),
                map(anychar, Character::Any),
            )),
        ),
    )(input)
}

#[derive(Debug, Clone, PartialEq)]
pub struct SchemeString<'a>(pub Cow<'a, str>);

impl<'a> Display for SchemeString<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\"{}\"", self.0)
    }
}

pub(super) fn string<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, SchemeString, E> {
    context(
        "string",
        delimited(
            context("opening string quote", char('"')),
            map(recognize(many0(string_character)), |c| {
                SchemeString(c.into())
            }),
            context("closing string quote", cut(char('"'))),
        ),
    )(input)
}

type StringCharacter = char;

fn string_character<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&str, StringCharacter, E> {
    alt((
        value('"', tag(r#"\""#)),
        value('\\', tag(r#"\\"#)),
        not(char('"'))
            .and(not(char('\\')))
            .and(anychar)
            .map(|c| c.1),
    ))(input)
}

pub type Symbol = Identifier;

fn symbol<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Symbol, E> {
    context("symbol", identifier)(input)
}

#[derive(Debug, Clone, PartialEq)]
pub enum List<'a> {
    NList(Vec<Datum<'a>>),
    Dot(OneOrMore<Datum<'a>>, Box<Datum<'a>>),
    Abbreviation(Abbreviation),
}

fn delimited_datum<'a, E: DatumError<'a>>(input: &'a str) -> IResult<&'a str, Datum, E> {
    whitespace_delimited(datum)(input)
}

fn list<'a, E: DatumError<'a>>(input: &'a str) -> IResult<&'a str, List, E> {
    context(
        "list",
        alt((
            s_expression(alt((
                map(
                    separated_pair(many1(delimited_datum), char('.'), cut(delimited_datum)),
                    |(one_or_more_datum, cdr_datum)| {
                        List::Dot(one_or_more_datum.try_into().unwrap(), Box::new(cdr_datum))
                    },
                ),
                map(many0(delimited_datum), List::NList),
            ))),
            map(abbreviation, List::Abbreviation),
        )),
    )(input)
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Abbreviation {
    SingleQuote,   // '
    Backtick,      // `
    Unquote,       // ,
    UnquoteSplice, // ,@
}

impl Display for Abbreviation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::SingleQuote => write!(f, "'"),
            Self::Backtick => write!(f, "`"),
            Self::Unquote => write!(f, ","),
            Self::UnquoteSplice => write!(f, ",@"),
        }
    }
}

fn abbreviation<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Abbreviation, E> {
    alt((
        value(Abbreviation::SingleQuote, char('\'')),
        value(Abbreviation::Backtick, char('`')),
        map(preceded(char(','), opt(char('@'))), |splice| {
            if splice.is_some() {
                Abbreviation::UnquoteSplice
            } else {
                Abbreviation::Unquote
            }
        }),
    ))(input)
}

pub type Vector<'a> = Vec<Datum<'a>>;

fn vector<'a, E: DatumError<'a>>(input: &'a str) -> IResult<&'a str, Vector, E> {
    context(
        "vector",
        preceded(
            char('#'),
            delimited(
                context("vector begin", char('(')),
                many0(delimited_datum),
                context("vector end", cut(char(')'))),
            ),
        ),
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_boolean() {
        assert_eq!(boolean::<VerboseError<&str>>("#t"), Ok(("", Boolean(true))));
        assert_eq!(
            boolean::<VerboseError<&str>>("#f"),
            Ok(("", Boolean(false)))
        );
    }

    #[test]
    fn test_character() {
        assert_eq!(
            character::<VerboseError<&str>>(r#"#\a"#),
            Ok(("", Character::Any('a')))
        );
        assert_eq!(
            character::<VerboseError<&str>>(r#"#\newline"#),
            Ok(("", Character::Newline))
        );
        assert_eq!(
            character::<VerboseError<&str>>(r#"#\space"#),
            Ok(("", Character::Space))
        )
    }

    #[test]
    fn test_string_character() {
        assert_eq!(
            string_character::<VerboseError<&str>>(r#"\""#),
            Ok(("", '"'))
        );
        assert_eq!(
            string_character::<VerboseError<&str>>(r#"\\"#),
            Ok(("", '\\'))
        );
        assert_eq!(string_character::<VerboseError<&str>>("f"), Ok(("", 'f')));
    }

    #[test]
    fn test_string() {
        assert_eq!(
            string::<VerboseError<&str>>(r#""hello, world!""#),
            Ok(("", SchemeString("hello, world!".into())))
        );
        assert!(string::<VerboseError<&str>>(r#""hello, world!"#).is_err());
    }

    #[test]
    fn test_abbreviation() {
        assert_eq!(
            abbreviation::<VerboseError<&str>>("'"),
            Ok(("", Abbreviation::SingleQuote))
        );
        assert_eq!(
            abbreviation::<VerboseError<&str>>("`"),
            Ok(("", Abbreviation::Backtick))
        );
        assert_eq!(
            abbreviation::<VerboseError<&str>>(","),
            Ok(("", Abbreviation::Unquote))
        );
        assert_eq!(
            abbreviation::<VerboseError<&str>>(",@"),
            Ok(("", Abbreviation::UnquoteSplice))
        );
    }

    #[test]
    fn test_list() {
        assert_eq!(
            list::<VerboseError<&str>>("()"),
            Ok(("", List::NList(vec![])))
        );
        assert_eq!(
            list::<VerboseError<&str>>("(a b c)"),
            Ok((
                "",
                List::NList(vec![
                    Datum::Symbol(Identifier::InitialSubsequent("a".into())),
                    Datum::Symbol(Identifier::InitialSubsequent("b".into())),
                    Datum::Symbol(Identifier::InitialSubsequent("c".into()))
                ])
            ))
        );
        assert_eq!(
            list::<VerboseError<&str>>("(a . b)"),
            Ok((
                "",
                List::Dot(
                    OneOrMore::One(Box::new(Datum::Symbol(Identifier::InitialSubsequent(
                        "a".into()
                    )))),
                    Box::new(Datum::Symbol(Identifier::InitialSubsequent("b".into())))
                )
            ))
        );
        assert_eq!(
            list::<VerboseError<&str>>("(a (a b))"),
            Ok((
                "",
                List::NList(vec![
                    Datum::Symbol(Identifier::InitialSubsequent("a".into())),
                    Datum::List(List::NList(vec![
                        Datum::Symbol(Identifier::InitialSubsequent("a".into())),
                        Datum::Symbol(Identifier::InitialSubsequent("b".into()))
                    ]))
                ])
            ))
        );
        assert_eq!(
            list::<VerboseError<&str>>("(a b c . b)"),
            Ok((
                "",
                List::Dot(
                    OneOrMore::More(vec![
                        Datum::Symbol(Identifier::InitialSubsequent("a".into())),
                        Datum::Symbol(Identifier::InitialSubsequent("b".into())),
                        Datum::Symbol(Identifier::InitialSubsequent("c".into()))
                    ]),
                    Box::new(Datum::Symbol(Identifier::InitialSubsequent("b".into())))
                )
            ))
        )
    }

    #[test]
    fn test_datum() {
        assert_eq!(
            datum::<VerboseError<&str>>("#t"),
            Ok(("", Datum::Boolean(Boolean(true))))
        );
        assert_eq!(
            datum::<VerboseError<&str>>("#f"),
            Ok(("", Datum::Boolean(Boolean(false))))
        );

        assert_eq!(
            datum::<VerboseError<&str>>("foo"),
            Ok((
                "",
                Datum::Symbol(Identifier::InitialSubsequent("foo".into()))
            ))
        );

        assert_eq!(
            datum::<VerboseError<&str>>("(a b c)"),
            Ok((
                "",
                Datum::List(List::NList(vec![
                    Datum::Symbol(Identifier::InitialSubsequent("a".into())),
                    Datum::Symbol(Identifier::InitialSubsequent("b".into())),
                    Datum::Symbol(Identifier::InitialSubsequent("c".into()))
                ]))
            ))
        );

        assert_eq!(
            datum::<VerboseError<&str>>("#(a b c)"),
            Ok((
                "",
                Datum::Vector(vec![
                    Datum::Symbol(Identifier::InitialSubsequent("a".into())),
                    Datum::Symbol(Identifier::InitialSubsequent("b".into())),
                    Datum::Symbol(Identifier::InitialSubsequent("c".into()))
                ])
            ))
        )
    }
}
