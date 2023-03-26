use core::fmt;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{anychar, char},
    combinator::{cut, map, not, opt, recognize, value},
    error::{context, ContextError, FromExternalError, ParseError},
    multi::{many0, many1},
    sequence::{delimited, preceded, separated_pair},
    AsChar, Compare, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition, Offset,
    Parser, Slice,
};
use std::{
    borrow::Cow,
    fmt::{Display, Error, Formatter},
    num::{ParseFloatError, ParseIntError},
    ops::{RangeFrom, RangeTo},
};

use super::{
    identifier, num, s_expression, whitespace_delimited, Constant, Identifier, Num, OneOrMore,
    ParseToNumber, ToIdentifier,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Datum<'a> {
    Boolean(Boolean),
    Symbol(Symbol),
    Number(Number),
    Character(Character),
    String(SchemeString<'a>),
    List(List<'a>),
    Vector(Vector<'a>),
}

impl<'a> Display for Datum<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        match self {
            Self::Boolean(b) => write!(f, "{}", b),
            Self::Symbol(s) => write!(f, "{}", s),
            Self::Number(n) => write!(f, "num"),
            Self::Character(c) => write!(f, "{}", c),
            Self::String(s) => write!(f, "{}", s),
            _ => todo!(),
        }
    }
}

impl<'a> From<Constant<'a>> for Datum<'a> {
    fn from(value: Constant<'a>) -> Self {
        match value {
            Constant::Boolean(b) => Self::Boolean(b),
            Constant::Character(c) => Self::Character(c),
            Constant::Number(n) => Self::Number(n),
            Constant::String(s) => Self::String(s),
        }
    }
}

pub trait DatumParseError<I>:
    ParseError<I>
    + ContextError<I>
    + FromExternalError<I, ParseFloatError>
    + FromExternalError<I, ParseIntError>
{
}

impl<'a, T> DatumParseError<&'a str> for T where
    T: ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, ParseFloatError>
        + FromExternalError<&'a str, ParseIntError>
{
}

pub trait ParseToDatum<'a>: ParseToNumber + ToIdentifier<'a> + Into<Cow<'a, str>> {}
impl<'a, T> ParseToDatum<'a> for T where T: ParseToNumber + ToIdentifier<'a> + Into<Cow<'a, str>> {}

pub(super) fn datum<'a, I, E: DatumParseError<I>>(input: I) -> IResult<I, Datum<'a>, E>
where
    I: ParseToDatum<'a>,
    <I as InputIter>::Item: AsChar + Clone + Copy,
    <I as InputTakeAtPosition>::Item: AsChar + Clone + Copy,
{
    context(
        "datum",
        alt((
            map(boolean, Datum::Boolean),
            map(number, Datum::Number),
            map(symbol, Datum::Symbol),
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

pub(super) fn boolean<I, E: ParseError<I> + ContextError<I>>(input: I) -> IResult<I, Boolean, E>
where
    I: Slice<RangeFrom<usize>> + InputIter + Clone,
    <I as InputIter>::Item: AsChar,
{
    context(
        "boolean",
        map(preceded(char('#'), alt((char('t'), char('f')))), |c| {
            Boolean(c == 't')
        }),
    )(input)
}

pub type Number = Num;

pub(super) fn number<'a, I, E: DatumParseError<I>>(input: I) -> IResult<I, Number, E>
where
    I: ParseToNumber,
    <I as InputIter>::Item: AsChar + Clone,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
    context("number", num)(input)
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

pub(super) fn character<'a, I, E: ParseError<I> + ContextError<I>>(
    input: I,
) -> IResult<I, Character, E>
where
    I: Slice<RangeFrom<usize>> + InputIter + InputTake + InputLength + Compare<&'a str> + Clone,
    <I as InputIter>::Item: AsChar,
{
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

pub(super) fn string<'a, I, E: ParseError<I> + ContextError<I>>(
    input: I,
) -> IResult<I, SchemeString<'a>, E>
where
    I: Slice<RangeFrom<usize>>
        + Slice<RangeTo<usize>>
        + InputIter
        + InputTake
        + InputLength
        + Compare<&'a str>
        + Offset
        + Clone
        + Into<Cow<'a, str>>,
    <I as InputIter>::Item: AsChar,
{
    let string_character = alt((
        value('"', tag::<_, I, _>(r#"\""#)),
        value('\\', tag(r#"\\"#)),
        not(char('"'))
            .and(not(char('\\')))
            .and(anychar)
            .map(|c| c.1),
    ));
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

pub type Symbol = Identifier;

fn symbol<'a, I, E: ParseError<I> + ContextError<I>>(input: I) -> IResult<I, Symbol, E>
where
    I: ToIdentifier<'a>,
    <I as InputIter>::Item: AsChar + Copy,
{
    context("symbol", identifier)(input)
}

#[derive(Debug, Clone, PartialEq)]
pub enum List<'a> {
    NList(Vec<Datum<'a>>),
    Dot(OneOrMore<Datum<'a>>, Box<Datum<'a>>),
    Abbreviation(Abbreviation),
}

impl<'a> Display for List<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        fn print_elems<'a>(elems: &Vec<Datum<'a>>, f: &mut Formatter) -> Result<(), Error> {
            for (idx, elem) in elems.iter().enumerate() {
                write!(f, "{}", elem)?;
                if idx != elems.len() - 1 {
                    write!(f, " ")?;
                }
            }
            Ok(())
        }
        match self {
            Self::NList(elems) => {
                write!(f, "(")?;
                print_elems(elems, f)?;
                write!(f, ")")
            }
            Self::Dot(elems, cdr) => {
                write!(f, "(")?;
                match elems {
                    OneOrMore::One(d) => write!(f, "{}", d)?,
                    OneOrMore::More(e) => print_elems(e, f)?,
                }
                write!(f, " . {}", cdr)
            }
            _ => todo!(),
        }
    }
}

fn delimited_datum<'a, I, E: DatumParseError<I>>(input: I) -> IResult<I, Datum<'a>, E>
where
    I: ParseToDatum<'a>,
    <I as InputIter>::Item: AsChar + Clone + Copy,
    <I as InputTakeAtPosition>::Item: AsChar + Clone + Copy,
{
    whitespace_delimited(datum)(input)
}

fn list<'a, I, E: DatumParseError<I>>(input: I) -> IResult<I, List<'a>, E>
where
    I: ParseToDatum<'a>,
    <I as InputIter>::Item: AsChar + Clone + Copy,
    <I as InputTakeAtPosition>::Item: AsChar + Clone + Copy,
{
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

fn abbreviation<I, E: ParseError<I>>(input: I) -> IResult<I, Abbreviation, E>
where
    I: Slice<RangeFrom<usize>> + InputIter + Clone,
    <I as InputIter>::Item: AsChar,
{
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

fn vector<'a, I, E: DatumParseError<I>>(input: I) -> IResult<I, Vector<'a>, E>
where
    I: ParseToDatum<'a>,
    <I as InputIter>::Item: AsChar + Clone + Copy,
    <I as InputTakeAtPosition>::Item: AsChar + Clone + Copy,
{
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
    use nom::error::VerboseError;

    use super::*;

    #[test]
    fn test_boolean() {
        assert_eq!(
            boolean::<&str, VerboseError<&str>>("#t"),
            Ok(("", Boolean(true)))
        );
        assert_eq!(
            boolean::<&str, VerboseError<&str>>("#f"),
            Ok(("", Boolean(false)))
        );
    }

    #[test]
    fn test_string() {
        assert_eq!(
            string::<_, VerboseError<&str>>(r#""hello, world!""#),
            Ok(("", SchemeString("hello, world!".into())))
        );
        assert!(string::<_, VerboseError<&str>>(r#""hello, world!"#).is_err());
    }

    #[test]
    fn test_abbreviation() {
        assert_eq!(
            abbreviation::<_, VerboseError<_>>("'"),
            Ok(("", Abbreviation::SingleQuote))
        );
        assert_eq!(
            abbreviation::<_, VerboseError<_>>("`"),
            Ok(("", Abbreviation::Backtick))
        );
        assert_eq!(
            abbreviation::<_, VerboseError<_>>(","),
            Ok(("", Abbreviation::Unquote))
        );
        assert_eq!(
            abbreviation::<_, VerboseError<_>>(",@"),
            Ok(("", Abbreviation::UnquoteSplice))
        );
    }

    #[test]
    fn test_list() {
        assert_eq!(
            list::<_, VerboseError<&str>>("()"),
            Ok(("", List::NList(vec![])))
        );
        assert_eq!(
            list::<_, VerboseError<&str>>("(a b c)"),
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
            list::<_, VerboseError<&str>>("(a . b)"),
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
            list::<_, VerboseError<&str>>("(a (a b))"),
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
            list::<_, VerboseError<&str>>("(a b c . b)"),
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
            datum::<_, VerboseError<&str>>("#t"),
            Ok(("", Datum::Boolean(Boolean(true))))
        );
        assert_eq!(
            datum::<_, VerboseError<&str>>("#f"),
            Ok(("", Datum::Boolean(Boolean(false))))
        );

        assert_eq!(
            datum::<_, VerboseError<&str>>("foo"),
            Ok((
                "",
                Datum::Symbol(Identifier::InitialSubsequent("foo".into()))
            ))
        );

        assert_eq!(
            datum::<_, VerboseError<&str>>("(a b c)"),
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
            datum::<_, VerboseError<&str>>("#(a b c)"),
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
