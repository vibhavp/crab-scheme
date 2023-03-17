use std::num::{ParseFloatError, ParseIntError};

use nom::{
    branch::{alt, permutation},
    bytes::complete::take_while1,
    character::complete::{char, digit0, digit1, one_of},
    combinator::{map, map_res, opt, recognize, success, value},
    error::{context, ContextError, FromExternalError, ParseError},
    multi::many0_count,
    sequence::{pair, preceded, separated_pair, terminated, tuple},
    IResult, Parser,
};

use super::whitespace_delimited;

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Numerical {
    Integer(isize),
    Float(f64),
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Prefix {
    pub radix: Radix,
    pub exactness: Exactness,
}

fn prefix<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Prefix, E> {
    context(
        "prefix",
        map(permutation((radix, exactness)), |(radix, exactness)| {
            Prefix { radix, exactness }
        }),
    )(input)
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Num {
    pub exactness: Exactness,
    pub complex: Complex,
}

pub(super) fn num<
    'a,
    E: ParseError<&'a str>
        + FromExternalError<&'a str, ParseFloatError>
        + FromExternalError<&'a str, ParseIntError>
        + ContextError<&'a str>,
>(
    input: &'a str,
) -> IResult<&str, Num, E> {
    let (input, prefix) = prefix(input)?;
    map(complex(prefix.radix), move |complex| Num {
        exactness: prefix.exactness,
        complex,
    })(input)
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Complex {
    pub real: Option<Real>,
    pub imag: Option<Real>,
}

fn complex<
    'a,
    E: ParseError<&'a str>
        + FromExternalError<&'a str, ParseFloatError>
        + FromExternalError<&'a str, ParseIntError>,
>(
    radix: Radix,
) -> impl FnMut(&'a str) -> IResult<&'a str, Complex, E> {
    alt((
        (map(preceded(char('+'), imag(radix)), |u| Complex {
            real: None,
            imag: Some(Real::Positive(u)),
        })),
        (map(preceded(char('-'), imag(radix)), |u| Complex {
            real: None,
            imag: Some(Real::Negative(u)),
        })),
        (map(
            separated_pair(real(radix), whitespace_delimited(char('+')), imag(radix)),
            |(real, u)| Complex {
                real: Some(real),
                imag: Some(Real::Positive(u)),
            },
        )),
        (map(
            separated_pair(real(radix), whitespace_delimited(char('-')), imag(radix)),
            |(real, u)| Complex {
                real: Some(real),
                imag: Some(Real::Negative(u)),
            },
        )),
        map(real(radix), |r| Complex {
            real: Some(r),
            imag: None,
        }),
    ))
}

fn imag<
    'a,
    E: ParseError<&'a str>
        + FromExternalError<&'a str, ParseIntError>
        + FromExternalError<&'a str, ParseFloatError>,
>(
    radix: Radix,
) -> impl FnMut(&'a str) -> IResult<&'a str, Ureal, E> {
    alt((
        map(char('i'), |_| Ureal::Decimal(1.0)),
        terminated(ureal(radix), char('i')),
    ))
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Real {
    Positive(Ureal),
    Negative(Ureal),
}

fn real<
    'a,
    E: ParseError<&'a str>
        + FromExternalError<&'a str, ParseIntError>
        + FromExternalError<&'a str, ParseFloatError>,
>(
    radix: Radix,
) -> impl FnMut(&'a str) -> IResult<&'a str, Real, E> {
    map(
        tuple((opt(alt((char('+'), char('-')))), ureal(radix))),
        |(sign, ur)| match sign {
            Some('-') => Real::Negative(ur),
            Some('+') | None => Real::Positive(ur),
            _ => Real::Positive(ur),
        },
    )
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Ureal {
    Uinteger(Uinteger),
    Frac(Uinteger, Uinteger),
    Decimal(Decimal10),
}

fn ureal<
    'a,
    E: ParseError<&'a str>
        + FromExternalError<&'a str, ParseIntError>
        + FromExternalError<&'a str, ParseFloatError>,
>(
    radix: Radix,
) -> impl FnMut(&'a str) -> IResult<&'a str, Ureal, E> {
    alt((
        map(decimal10, Ureal::Decimal),
        map(
            separated_pair(
                uinteger(radix),
                whitespace_delimited(char('/')),
                uinteger(radix),
            ),
            |(n, d)| Ureal::Frac(n, d),
        ),
        map(uinteger(radix), Ureal::Uinteger),
    ))
}

fn uinteger10<'a, E: ParseError<&'a str> + FromExternalError<&'a str, ParseIntError>>(
    input: &'a str,
) -> IResult<&'a str, usize, E> {
    uinteger(Radix::Ten)(input)
}

pub type Uinteger = usize;

fn uinteger<'a, E: ParseError<&'a str> + FromExternalError<&'a str, ParseIntError>>(
    radix: Radix,
) -> impl FnMut(&'a str) -> IResult<&'a str, Uinteger, E> {
    map_res(
        pair(
            recognize(take_while1(move |chr: char| chr.is_digit(radix as u32))),
            many0_count(char('#')),
        ),
        move |(digits, zeros)| {
            usize::from_str_radix(digits, radix as u32)
                .map(|u| u * (10_usize).pow(zeros.try_into().unwrap()))
        },
    )
}

pub type Decimal10 = f64;

fn sign<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, i32, E> {
    alt((value(1, char('+')), value(-1, char('-')), success(1)))(input)
}

fn decimal10<
    'a,
    E: ParseError<&'a str>
        + FromExternalError<&'a str, ParseIntError>
        + FromExternalError<&'a str, ParseFloatError>,
>(
    input: &'a str,
) -> IResult<&'a str, Decimal10, E> {
    alt((
        map_res(tuple((recognize(uinteger10), exponent)), |(u, e)| {
            let mut f = String::from(u);
            f.push('e');
            f.push_str(&e.to_string());
            f.parse::<f64>()
        }),
        map_res(
            tuple((
                alt((
                    recognize(tuple((char::<&'a str, _>('.'), digit1))),
                    recognize(tuple((digit1, char('.'), digit0))),
                )),
                opt(recognize(tuple((preceded(one_of("esfdl"), sign), digit1)))),
            )),
            |(base, exp)| {
                let mut f = String::from(base);
                if let Some(e) = exp {
                    f.push_str(&e.replace(['e', 's', 'f', 'd', 'l'], "e"));
                }
                f.parse::<f64>()
            },
        ),
    ))(input)
}

fn exponent<'a, E: ParseError<&'a str> + FromExternalError<&'a str, ParseIntError>>(
    input: &'a str,
) -> IResult<&'a str, i32, E> {
    map_res(
        tuple((preceded(one_of("esfdl"), sign), digit1)),
        |(sign, digits)| digits.parse::<u32>().map(|u| (u as i32 * sign)),
    )(input)
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Exactness {
    Empty,
    Inexact,
    Exact,
}

fn exactness<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Exactness, E> {
    alt((
        value(Exactness::Inexact, char('i')),
        value(Exactness::Exact, char('e')),
        success(Exactness::Empty),
    ))(input)
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Radix {
    Two = 2,
    Eight = 8,
    Ten = 10,
    Sixteen = 16,
}

fn radix<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&str, Radix, E> {
    map(
        opt(preceded(
            char('#'),
            alt((
                value(Radix::Two, char('b')),
                value(Radix::Eight, char('o')),
                value(Radix::Sixteen, char('x')),
                value(Radix::Ten, char('d')),
            )),
        )),
        |r| r.unwrap_or(Radix::Ten),
    )
    .parse(input)
}

#[cfg(test)]
mod tests {
    use nom::error::VerboseError;

    use super::*;

    #[test]
    fn test_prefix() {
        use Exactness::*;
        use Radix::*;

        fn case(input: &'static str, r: Radix, e: Exactness) -> Case {
            Case {
                input,
                prefix: Prefix {
                    radix: r,
                    exactness: e,
                },
            }
        }

        struct Case {
            input: &'static str,
            prefix: Prefix,
        }

        let cases = [
            case("#be", Two, Exact),
            case("#bi", Two, Inexact),
            case("#b", Two, Empty),
            case("#oe", Eight, Exact),
            case("#oi", Eight, Inexact),
            case("#o", Eight, Empty),
            case("e", Ten, Exact),
            case("#de", Ten, Exact),
            case("i", Ten, Inexact),
            case("", Ten, Empty),
            case("#xe", Sixteen, Exact),
            case("#xi", Sixteen, Inexact),
            case("#x", Sixteen, Empty),
        ];

        for case in cases {
            assert_eq!(
                prefix::<VerboseError<&str>>(case.input),
                Ok(("", case.prefix)),
                "input: {}",
                case.input
            );
        }
    }

    #[test]
    fn test_imag() {
        struct Case {
            input: &'static str,
            num: Num,
        }
        fn case(
            input: &'static str,
            exactness: Exactness,
            real: Option<Real>,
            imag: Option<Real>,
        ) -> Case {
            Case {
                input,
                num: Num {
                    exactness,
                    complex: Complex { real, imag },
                },
            }
        }
        let cases = [
            case(
                "+2.0i",
                Exactness::Empty,
                None,
                Some(Real::Positive(Ureal::Decimal(2.0))),
            ),
            case(
                "1 +2i",
                Exactness::Empty,
                Some(Real::Positive(Ureal::Uinteger(1))),
                Some(Real::Positive(Ureal::Uinteger(2))),
            ),
            case(
                "1.0 + 2i",
                Exactness::Empty,
                Some(Real::Positive(Ureal::Decimal(1.0))),
                Some(Real::Positive(Ureal::Uinteger(2))),
            ),
            case(
                "1+ 2.0i",
                Exactness::Empty,
                Some(Real::Positive(Ureal::Uinteger(1))),
                Some(Real::Positive(Ureal::Decimal(2.0))),
            ),
            case(
                "1.0  + 2.0i",
                Exactness::Empty,
                Some(Real::Positive(Ureal::Decimal(1.0))),
                Some(Real::Positive(Ureal::Decimal(2.0))),
            ),
            case(
                "-1.0  + 2.0i",
                Exactness::Empty,
                Some(Real::Negative(Ureal::Decimal(1.0))),
                Some(Real::Positive(Ureal::Decimal(2.0))),
            ),
        ];

        for case in cases {
            assert_eq!(
                num::<VerboseError<&str>>(case.input),
                Ok(("", case.num)),
                "parsing {}",
                case.input
            )
        }
    }

    #[test]
    fn test_decimal10() {
        struct Case {
            input: &'static str,
            decimal: Decimal10,
        }

        fn case(input: &'static str, decimal: Decimal10) -> Case {
            Case { input, decimal }
        }

        let cases = [
            case("10e1", "10e1".parse::<f64>().unwrap()),
            case("10e-1", "10e-1".parse::<f64>().unwrap()),
            case("10e0", "10e0".parse::<f64>().unwrap()),
            case("10.0", 10.0),
            case("10.2e0", "10.2e0".parse::<f64>().unwrap()),
            case("10.2e3", "10.2e3".parse::<f64>().unwrap()),
            case("10.2e-3", "10.2e-3".parse::<f64>().unwrap()),
        ];

        for case in cases {
            assert_eq!(
                decimal10::<VerboseError<&str>>(case.input),
                Ok(("", case.decimal)),
                "parsing {}",
                case.input
            )
        }
    }
}
