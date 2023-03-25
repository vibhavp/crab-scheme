use super::whitespace_delimited;
use nom::{
    branch::{alt, permutation},
    character::complete::{char, digit0, digit1},
    combinator::{cut, map, map_res, opt, recognize, success, value},
    error::{context, ContextError, FromExternalError, ParseError},
    multi::many0_count,
    sequence::{pair, preceded, separated_pair, terminated, tuple},
    AsChar, IResult, InputIter, InputLength, InputTakeAtPosition, Offset, Slice,
};
use std::{
    num::{ParseFloatError, ParseIntError},
    ops::{RangeFrom, RangeTo},
    str::FromStr,
};

pub trait ParseToRadix<T, E> {
    fn parse_to_radix(&self, radix: Radix) -> Result<T, E>;
}

impl ParseToRadix<usize, ParseIntError> for &str {
    fn parse_to_radix(&self, radix: Radix) -> Result<usize, ParseIntError> {
        usize::from_str_radix(self, radix as u32)
    }
}

pub trait ParseToResult<T, E> {
    fn parse_to_res(&self) -> Result<T, E>;
}

impl<T: FromStr> ParseToResult<T, T::Err> for &str {
    fn parse_to_res(&self) -> Result<T, T::Err> {
        T::from_str(self)
    }
}

pub trait ParseToNumber:
    Slice<RangeFrom<usize>>
    + Slice<RangeTo<usize>>
    + Offset
    + InputIter
    + InputLength
    + InputTakeAtPosition
    + ParseToResult<f64, ParseFloatError>
    + ParseToRadix<usize, ParseIntError>
    + ParseToResult<u32, ParseIntError>
    + Clone
{
}

impl<T> ParseToNumber for T where
    T: Slice<RangeFrom<usize>>
        + Slice<RangeTo<usize>>
        + Offset
        + InputIter
        + InputLength
        + InputTakeAtPosition
        + ParseToResult<f64, ParseFloatError>
        + ParseToRadix<usize, ParseIntError>
        + ParseToResult<u32, ParseIntError>
        + Clone
{
}

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

fn prefix<I, E: ParseError<I> + ContextError<I>>(input: I) -> IResult<I, Prefix, E>
where
    I: Slice<RangeFrom<usize>> + InputIter + Clone,
    <I as InputIter>::Item: AsChar,
{
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
    I,
    E: ParseError<I>
        + FromExternalError<I, ParseIntError>
        + FromExternalError<I, ParseFloatError>
        + ContextError<I>,
>(
    input: I,
) -> IResult<I, Num, E>
where
    I: ParseToNumber,
    <I as InputIter>::Item: AsChar + Clone,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
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
    I,
    E: ParseError<I> + FromExternalError<I, ParseIntError> + FromExternalError<I, ParseFloatError>,
>(
    radix: Radix,
) -> impl FnMut(I) -> IResult<I, Complex, E>
where
    I: ParseToNumber,
    <I as InputIter>::Item: AsChar,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
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
    I,
    E: ParseError<I> + FromExternalError<I, ParseIntError> + FromExternalError<I, ParseFloatError>,
>(
    radix: Radix,
) -> impl FnMut(I) -> IResult<I, Ureal, E>
where
    I: ParseToNumber,
    <I as InputIter>::Item: AsChar,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
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
    I,
    E: ParseError<I> + FromExternalError<I, ParseIntError> + FromExternalError<I, ParseFloatError>,
>(
    radix: Radix,
) -> impl FnMut(I) -> IResult<I, Real, E>
where
    I: ParseToNumber,
    <I as InputIter>::Item: AsChar,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
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
    I,
    E: ParseError<I> + FromExternalError<I, ParseIntError> + FromExternalError<I, ParseFloatError>,
>(
    radix: Radix,
) -> impl FnMut(I) -> IResult<I, Ureal, E>
where
    I: ParseToNumber,
    <I as InputIter>::Item: AsChar,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
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

fn uinteger10<I, E: ParseError<I> + FromExternalError<I, ParseIntError>>(
    input: I,
) -> IResult<I, usize, E>
where
    I: Slice<RangeFrom<usize>>
        + Offset
        + Slice<RangeTo<usize>>
        + InputIter
        + InputLength
        + ParseToRadix<usize, ParseIntError>
        + InputTakeAtPosition
        + Clone,
    <I as InputIter>::Item: AsChar,
    <I as InputTakeAtPosition>::Item: AsChar,
{
    uinteger(Radix::Ten)(input)
}

pub type Uinteger = usize;

fn uinteger<I, E: ParseError<I> + FromExternalError<I, ParseIntError>>(
    radix: Radix,
) -> impl FnMut(I) -> IResult<I, Uinteger, E>
where
    I: Slice<RangeFrom<usize>>
        + Slice<RangeTo<usize>>
        + Offset
        + InputIter
        + InputLength
        + ParseToRadix<usize, ParseIntError>
        + InputTakeAtPosition
        + Clone,
    <I as InputIter>::Item: AsChar,
    <I as InputTakeAtPosition>::Item: AsChar,
{
    map_res(
        pair(
            recognize(match radix {
                Radix::Eight => digit1::<I, _>,
                Radix::Sixteen => digit1,
                Radix::Ten => digit1,
                Radix::Two => digit1,
            }),
            many0_count(char('#')),
        ),
        move |(digits, zeros)| {
            digits
                .parse_to_radix(radix)
                .map(|u| u * (10_usize).pow(zeros.try_into().unwrap()))
        },
    )
}

pub type Decimal10 = f64;

fn sign<I, E: ParseError<I>>(input: I) -> IResult<I, i32, E>
where
    I: Slice<RangeFrom<usize>> + InputIter + Clone,
    <I as InputIter>::Item: AsChar,
{
    alt((value(1, char('+')), value(-1, char('-')), success(1)))(input)
}

fn decimal10<
    I,
    E: ParseError<I> + FromExternalError<I, ParseIntError> + FromExternalError<I, ParseFloatError>,
>(
    input: I,
) -> IResult<I, Decimal10, E>
where
    I: Slice<RangeFrom<usize>>
        + Slice<RangeTo<usize>>
        + Offset
        + InputIter
        + InputLength
        + InputTakeAtPosition
        + ParseToResult<f64, ParseFloatError>
        + ParseToRadix<usize, ParseIntError>
        + ParseToResult<u32, ParseIntError>
        + Clone,
    <I as InputIter>::Item: AsChar,
    <I as InputTakeAtPosition>::Item: AsChar,
{
    alt((
        map_res(
            tuple((recognize(uinteger10::<I, _>), exponent)),
            |(u, e)| {
                let mut f = String::from_iter(u.iter_elements().map(AsChar::as_char));
                f.push('e');
                f.push_str(&e.to_string());
                f.parse::<f64>()
            },
        ),
        map_res(
            tuple((
                alt((
                    recognize(tuple((char::<I, _>('.'), digit1))),
                    recognize(tuple((digit1, char('.'), digit0))),
                )),
                opt(recognize(tuple((
                    preceded(
                        alt((
                            char::<I, _>('e'),
                            char('s'),
                            char('f'),
                            char('d'),
                            char('l'),
                        )),
                        sign,
                    ),
                    digit1,
                )))),
            )),
            |(base, exp)| {
                let mut f = String::from_iter(base.iter_elements().map(AsChar::as_char));
                if let Some(e) = exp {
                    f.extend(e.iter_elements().map(|c| match c.as_char() {
                        'e' | 's' | 'f' | 'd' | 'l' => 'e',
                        c => c,
                    }));
                }
                f.parse::<f64>()
            },
        ),
    ))(input)
}

fn exponent<I, E: ParseError<I> + FromExternalError<I, ParseIntError>>(
    input: I,
) -> IResult<I, i32, E>
where
    I: Slice<RangeFrom<usize>>
        + InputIter
        + Clone
        + InputTakeAtPosition
        + ParseToResult<u32, ParseIntError>,
    <I as InputIter>::Item: AsChar,
    <I as InputTakeAtPosition>::Item: AsChar,
{
    map_res(
        tuple((
            preceded(
                alt((
                    char::<I, _>('e'),
                    char('s'),
                    char('f'),
                    char('d'),
                    char('l'),
                )),
                sign,
            ),
            cut(digit1),
        )),
        |(sign, digits)| digits.parse_to_res().map(|u| (u as i32 * sign)),
    )(input)
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Exactness {
    Empty,
    Inexact,
    Exact,
}

fn exactness<I, E: ParseError<I>>(input: I) -> IResult<I, Exactness, E>
where
    I: Slice<RangeFrom<usize>> + InputIter + Clone,
    <I as InputIter>::Item: AsChar,
{
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

fn radix<I, E: ParseError<I>>(input: I) -> IResult<I, Radix, E>
where
    I: Slice<RangeFrom<usize>> + InputIter + Clone,
    <I as InputIter>::Item: AsChar,
{
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
    )(input)
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
                prefix::<_, VerboseError<&str>>(case.input),
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
                "-2.0i",
                Exactness::Empty,
                None,
                Some(Real::Negative(Ureal::Decimal(2.0))),
            ),
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
                num::<_, VerboseError<&str>>(case.input),
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
                decimal10::<_, VerboseError<&str>>(case.input),
                Ok(("", case.decimal)),
                "parsing {}",
                case.input
            )
        }
    }
}
