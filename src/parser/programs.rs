use nom::{
    branch::alt, combinator::map, error::context, multi::many0, AsChar, IResult, InputIter,
    InputTakeAtPosition,
};

use super::{
    definition, expression, whitespace_delimited, DatumParseError, Definition, Expression,
    ParseToDatum,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Program<'a>(pub Vec<Form<'a>>);

pub fn program<'a, I, E: DatumParseError<I>>(input: I) -> IResult<I, Program<'a>, E>
where
    I: ParseToDatum<'a>,
    <I as InputIter>::Item: AsChar + Clone + Copy,
    <I as InputTakeAtPosition>::Item: AsChar + Clone + Copy,
{
    context("program", map(many0(whitespace_delimited(form)), Program))(input)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Form<'a> {
    Definition(Definition<'a>),
    Expression(Expression<'a>),
}

fn form<'a, I, E: DatumParseError<I>>(input: I) -> IResult<I, Form<'a>, E>
where
    I: ParseToDatum<'a>,
    <I as InputIter>::Item: AsChar + Clone + Copy,
    <I as InputTakeAtPosition>::Item: AsChar + Clone + Copy,
{
    context(
        "form",
        alt((
            map(definition, Form::Definition),
            map(expression, Form::Expression),
        )),
    )(input)
}
