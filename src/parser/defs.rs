use super::{expression, identifier, whitespace_delimited, Expression, Identifier, OneOrMore};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, multispace1},
    combinator::{cut, map, map_res, opt},
    error::{context, VerboseError},
    multi::{many0, many1},
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    IResult,
};

pub struct Program<'a>(pub Vec<Form<'a>>);

pub fn program(input: &str) -> IResult<&str, Program, VerboseError<&str>> {
    context("program", map(many0(whitespace_delimited(form)), Program))(input)
}

pub enum Form<'a> {
    Definition(Definition<'a>),
    Expression(Expression<'a>),
}

fn form(input: &str) -> IResult<&str, Form, VerboseError<&str>> {
    context(
        "form",
        alt((
            map(definition, Form::Definition),
            map(expression, Form::Expression),
        )),
    )(input)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Definition<'a> {
    Variable(VariableDefinition<'a>),
    Syntax,
    Begin(Vec<Definition<'a>>),
    LetSyntax(Vec<SyntaxBinding>, Vec<Definition<'a>>),
    LetRecSyntax(Vec<SyntaxBinding>, Vec<Definition<'a>>),
}

fn definition(input: &str) -> IResult<&str, Definition, VerboseError<&str>> {
    let begin = context(
        "begin definition",
        preceded(terminated(tag("begin"), multispace1), many0(definition)),
    );
    context(
        "definition",
        delimited(
            context("definition begin", char('(')),
            whitespace_delimited(alt((
                map(variable_definition, Definition::Variable),
                map(begin, Definition::Begin),
            ))),
            context("definition begin", cut(char(')'))),
        ),
    )(input)
}

#[derive(Debug, Clone, PartialEq)]
pub enum VariableDefinition<'a> {
    SimpleDefine {
        var: Variable,
        expr: Expression<'a>,
    },
    Function {
        name: Variable,
        args: Vec<Variable>,
        rest: Option<Variable>,
        body: Body<'a>,
    },
}

fn variable_definition(input: &str) -> IResult<&str, VariableDefinition, VerboseError<&str>> {
    let function_def = map(
        tuple((
            delimited(
                context("function signature begin", char('(')),
                tuple((
                    whitespace_delimited(variable),
                    many0(whitespace_delimited(variable)),
                    opt(preceded(char('.'), cut(whitespace_delimited(variable)))),
                )),
                context("function signature end", cut(char(')'))),
            ),
            whitespace_delimited(body),
        )),
        |((name, args, rest), body)| VariableDefinition::Function {
            name,
            args,
            rest,
            body,
        },
    );

    context(
        "variable definition",
        preceded(
            terminated(tag("define"), multispace1),
            alt((
                map(
                    separated_pair(variable, multispace1, expression),
                    |(var, expr)| VariableDefinition::SimpleDefine { var, expr },
                ),
                context("function definition", function_def),
            )),
        ),
    )(input)
}

pub type Variable = Identifier;

pub(super) fn variable(input: &str) -> IResult<&str, Variable, VerboseError<&str>> {
    context("variable", identifier)(input)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Body<'a>(pub Vec<Definition<'a>>, pub OneOrMore<Expression<'a>>);

pub(super) fn body(input: &str) -> IResult<&str, Body, VerboseError<&str>> {
    context(
        "body",
        map_res(
            tuple((
                many0(whitespace_delimited(definition)),
                many1(whitespace_delimited(expression)),
            )),
            |(def, expr)| Ok::<_, ()>(Body(def, expr.try_into()?)),
        ),
    )(input)
}

pub type Keyword = Identifier;

#[derive(Debug, Clone, PartialEq)]
pub struct SyntaxBinding {
    pub keyword: Keyword,
}
