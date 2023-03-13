use super::{
    expression, identifier, s_expression, whitespace_delimited, Expression, Identifier, OneOrMore,
};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, multispace0, multispace1},
    combinator::{cut, map, map_res, opt},
    error::{context, VerboseError},
    multi::{many0, many1},
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    IResult,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Definition<'a> {
    Variable(VariableDefinition<'a>),
    DefineSyntax(Keyword, TransformerExpression<'a>),
    Begin(Vec<Definition<'a>>),
    LetSyntax(Vec<SyntaxBinding<'a>>, Vec<Definition<'a>>),
    LetRecSyntax(Vec<SyntaxBinding<'a>>, Vec<Definition<'a>>),
}

pub(super) fn definition(input: &str) -> IResult<&str, Definition, VerboseError<&str>> {
    let begin = context(
        "begin",
        preceded(
            terminated(tag("begin"), multispace0),
            many0(whitespace_delimited(definition)),
        ),
    );
    let define_syntax = context(
        "define-syntax",
        preceded(
            terminated(tag("define-syntax"), multispace0),
            cut(separated_pair(keyword, multispace0, transformer_expression)),
        ),
    );

    fn let_syntax_form<'a>(
        name: &'static str,
    ) -> impl FnMut(
        &'a str,
    ) -> IResult<
        &str,
        (Vec<SyntaxBinding<'a>>, Vec<Definition>),
        VerboseError<&'a str>,
    > {
        context(
            name,
            preceded(
                terminated(tag(name), multispace0),
                separated_pair(
                    s_expression(many0(whitespace_delimited(syntax_binding))),
                    multispace0,
                    many0(whitespace_delimited(definition)),
                ),
            ),
        )
    }

    let let_syntax = let_syntax_form("let-syntax");
    let letrec_syntax = let_syntax_form("letrec-syntax");

    context(
        "definition",
        s_expression(alt((
            map(variable_definition, Definition::Variable),
            map(begin, Definition::Begin),
            map(define_syntax, |(keyword, expression)| {
                Definition::DefineSyntax(keyword, expression)
            }),
            map(let_syntax, |(bindings, definitions)| {
                Definition::LetSyntax(bindings, definitions)
            }),
            map(letrec_syntax, |(bindings, definitions)| {
                Definition::LetRecSyntax(bindings, definitions)
            }),
        ))),
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

fn keyword(input: &str) -> IResult<&str, Keyword, VerboseError<&str>> {
    context("keyword", identifier)(input)
}

pub type TransformerExpression<'a> = Expression<'a>;

fn transformer_expression(input: &str) -> IResult<&str, TransformerExpression, VerboseError<&str>> {
    context("transformer expression", expression)(input)
}

#[derive(Debug, Clone, PartialEq)]
pub struct SyntaxBinding<'a> {
    pub keyword: Keyword,
    pub transformer: TransformerExpression<'a>,
}

// (<keyword> <transformer expression>)
fn syntax_binding(input: &str) -> IResult<&str, SyntaxBinding, VerboseError<&str>> {
    context(
        "syntax binding",
        map(
            s_expression(separated_pair(keyword, multispace0, transformer_expression)),
            |(keyword, transformer)| SyntaxBinding {
                keyword,
                transformer,
            },
        ),
    )(input)
}
