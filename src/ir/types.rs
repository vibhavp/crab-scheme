#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Number(NumberType),
    Cons,
    Vector,
    Symbol,
    Character,
    String,
    Procedure,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NumberType {
    Number,
    Complex,
    Rational,
    Floating,
    Integer,
}
