use crate::parse::*;

use super::{
    atoms::{IntConst, KeywordConst, StringConst},
    id::Id,
    sym::Sym,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Op {
    Plus(Term),
    Minus(Term),
    Splat(Term),
    Div(Term),
    And(Term),
    Or(Term),
    Eq(Term),
    Lt(Term),
    Gt(Term),
}
impl Parses<Op> for Op {
    fn parse<'a>(input: &'a str) -> ParseResult<'a, Op> {
        or_else(
            map(
                right(left(Sym::Plus, ok(comspace())), move |input| {
                    Term::parse(input)
                }),
                |term| Op::Plus(term),
            ),
            or_else(
                map(
                    right(left(Sym::Minus, ok(comspace())), move |input| {
                        Term::parse(input)
                    }),
                    |term| Op::Minus(term),
                ),
                or_else(
                    map(
                        right(left(Sym::Splat, ok(comspace())), move |input| {
                            Term::parse(input)
                        }),
                        |term| Op::Splat(term),
                    ),
                    or_else(
                        map(
                            right(left(Sym::Slash, ok(comspace())), move |input| {
                                Term::parse(input)
                            }),
                            |term| Op::Div(term),
                        ),
                        or_else(
                            map(
                                right(left(Sym::Amp, ok(comspace())), move |input| {
                                    Term::parse(input)
                                }),
                                |term| Op::And(term),
                            ),
                            or_else(
                                map(
                                    right(left(Sym::Pipe, ok(comspace())), move |input| {
                                        Term::parse(input)
                                    }),
                                    |term| Op::Or(term),
                                ),
                                or_else(
                                    map(
                                        right(left(Sym::Equals, ok(comspace())), move |input| {
                                            Term::parse(input)
                                        }),
                                        |term| Op::Eq(term),
                                    ),
                                    or_else(
                                        map(
                                            right(
                                                left(Sym::LAngle, ok(comspace())),
                                                move |input| Term::parse(input),
                                            ),
                                            |term| Op::Lt(term),
                                        ),
                                        map(
                                            right(
                                                left(Sym::RAngle, ok(comspace())),
                                                move |input| Term::parse(input),
                                            ),
                                            |term| Op::Gt(term),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        )
        .parse(input)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Neg(Term),
    Tilde(Term),
}
impl Parses<UnaryOp> for UnaryOp {
    fn parse<'a>(input: &'a str) -> ParseResult<'a, UnaryOp> {
        or_else(
            map(
                right(left(Sym::Minus, ok(comspace())), move |input| {
                    Term::parse(input)
                }),
                |term| UnaryOp::Neg(term),
            ),
            map(
                right(left(Sym::Tilde, ok(comspace())), move |input| {
                    Term::parse(input)
                }),
                |term| UnaryOp::Tilde(term),
            ),
        )
        .parse(input)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    IntConst(IntConst),
    StringConst(StringConst),
    KeywordConst(KeywordConst),
    VarName(Id),
    VarSub(Id, Box<Expression>),
    Expr(Box<Expression>),
    UnaryOp(Box<UnaryOp>),
    SubroutineCall(Box<SubroutineCall>),
}
impl Term {
    pub fn int_parser<'a>(input: &'a str) -> ParseResult<'a, Self> {
        println!("Term::int_parser");
        map(
            move |input| IntConst::parse(input),
            |value| Self::IntConst(value),
        )
        .parse(input)
    }

    pub fn string_parser<'a>(input: &'a str) -> ParseResult<'a, Self> {
        println!("Term::string_parser");
        map(
            move |input| StringConst::parse(input),
            |value| Self::StringConst(value),
        )
        .parse(input)
    }

    pub fn keyword_parser<'a>(input: &'a str) -> ParseResult<'a, Self> {
        println!("Term::keyword_parser");
        map(
            move |input| KeywordConst::parse(input),
            |value| Self::KeywordConst(value),
        )
        .parse(input)
    }

    pub fn var_name_parser<'a>(input: &'a str) -> ParseResult<'a, Self> {
        println!("Term::var_name_parser");
        map(move |input| Id::parse(input), |value| Self::VarName(value)).parse(input)
    }

    pub fn var_sub_parser<'a>(input: &'a str) -> ParseResult<'a, Self> {
        println!("Term::var_sub_parser");
        map(
            pair(
                left(move |input| Id::parse(input), ok(comspace())),
                right(
                    left(Sym::LSquare, ok(comspace())),
                    left(
                        left(move |input| Expression::parse(input), ok(comspace())),
                        Sym::RSquare,
                    ),
                ),
            ),
            |(id, expr)| Self::VarSub(id, Box::new(expr)),
        )
        .parse(input)
    }

    pub fn expr_parser<'a>(input: &'a str) -> ParseResult<'a, Self> {
        println!("Term::expr_parser");
        map(
            right(
                left(Sym::LRound, ok(comspace())),
                left(
                    left(move |input| Expression::parse(input), ok(comspace())),
                    Sym::RRound,
                ),
            ),
            |expr| Self::Expr(Box::new(expr)),
        )
        .parse(input)
    }

    pub fn unary_op_parser<'a>(input: &'a str) -> ParseResult<'a, Self> {
        println!("Term::unary_op_parser");
        map(
            move |input| UnaryOp::parse(input),
            |value| Self::UnaryOp(Box::new(value)),
        )
        .parse(input)
    }

    pub fn subroutine_call_parser<'a>(input: &'a str) -> ParseResult<'a, Self> {
        println!("Term::subroutine_call_parser");
        map(
            move |input| SubroutineCall::parse(input),
            |value| Self::SubroutineCall(Box::new(value)),
        )
        .parse(input)
    }
}
impl Parses<Term> for Term {
    fn parse<'a>(input: &'a str) -> ParseResult<'a, Term> {
        or_else(
            move |input| Self::expr_parser(input),
            or_else(
                move |input| Self::string_parser(input),
                or_else(
                    move |input| Self::unary_op_parser(input),
                    or_else(
                        move |input| Self::int_parser(input),
                        or_else(
                            move |input| Self::subroutine_call_parser(input),
                            or_else(
                                move |input| Self::var_sub_parser(input),
                                or_else(
                                    move |input| Self::var_name_parser(input),
                                    move |input| Self::keyword_parser(input),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        )
        .parse(input)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expression {
    term: Term,
    ops: Vec<Op>,
}
impl Expression {
    pub fn new(term: Term, ops: Vec<Op>) -> Self {
        Self { term, ops }
    }
}
impl Parses<Expression> for Expression {
    fn parse<'a>(input: &'a str) -> ParseResult<'a, Expression> {
        map(
            pair(
                left(move |input| Term::parse(input), ok(comspace())),
                range(left(move |input| Op::parse(input), ok(comspace())), 0..),
            ),
            |(term, ops)| Expression::new(term, ops),
        )
        .parse(input)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpressionList {
    exprs: Vec<Expression>,
}
impl ExpressionList {
    pub fn new(exprs: Vec<Expression>) -> Self {
        Self { exprs }
    }
}
impl Parses<ExpressionList> for ExpressionList {
    fn parse<'a>(input: &'a str) -> ParseResult<'a, ExpressionList> {
        map(
            ok(map(
                pair(
                    left(move |input| Expression::parse(input), ok(comspace())),
                    range(
                        right(
                            left(Sym::Comma, ok(comspace())),
                            left(move |input| Expression::parse(input), ok(comspace())),
                        ),
                        0..,
                    ),
                ),
                |(first, exprs)| vec![vec![first], exprs].concat(),
            )),
            |exprs| ExpressionList::new(exprs.unwrap_or(Vec::new())),
        )
        .parse(input)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SubroutineCall {
    qualifier: Option<Id>,
    name: Id,
    exprs: ExpressionList,
}
impl SubroutineCall {
    pub fn new(qualifier: Option<Id>, name: Id, exprs: ExpressionList) -> Self {
        Self {
            qualifier,
            name,
            exprs,
        }
    }
}
impl Parses<SubroutineCall> for SubroutineCall {
    fn parse<'a>(input: &'a str) -> ParseResult<'a, SubroutineCall> {
        map(
            pair(
                ok(left(
                    left(move |input| Id::parse(input), ok(comspace())),
                    left(Sym::Dot, ok(comspace())),
                )),
                pair(
                    left(move |input| Id::parse(input), ok(comspace())),
                    right(
                        left(Sym::LRound, ok(comspace())),
                        left(
                            left(move |input| ExpressionList::parse(input), ok(comspace())),
                            Sym::RRound,
                        ),
                    ),
                ),
            ),
            |(qualifier, (name, exprs))| SubroutineCall::new(qualifier, name, exprs),
        )
        .parse(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::*;

    #[test]
    fn term_int_parser() {
        assert_eq!(
            Ok(("", Term::IntConst(IntConst::new(12345).expect("valid")))),
            Term::int_parser("12345")
        )
    }

    #[test]
    fn term_string_parser() {
        assert_eq!(
            Ok((
                "",
                Term::StringConst(StringConst::new("a string".to_owned()))
            )),
            Term::string_parser("\"a string\"")
        )
    }

    #[test]
    fn term_expr_parser() {
        assert_eq!(
            Ok((
                "",
                Term::Expr(Box::new(Expression::new(
                    Term::StringConst(StringConst::new("a string".to_owned())),
                    Vec::new()
                )))
            )),
            Term::expr_parser("(\"a string\")")
        );

        // assert_eq!(
        //     Ok(("", Term::Expr(Box::new(Expression::new(Term::KeywordConst(KeywordConst::True), Vec::new()))))),
        //     parser.parse("(true)")
        // );
    }

    #[test]
    fn subroutine_call_parser() {
        assert_eq!(
            Ok((
                "",
                SubroutineCall::new(
                    None,
                    Id::new("a_function".to_owned()),
                    ExpressionList::new(Vec::new())
                )
            )),
            SubroutineCall::parse("a_function()")
        );

        // assert_eq!(
        //     Ok(("", Term::Expr(Box::new(Expression::new(Term::KeywordConst(KeywordConst::True), Vec::new()))))),
        //     parser.parse("(true)")
        // );
    }

    #[test]
    fn term_call_parser() {

        assert_eq!(
            Ok((
                "",
                Term::SubroutineCall(Box::new(SubroutineCall::new(
                    None,
                    Id::new("a_function".to_owned()),
                    ExpressionList::new(Vec::new())
                )))
            )),
            Term::subroutine_call_parser("a_function()")
        );

        // assert_eq!(
        //     Ok(("", Term::Expr(Box::new(Expression::new(Term::KeywordConst(KeywordConst::True), Vec::new()))))),
        //     parser.parse("(true)")
        // );
    }

    #[test]
    fn unary_ops() {
        let parser = move |input| UnaryOp::parse(input);

        // assert_eq!(
        //     Ok((
        //         "",
        //         UnaryOp::Neg(Term::IntConst(IntConst::new(12345).expect("valid")))
        //     )),
        //     parser.parse("-12345")
        // )
    }
}
