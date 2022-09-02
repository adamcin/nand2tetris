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
impl Parseable for Op {
    fn parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        Box::new(or_else(
            map(
                right(
                    left(unbox(Sym::Plus.matcher(ctx)), ok(comspace())),
                    unbox(Term::parser(ctx)),
                ),
                |term| Op::Plus(term),
            ),
            or_else(
                map(
                    right(
                        left(unbox(Sym::Minus.matcher(ctx)), ok(comspace())),
                        unbox(Term::parser(ctx)),
                    ),
                    |term| Op::Minus(term),
                ),
                or_else(
                    map(
                        right(
                            left(unbox(Sym::Splat.matcher(ctx)), ok(comspace())),
                            unbox(Term::parser(ctx)),
                        ),
                        |term| Op::Splat(term),
                    ),
                    or_else(
                        map(
                            right(
                                left(unbox(Sym::Slash.matcher(ctx)), ok(comspace())),
                                unbox(Term::parser(ctx)),
                            ),
                            |term| Op::Div(term),
                        ),
                        or_else(
                            map(
                                right(
                                    left(unbox(Sym::Amp.matcher(ctx)), ok(comspace())),
                                    unbox(Term::parser(ctx)),
                                ),
                                |term| Op::And(term),
                            ),
                            or_else(
                                map(
                                    right(
                                        left(unbox(Sym::Pipe.matcher(ctx)), ok(comspace())),
                                        unbox(Term::parser(ctx)),
                                    ),
                                    |term| Op::Or(term),
                                ),
                                or_else(
                                    map(
                                        right(
                                            left(unbox(Sym::Equals.matcher(ctx)), ok(comspace())),
                                            unbox(Term::parser(ctx)),
                                        ),
                                        |term| Op::Eq(term),
                                    ),
                                    or_else(
                                        map(
                                            right(
                                                left(
                                                    unbox(Sym::LAngle.matcher(ctx)),
                                                    ok(comspace()),
                                                ),
                                                unbox(Term::parser(ctx)),
                                            ),
                                            |term| Op::Lt(term),
                                        ),
                                        map(
                                            right(
                                                left(
                                                    unbox(Sym::RAngle.matcher(ctx)),
                                                    ok(comspace()),
                                                ),
                                                unbox(Term::parser(ctx)),
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
        ))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Neg(Term),
    Tilde(Term),
}
impl Parseable for UnaryOp {
    fn parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        Box::new(or_else(
            map(
                right(
                    left(unbox(Sym::Minus.matcher(ctx)), ok(comspace())),
                    unbox(Term::parser(ctx)),
                ),
                |term| UnaryOp::Neg(term),
            ),
            map(
                right(
                    left(unbox(Sym::Tilde.matcher(ctx)), ok(comspace())),
                    unbox(Term::parser(ctx)),
                ),
                |term| UnaryOp::Tilde(term),
            ),
        ))
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
    pub fn int_parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        println!("Term::int_parser");
        Box::new(map(unbox(IntConst::parser(ctx)), |value| {
            Self::IntConst(value)
        }))
    }

    pub fn string_parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        println!("Term::string_parser");
        Box::new(map(unbox(StringConst::parser(ctx)), |value| {
            Self::StringConst(value)
        }))
    }

    pub fn keyword_parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        println!("Term::keyword_parser");
        Box::new(map(unbox(KeywordConst::parser(ctx)), |value| {
            Self::KeywordConst(value)
        }))
    }

    pub fn var_name_parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        println!("Term::var_name_parser");
        Box::new(map(unbox(Id::parser(ctx)), |value| Self::VarName(value)))
    }

    pub fn var_sub_parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        println!("Term::var_sub_parser");
        Box::new(map(
            pair(
                left(unbox(Id::parser(ctx)), ok(comspace())),
                right(
                    left(unbox(Sym::LSquare.matcher(ctx)), ok(comspace())),
                    left(
                        left(unbox(Expression::parser(ctx)), ok(comspace())),
                        unbox(Sym::RSquare.matcher(ctx)),
                    ),
                ),
            ),
            |(id, expr)| Self::VarSub(id, Box::new(expr)),
        ))
    }

    pub fn expr_parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        println!("Term::expr_parser");
        Box::new(map(
            right(
                left(unbox(Sym::LRound.matcher(ctx)), ok(comspace())),
                left(
                    left(unbox(Expression::parser(ctx)), ok(comspace())),
                    unbox(Sym::RRound.matcher(ctx)),
                ),
            ),
            |expr| Self::Expr(Box::new(expr)),
        ))
    }

    pub fn unary_op_parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        println!("Term::unary_op_parser");
        Box::new(map(unbox(UnaryOp::parser(ctx)), |value| {
            Self::UnaryOp(Box::new(value))
        }))
    }

    pub fn subroutine_call_parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        println!("Term::subroutine_call_parser");
        Box::new(map(unbox(SubroutineCall::parser(ctx)), |value| {
            Self::SubroutineCall(Box::new(value))
        }))
    }
}
impl Parseable for Term {
    fn parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        println!("Term::parser");
        Box::new(or_else(
            unbox(Self::expr_parser(ctx)),
            or_else(
                unbox(Self::string_parser(ctx)),
                or_else(
                    unbox(Self::unary_op_parser(ctx)),
                    or_else(
                        unbox(Self::int_parser(ctx)),
                        or_else(
                            unbox(Self::subroutine_call_parser(ctx)),
                            or_else(
                                unbox(Self::var_sub_parser(ctx)),
                                or_else(
                                    unbox(Self::var_name_parser(ctx)),
                                    unbox(Self::keyword_parser(ctx)),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ))
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
impl Parseable for Expression {
    fn parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        Box::new(map(
            pair(
                left(unbox(Term::parser(ctx)), ok(comspace())),
                range(left(unbox(Op::parser(ctx)), ok(comspace())), 0..),
            ),
            |(term, ops)| Expression::new(term, ops),
        ))
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
impl Parseable for ExpressionList {
    fn parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        Box::new(map(
            ok(map(
                pair(
                    left(unbox(Expression::parser(ctx)), ok(comspace())),
                    range(
                        right(
                            left(unbox(Sym::Comma.matcher(ctx)), ok(comspace())),
                            left(unbox(Expression::parser(ctx)), ok(comspace())),
                        ),
                        0..,
                    ),
                ),
                |(first, exprs)| vec![vec![first], exprs].concat(),
            )),
            |exprs| ExpressionList::new(exprs.unwrap_or(Vec::new())),
        ))
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
impl Parseable for SubroutineCall {
    fn parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        Box::new(map(
            pair(
                ok(left(
                    left(unbox(Id::parser(ctx)), ok(comspace())),
                    left(unbox(Sym::Dot.matcher(ctx)), ok(comspace())),
                )),
                pair(
                    left(unbox(Id::parser(ctx)), ok(comspace())),
                    right(
                        left(unbox(Sym::LRound.matcher(ctx)), ok(comspace())),
                        left(
                            left(unbox(ExpressionList::parser(ctx)), ok(comspace())),
                            unbox(Sym::RRound.matcher(ctx)),
                        ),
                    ),
                ),
            ),
            |(qualifier, (name, exprs))| SubroutineCall::new(qualifier, name, exprs),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::*;

    #[test]
    fn term_int_parser() {
        let ctx = Ctx {};
        let parser = Term::int_parser(&ctx);

        assert_eq!(
            Ok(("", Term::IntConst(IntConst::new(12345).expect("valid")))),
            parser.parse("12345")
        )
    }

    #[test]
    fn term_string_parser() {
        let ctx = Ctx {};
        let parser = Term::string_parser(&ctx);

        assert_eq!(
            Ok((
                "",
                Term::StringConst(StringConst::new("a string".to_owned()))
            )),
            parser.parse("\"a string\"")
        )
    }

    #[test]
    fn term_expr_parser() {
        let ctx = Ctx {};
        let parser = Term::expr_parser(&ctx);

        assert_eq!(
            Ok((
                "",
                Term::Expr(Box::new(Expression::new(
                    Term::StringConst(StringConst::new("a string".to_owned())),
                    Vec::new()
                )))
            )),
            parser.parse("(\"a string\")")
        );

        // assert_eq!(
        //     Ok(("", Term::Expr(Box::new(Expression::new(Term::KeywordConst(KeywordConst::True), Vec::new()))))),
        //     parser.parse("(true)")
        // );
    }

    #[test]
    fn subroutine_call_parser() {
        let ctx = Ctx {};
        let parser = SubroutineCall::parser(&ctx);

        assert_eq!(
            Ok((
                "",
                SubroutineCall::new(
                    None,
                    Id::new("a_function".to_owned()),
                    ExpressionList::new(Vec::new())
                )
            )),
            parser.parse("a_function()")
        );

        // assert_eq!(
        //     Ok(("", Term::Expr(Box::new(Expression::new(Term::KeywordConst(KeywordConst::True), Vec::new()))))),
        //     parser.parse("(true)")
        // );
    }

    #[test]
    fn term_call_parser() {
        let ctx = Ctx {};
        let parser = Term::subroutine_call_parser(&ctx);

        assert_eq!(
            Ok((
                "",
                Term::SubroutineCall(Box::new(SubroutineCall::new(
                    None,
                    Id::new("a_function".to_owned()),
                    ExpressionList::new(Vec::new())
                )))
            )),
            parser.parse("a_function()")
        );

        // assert_eq!(
        //     Ok(("", Term::Expr(Box::new(Expression::new(Term::KeywordConst(KeywordConst::True), Vec::new()))))),
        //     parser.parse("(true)")
        // );
    }

    #[test]
    fn unary_ops() {
        let ctx = Ctx {};
        let parser = UnaryOp::parser(&ctx);

        // assert_eq!(
        //     Ok((
        //         "",
        //         UnaryOp::Neg(Term::IntConst(IntConst::new(12345).expect("valid")))
        //     )),
        //     parser.parse("-12345")
        // )
    }
}
