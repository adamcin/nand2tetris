use crate::parse::*;

use super::{
    expression::{Expression, SubroutineCall},
    id::Id,
    keyword::Keyword,
    sym::Sym,
};

pub enum Statement {
    Let(Id, Box<Option<Expression>>, Box<Expression>),
    If(Box<Expression>, Box<Statements>, Box<Option<Statements>>),
    While(Box<Expression>, Box<Statements>),
    Do(Box<SubroutineCall>),
    Return(Box<Option<Expression>>),
}
impl Statement {
    pub fn let_parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        Box::new(right(
            left(unbox(Keyword::Let.matcher(ctx)), comspace()),
            map(
                pair(
                    left(unbox(Id::parser(ctx)), ok(comspace())),
                    pair(
                        ok(right(
                            left(unbox(Sym::LSquare.matcher(ctx)), ok(comspace())),
                            left(
                                left(unbox(Expression::parser(ctx)), ok(comspace())),
                                left(unbox(Sym::RSquare.matcher(ctx)), ok(comspace())),
                            ),
                        )),
                        left(
                            right(
                                left(unbox(Sym::Equals.matcher(ctx)), ok(comspace())),
                                left(unbox(Expression::parser(ctx)), ok(comspace())),
                            ),
                            unbox(Sym::Semi.matcher(ctx)),
                        ),
                    ),
                ),
                |(var_name, (var_sub, value))| {
                    Self::Let(var_name, Box::new(var_sub), Box::new(value))
                },
            ),
        ))
    }

    pub fn if_parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        Box::new(right(
            left(unbox(Keyword::If.matcher(ctx)), ok(comspace())),
            map(
                pair(
                    right(
                        left(unbox(Sym::LRound.matcher(ctx)), ok(comspace())),
                        left(
                            left(unbox(Expression::parser(ctx)), ok(comspace())),
                            left(unbox(Sym::RRound.matcher(ctx)), ok(comspace())),
                        ),
                    ),
                    pair(
                        right(
                            left(unbox(Sym::LCurly.matcher(ctx)), ok(comspace())),
                            left(
                                left(unbox(Statements::parser(ctx)), ok(comspace())),
                                left(unbox(Sym::RCurly.matcher(ctx)), ok(comspace())),
                            ),
                        ),
                        ok(right(
                            left(
                                left(unbox(Keyword::Else.matcher(ctx)), ok(comspace())),
                                left(unbox(Sym::LCurly.matcher(ctx)), ok(comspace())),
                            ),
                            left(
                                left(unbox(Statements::parser(ctx)), ok(comspace())),
                                unbox(Sym::RCurly.matcher(ctx)),
                            ),
                        )),
                    ),
                ),
                |(condition, (stmts, estmts))| {
                    Self::If(Box::new(condition), Box::new(stmts), Box::new(estmts))
                },
            ),
        ))
    }

    pub fn while_parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        Box::new(right(
            left(unbox(Keyword::While.matcher(ctx)), ok(comspace())),
            map(
                pair(
                    right(
                        left(unbox(Sym::LRound.matcher(ctx)), ok(comspace())),
                        left(
                            left(unbox(Expression::parser(ctx)), ok(comspace())),
                            left(unbox(Sym::RRound.matcher(ctx)), ok(comspace())),
                        ),
                    ),
                    right(
                        left(unbox(Sym::LCurly.matcher(ctx)), ok(comspace())),
                        left(
                            left(unbox(Statements::parser(ctx)), ok(comspace())),
                            left(unbox(Sym::RCurly.matcher(ctx)), ok(comspace())),
                        ),
                    ),
                ),
                |(condition, stmts)| Self::While(Box::new(condition), Box::new(stmts)),
            ),
        ))
    }

    pub fn do_parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        Box::new(right(
            left(unbox(Keyword::Do.matcher(ctx)), comspace()),
            left(
                left(
                    map(unbox(SubroutineCall::parser(ctx)), |call| {
                        Self::Do(Box::new(call))
                    }),
                    ok(comspace()),
                ),
                unbox(Sym::Semi.matcher(ctx)),
            ),
        ))
    }

    pub fn return_parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        Box::new(right(
            unbox(Keyword::Return.matcher(ctx)),
            left(
                left(
                    map(
                        ok(right(comspace(), unbox(Expression::parser(ctx)))),
                        |expr| Self::Return(Box::new(expr)),
                    ),
                    ok(comspace()),
                ),
                unbox(Sym::Semi.matcher(ctx)),
            ),
        ))
    }
}
impl Parseable for Statement {
    fn parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        Box::new(or_else(
            unbox(Self::let_parser(ctx)),
            or_else(
                unbox(Self::if_parser(ctx)),
                or_else(
                    unbox(Self::while_parser(ctx)),
                    or_else(unbox(Self::do_parser(ctx)), unbox(Self::return_parser(ctx))),
                ),
            ),
        ))
    }
}

pub struct Statements {
    stmts: Vec<Statement>,
}
impl Statements {
    pub fn new(stmts: Vec<Statement>) -> Self {
        Self { stmts }
    }
}
impl Parseable for Statements {
    fn parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        Box::new(map(
            range(left(unbox(Statement::parser(ctx)), ok(comspace())), 0..),
            |stmts| Self::new(stmts),
        ))
    }
}
