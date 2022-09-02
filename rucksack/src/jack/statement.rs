use crate::parse::*;

use super::{
    expression::{Expression, SubroutineCall},
    id::Id,
    keyword::Keyword,
    sym::Sym,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Let(Id, Box<Option<Expression>>, Box<Expression>),
    If(Box<Expression>, Box<Statements>, Box<Option<Statements>>),
    While(Box<Expression>, Box<Statements>),
    Do(Box<SubroutineCall>),
    Return(Box<Option<Expression>>),
}
impl Statement {
    pub fn let_parser<'a>(input: &'a str) -> ParseResult<'a, Self> {
        right(
            left(Keyword::Let, comspace()),
            map(
                pair(
                    left(move |input| Id::parse(input), ok(comspace())),
                    pair(
                        ok(right(
                            left(Sym::LSquare, ok(comspace())),
                            left(
                                left(move |input| Expression::parse(input), ok(comspace())),
                                left(Sym::RSquare, ok(comspace())),
                            ),
                        )),
                        left(
                            right(
                                left(Sym::Equals, ok(comspace())),
                                left(move |input| Expression::parse(input), ok(comspace())),
                            ),
                            Sym::Semi,
                        ),
                    ),
                ),
                |(var_name, (var_sub, value))| {
                    Self::Let(var_name, Box::new(var_sub), Box::new(value))
                },
            ),
        )
        .parse(input)
    }

    pub fn if_parser<'a>(input: &'a str) -> ParseResult<'a, Self> {
        right(
            left(Keyword::If, ok(comspace())),
            map(
                pair(
                    right(
                        left(Sym::LRound, ok(comspace())),
                        left(
                            left(move |input| Expression::parse(input), ok(comspace())),
                            left(Sym::RRound, ok(comspace())),
                        ),
                    ),
                    pair(
                        right(
                            left(Sym::LCurly, ok(comspace())),
                            left(
                                left(move |input| Statements::parse(input), ok(comspace())),
                                left(Sym::RCurly, ok(comspace())),
                            ),
                        ),
                        ok(right(
                            left(
                                left(Keyword::Else, ok(comspace())),
                                left(Sym::LCurly, ok(comspace())),
                            ),
                            left(
                                left(move |input| Statements::parse(input), ok(comspace())),
                                Sym::RCurly,
                            ),
                        )),
                    ),
                ),
                |(condition, (stmts, estmts))| {
                    Self::If(Box::new(condition), Box::new(stmts), Box::new(estmts))
                },
            ),
        )
        .parse(input)
    }

    pub fn while_parser<'a>(input: &'a str) -> ParseResult<'a, Self> {
        right(
            left(Keyword::While, ok(comspace())),
            map(
                pair(
                    right(
                        left(Sym::LRound, ok(comspace())),
                        left(
                            left(move |input| Expression::parse(input), ok(comspace())),
                            left(Sym::RRound, ok(comspace())),
                        ),
                    ),
                    right(
                        left(Sym::LCurly, ok(comspace())),
                        left(
                            left(move |input| Statements::parse(input), ok(comspace())),
                            left(Sym::RCurly, ok(comspace())),
                        ),
                    ),
                ),
                |(condition, stmts)| Self::While(Box::new(condition), Box::new(stmts)),
            ),
        )
        .parse(input)
    }

    pub fn do_parser<'a>(input: &'a str) -> ParseResult<'a, Self> {
        right(
            left(Keyword::Do, comspace()),
            left(
                left(
                    map(
                        move |input| SubroutineCall::parse(input),
                        |call| Self::Do(Box::new(call)),
                    ),
                    ok(comspace()),
                ),
                Sym::Semi,
            ),
        )
        .parse(input)
    }

    pub fn return_parser<'a>(input: &'a str) -> ParseResult<'a, Self> {
        right(
            Keyword::Return,
            left(
                left(
                    map(
                        ok(right(comspace(), move |input| Expression::parse(input))),
                        |expr| Self::Return(Box::new(expr)),
                    ),
                    ok(comspace()),
                ),
                Sym::Semi,
            ),
        )
        .parse(input)
    }
}
impl Parses<Statement> for Statement {
    fn parse<'a>(input: &'a str) -> ParseResult<'a, Statement> {
        or_else(
            move |input| Self::let_parser(input),
            or_else(
                move |input| Self::if_parser(input),
                or_else(
                    move |input| Self::while_parser(input),
                    or_else(
                        move |input| Self::do_parser(input),
                        move |input| Self::return_parser(input),
                    ),
                ),
            ),
        )
        .parse(input)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Statements {
    stmts: Vec<Statement>,
}
impl Statements {
    pub fn new(stmts: Vec<Statement>) -> Self {
        Self { stmts }
    }
}
impl Parses<Statements> for Statements {
    fn parse<'a>(input: &'a str) -> ParseResult<'a, Statements> {
        map(
            range(
                left(move |input| Statement::parse(input), ok(comspace())),
                0..,
            ),
            |stmts| Self::new(stmts),
        )
        .parse(input)
    }
}
