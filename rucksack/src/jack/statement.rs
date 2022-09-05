use crate::parse::*;

use super::{
    expression::{Expression, KeywordConst, SubroutineCall, Term},
    id::Id,
    keyword::Keyword,
    sym::Sym,
    token::Token,
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
    pub fn new_let_var(var_name: Id, expr: Expression) -> Self {
        Self::Let(var_name, Box::new(None), Box::new(expr))
    }

    pub fn new_let_var_sub(var_name: Id, sub_expr: Expression, expr: Expression) -> Self {
        Self::Let(var_name, Box::new(Some(sub_expr)), Box::new(expr))
    }

    pub fn new_if(condition: Expression, block: Statements) -> Self {
        Self::If(Box::new(condition), Box::new(block), Box::new(None))
    }

    pub fn new_if_else(condition: Expression, block: Statements, else_block: Statements) -> Self {
        Self::If(
            Box::new(condition),
            Box::new(block),
            Box::new(Some(else_block)),
        )
    }

    pub fn new_while(condition: Expression, block: Statements) -> Self {
        Self::While(Box::new(condition), Box::new(block))
    }

    pub fn new_do(call: SubroutineCall) -> Self {
        Self::Do(Box::new(call))
    }

    pub fn new_return_void() -> Self {
        Self::Return(Box::new(None))
    }

    pub fn new_return_this() -> Self {
        Self::Return(Box::new(Some(Expression::new(
            Term::KeywordConst(KeywordConst::This),
            Vec::new(),
        ))))
    }

    pub fn new_return(value: Expression) -> Self {
        Self::Return(Box::new(Some(value)))
    }

    pub fn let_parser<'a>(input: &'a [Token]) -> ParseResult<'a, &'a [Token], Self> {
        right(
            Keyword::Let,
            map(
                pair(
                    move |input| Token::id(input),
                    pair(
                        ok(right(
                            Sym::LSquare,
                            left(move |input| Expression::parse_into(input), Sym::RSquare),
                        )),
                        left(
                            right(Sym::Equals, move |input| Expression::parse_into(input)),
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

    pub fn if_parser<'a>(input: &'a [Token]) -> ParseResult<'a, &'a [Token], Self> {
        right(
            Keyword::If,
            map(
                pair(
                    right(
                        Sym::LRound,
                        left(move |input| Expression::parse_into(input), Sym::RRound),
                    ),
                    pair(
                        right(
                            Sym::LCurly,
                            left(move |input| Statements::parse_into(input), Sym::RCurly),
                        ),
                        ok(right(
                            left(Keyword::Else, Sym::LCurly),
                            left(move |input| Statements::parse_into(input), Sym::RCurly),
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

    pub fn while_parser<'a>(input: &'a [Token]) -> ParseResult<'a, &'a [Token], Self> {
        right(
            Keyword::While,
            map(
                pair(
                    right(
                        Sym::LRound,
                        left(move |input| Expression::parse_into(input), Sym::RRound),
                    ),
                    right(
                        Sym::LCurly,
                        left(move |input| Statements::parse_into(input), Sym::RCurly),
                    ),
                ),
                |(condition, stmts)| Self::While(Box::new(condition), Box::new(stmts)),
            ),
        )
        .parse(input)
    }

    pub fn do_parser<'a>(input: &'a [Token]) -> ParseResult<'a, &'a [Token], Self> {
        right(
            Keyword::Do,
            left(
                map(
                    move |input| SubroutineCall::parse_into(input),
                    |call| Self::Do(Box::new(call)),
                ),
                Sym::Semi,
            ),
        )
        .parse(input)
    }

    pub fn return_parser<'a>(input: &'a [Token]) -> ParseResult<'a, &'a [Token], Self> {
        right(
            Keyword::Return,
            left(
                map(ok(move |input| Expression::parse_into(input)), |expr| {
                    Self::Return(Box::new(expr))
                }),
                Sym::Semi,
            ),
        )
        .parse(input)
    }
}
impl<'a> Parses<'a> for Statement {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
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
impl<'a> Parses<'a> for Statements {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        map(
            range(move |input| Statement::parse_into(input), 0..),
            |stmts| Self::new(stmts),
        )
        .parse(input)
    }
}
impl From<Vec<Statement>> for Statements {
    fn from(items: Vec<Statement>) -> Self {
        Statements::new(items)
    }
}

impl From<Statement> for Statements {
    fn from(item: Statement) -> Self {
        Statements::new(vec![item])
    }
}
#[cfg(test)]
mod tests {
    use crate::jack::{
        common::testutil::{assert_tokens, transform_result},
        expression::{Op, SubroutineCall, Term},
        statement::Statement,
        token::IntConst,
    };

    use super::*;

    fn token_result(tokens: &[Token]) -> Result<Statement, Option<Token>> {
        let parser = Statement::parse_into;
        transform_result(parser.parse(tokens))
    }

    #[test]
    fn parsing() {
        let cases = vec![(
            r"
            while(index < values.size()) {
                let total = total + values.get(index);
                let index = index + 1;
            }
            ",
            Ok(Statement::new_while(
                (
                    Id::from("index").into(),
                    Op::Lt(SubroutineCall::new_qual(Id::from("values"), Id::from("size")).into()),
                )
                    .into(),
                vec![
                    Statement::new_let_var(
                        Id::from("total"),
                        (
                            Id::from("total").into(),
                            Op::Plus(
                                SubroutineCall::new_qual_params(
                                    Id::from("values"),
                                    Id::from("get"),
                                    vec![Id::from("index").into()].into(),
                                )
                                .into(),
                            ),
                        )
                            .into(),
                    ),
                    Statement::new_let_var(
                        Id::from("index"),
                        (Id::from("index").into(), Op::Plus(IntConst::one().into())).into(),
                    ),
                ]
                .into(),
            )),
        )];
        assert_tokens(cases, token_result);
    }
}
