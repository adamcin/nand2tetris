use crate::parse::*;

use super::{
    id::Id,
    keyword::Keyword,
    sym::Sym,
    token::{IntConst, StringConst, Token},
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
impl<'a> Parses<'a> for Op {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        or_else(
            map(
                right(Sym::Plus, move |input| Term::parse_into(input)),
                |term| Self::Plus(term),
            ),
            or_else(
                map(
                    right(Sym::Minus, move |input| Term::parse_into(input)),
                    |term| Self::Minus(term),
                ),
                or_else(
                    map(
                        right(Sym::Splat, move |input| Term::parse_into(input)),
                        |term| Self::Splat(term),
                    ),
                    or_else(
                        map(
                            right(Sym::Slash, move |input| Term::parse_into(input)),
                            |term| Self::Div(term),
                        ),
                        or_else(
                            map(
                                right(Sym::Amp, move |input| Term::parse_into(input)),
                                |term| Self::And(term),
                            ),
                            or_else(
                                map(
                                    right(Sym::Pipe, move |input| Term::parse_into(input)),
                                    |term| Self::Or(term),
                                ),
                                or_else(
                                    map(
                                        right(Sym::Equals, move |input| Term::parse_into(input)),
                                        |term| Self::Eq(term),
                                    ),
                                    or_else(
                                        map(
                                            right(Sym::LAngle, move |input| {
                                                Term::parse_into(input)
                                            }),
                                            |term| Self::Lt(term),
                                        ),
                                        map(
                                            right(Sym::RAngle, move |input| {
                                                Term::parse_into(input)
                                            }),
                                            |term| Self::Gt(term),
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
impl<'a> Parses<'a> for UnaryOp {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        or_else(
            map(
                right(Sym::Minus, move |input| Term::parse_into(input)),
                |term| Self::Neg(term),
            ),
            map(
                right(Sym::Tilde, move |input| Term::parse_into(input)),
                |term| Self::Tilde(term),
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
    pub fn int_parser<'a>(input: &'a [Token]) -> ParseResult<'a, &'a [Token], Self> {
        map(
            move |input| Token::int(input),
            |value| Self::IntConst(value),
        )
        .parse(input)
    }

    pub fn string_parser<'a>(input: &'a [Token]) -> ParseResult<'a, &'a [Token], Self> {
        map(
            move |input| Token::str(input),
            |value| Self::StringConst(value),
        )
        .parse(input)
    }

    pub fn keyword_parser<'a>(input: &'a [Token]) -> ParseResult<'a, &'a [Token], Self> {
        map(
            move |input| KeywordConst::parse_into(input),
            |value| Self::KeywordConst(value),
        )
        .parse(input)
    }

    pub fn var_name_parser<'a>(input: &'a [Token]) -> ParseResult<'a, &'a [Token], Self> {
        map(move |input| Token::id(input), |value| Self::VarName(value)).parse(input)
    }

    pub fn var_sub_parser<'a>(input: &'a [Token]) -> ParseResult<'a, &'a [Token], Self> {
        map(
            pair(
                move |input| Token::id(input),
                right(
                    Sym::LSquare,
                    left(move |input| Expression::parse_into(input), Sym::RSquare),
                ),
            ),
            |(id, expr)| Self::VarSub(id, Box::new(expr)),
        )
        .parse(input)
    }

    pub fn expr_parser<'a>(input: &'a [Token]) -> ParseResult<'a, &'a [Token], Self> {
        map(
            right(
                Sym::LRound,
                left(move |input| Expression::parse_into(input), Sym::RRound),
            ),
            |expr| Self::Expr(Box::new(expr)),
        )
        .parse(input)
    }

    pub fn unary_op_parser<'a>(input: &'a [Token]) -> ParseResult<'a, &'a [Token], Self> {
        map(
            move |input| UnaryOp::parse_into(input),
            |value| Self::UnaryOp(Box::new(value)),
        )
        .parse(input)
    }

    pub fn subroutine_call_parser<'a>(input: &'a [Token]) -> ParseResult<'a, &'a [Token], Self> {
        map(
            move |input| {
                let result = SubroutineCall::parse_into(input);
                result
            },
            |value| Self::SubroutineCall(Box::new(value)),
        )
        .parse(input)
    }
}

impl<'a> Parses<'a> for Term {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
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

impl From<KeywordConst> for Term {
    fn from(item: KeywordConst) -> Self {
        Term::KeywordConst(item)
    }
}

impl From<Id> for Term {
    fn from(item: Id) -> Self {
        Term::VarName(item)
    }
}

impl From<IntConst> for Term {
    fn from(item: IntConst) -> Self {
        Term::IntConst(item)
    }
}

impl From<StringConst> for Term {
    fn from(item: StringConst) -> Self {
        Term::StringConst(item)
    }
}

impl From<UnaryOp> for Term {
    fn from(item: UnaryOp) -> Self {
        Term::UnaryOp(Box::new(item))
    }
}

impl From<SubroutineCall> for Term {
    fn from(item: SubroutineCall) -> Self {
        Term::SubroutineCall(Box::new(item))
    }
}

impl<T> From<(Id, T)> for Term
where
    T: Into<Expression>,
{
    fn from(item: (Id, T)) -> Self {
        let (name, sub) = item;
        Term::VarSub(name, Box::new(sub.into()))
    }
}

impl From<Expression> for Term {
    fn from(item: Expression) -> Self {
        Term::Expr(Box::new(item))
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
impl<'a> Parses<'a> for Expression {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        map(
            pair(
                move |input| Term::parse_into(input),
                range(move |input| Op::parse_into(input), 0..),
            ),
            |(term, ops)| Self::new(term, ops),
        )
        .parse(input)
    }
}

impl From<Term> for Expression {
    fn from(item: Term) -> Self {
        Expression::new(item, Vec::new())
    }
}

impl From<(Term, Op)> for Expression {
    fn from(item: (Term, Op)) -> Self {
        let (t, op) = item;
        Expression::new(t, vec![op])
    }
}

impl From<(Term, Vec<Op>)> for Expression {
    fn from(item: (Term, Vec<Op>)) -> Self {
        let (t, ops) = item;
        Expression::new(t, ops)
    }
}

impl From<(Term, Op, Op)> for Expression {
    fn from(item: (Term, Op, Op)) -> Self {
        let (t, op0, op1) = item;
        Expression::new(t, vec![op0, op1])
    }
}

impl From<(Term, Op, Op, Op)> for Expression {
    fn from(item: (Term, Op, Op, Op)) -> Self {
        let (t, op0, op1, op2) = item;
        Expression::new(t, vec![op0, op1, op2])
    }
}

impl From<Id> for Expression {
    fn from(item: Id) -> Self {
        Expression::new(item.into(), Vec::new())
    }
}

impl From<IntConst> for Expression {
    fn from(item: IntConst) -> Self {
        Expression::new(item.into(), Vec::new())
    }
}

impl From<StringConst> for Expression {
    fn from(item: StringConst) -> Self {
        Expression::new(item.into(), Vec::new())
    }
}

impl From<KeywordConst> for Expression {
    fn from(item: KeywordConst) -> Self {
        Expression::new(item.into(), Vec::new())
    }
}

impl From<UnaryOp> for Expression {
    fn from(item: UnaryOp) -> Self {
        Expression::new(item.into(), Vec::new())
    }
}

impl From<SubroutineCall> for Expression {
    fn from(item: SubroutineCall) -> Self {
        Expression::new(item.into(), Vec::new())
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
impl<'a> Parses<'a> for ExpressionList {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        map(
            ok(map(
                pair(
                    move |input| Expression::parse_into(input),
                    range(
                        right(Sym::Comma, move |input| Expression::parse_into(input)),
                        0..,
                    ),
                ),
                |(first, exprs)| vec![vec![first], exprs].concat(),
            )),
            |exprs| Self::new(exprs.unwrap_or(Vec::new())),
        )
        .parse(input)
    }
}

impl From<Vec<Expression>> for ExpressionList {
    fn from(items: Vec<Expression>) -> Self {
        ExpressionList::new(items)
    }
}

impl From<Expression> for ExpressionList {
    fn from(item: Expression) -> Self {
        ExpressionList::new(vec![item])
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

    pub fn new_simp(name: Id) -> Self {
        Self::new(None, name, Vec::new().into())
    }

    pub fn new_qual(qualifier: Id, name: Id) -> Self {
        Self::new(Some(qualifier), name, Vec::new().into())
    }

    pub fn new_simp_params(name: Id, exprs: ExpressionList) -> Self {
        Self::new(None, name, exprs)
    }

    pub fn new_qual_params(qualifier: Id, name: Id, exprs: ExpressionList) -> Self {
        Self::new(Some(qualifier), name, exprs)
    }
}

impl<'a> Parses<'a> for SubroutineCall {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        map(
            pair(
                ok(left(move |input| Token::id(input), Sym::Dot)),
                pair(
                    move |input| Token::id(input),
                    right(
                        Sym::LRound,
                        left(move |input| ExpressionList::parse_into(input), Sym::RRound),
                    ),
                ),
            ),
            |(qualifier, (name, exprs))| Self::new(qualifier, name, exprs),
        )
        .parse(input)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum KeywordConst {
    True,
    False,
    Null,
    This,
}
impl<'a> Parses<'a> for KeywordConst {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        or_else(
            map(Keyword::True, |_| Self::True),
            or_else(
                map(Keyword::False, |_| Self::False),
                or_else(
                    map(Keyword::Null, |_| Self::Null),
                    map(Keyword::This, |_| Self::This),
                ),
            ),
        )
        .parse(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        jack::{
            common::testutil::{assert_tokens, transform_result},
            token::TokenStream,
        },
        parse::*,
    };

    fn token_result(tokens: &[Token]) -> Result<Expression, Option<Token>> {
        let parser = move |input| Expression::parse_into(input);
        transform_result(parser.parse(tokens))
    }

    fn token_result_to_term(tokens: &[Token]) -> Result<Term, Option<Token>> {
        let parser = move |input| Term::parse_into(input);
        transform_result(parser.parse(tokens))
    }

    fn token_result_to_unary_op(tokens: &[Token]) -> Result<UnaryOp, Option<Token>> {
        let parser = move |input| UnaryOp::parse_into(input);
        transform_result(parser.parse(tokens))
    }

    #[test]
    fn term_int_parser() {
        let cases = vec![(
            "12345",
            Ok(Term::IntConst(IntConst::new(12345).expect("valid"))),
        )];
        assert_tokens(cases, token_result_to_term);
    }

    #[test]
    fn term_string_parser() {
        let cases = vec![(
            "\"a string\"",
            Ok(Term::StringConst(StringConst::new("a string".to_owned()))),
        )];
        assert_tokens(cases, token_result_to_term);
    }

    #[test]
    fn term_expr_parser() {
        let cases = vec![
            (
                "(\"a string\")",
                Ok(Term::Expr(Box::new(Expression::new(
                    Term::StringConst(StringConst::new("a string".to_owned())),
                    Vec::new(),
                )))),
            ),
            (
                "(true)",
                Ok(Term::Expr(Box::new(Expression::new(
                    Term::KeywordConst(KeywordConst::True),
                    Vec::new(),
                )))),
            ),
            (
                "((false))",
                Ok(Term::Expr(Box::new(Expression::new(
                    Term::Expr(Box::new(Expression::new(
                        Term::KeywordConst(KeywordConst::False),
                        Vec::new(),
                    ))),
                    Vec::new(),
                )))),
            ),
        ];
        assert_tokens(cases, token_result_to_term);
    }

    #[test]
    fn term_call_parser() {
        let cases = vec![(
            "a_function()",
            Ok(Term::SubroutineCall(Box::new(SubroutineCall::new(
                None,
                Id::new("a_function".to_owned()),
                ExpressionList::new(Vec::new()),
            )))),
        )];
        assert_tokens(cases, token_result_to_term);
    }

    #[test]
    fn unary_ops() {
        let cases = vec![(
            "-12345",
            Ok(UnaryOp::Neg(Term::IntConst(
                IntConst::new(12345).expect("valid"),
            ))),
        )];
        assert_tokens(cases, token_result_to_unary_op);
    }

    #[test]
    fn parsing() {
        let cases = vec![(
            r"index < values.size()",
            Ok((
                Id::from("index").into(),
                Op::Lt(SubroutineCall::new_qual(Id::from("values"), Id::from("size")).into()),
            )
                .into()),
        )];
        assert_tokens(cases, token_result);
    }
}
