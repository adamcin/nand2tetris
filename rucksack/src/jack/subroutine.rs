use crate::parse::*;

use super::id::Id;
use super::keyword::Keyword;
use super::statement::{Statement, Statements};
use super::sym::Sym;
use super::token::Token;
use super::typea::Type;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SubroutineKind {
    Constructor,
    Function,
    Method,
}
impl<'a> Parses<'a> for SubroutineKind {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        or_else(
            map(Keyword::Constructor, |_| Self::Constructor),
            or_else(
                map(Keyword::Function, |_| Self::Function),
                map(Keyword::Method, |_| Self::Method),
            ),
        )
        .parse(input)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReturnType {
    Void,
    Returns(Type),
}
impl<'a> Parses<'a> for ReturnType {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        or_else(
            map(Keyword::Void, |_| Self::Void),
            map(move |input| Type::parse_into(input), |t| Self::Returns(t)),
        )
        .parse(input)
    }
}

impl From<Type> for ReturnType {
    fn from(item: Type) -> Self {
        Self::Returns(item)
    }
}

impl From<Id> for ReturnType {
    fn from(item: Id) -> Self {
        Self::Returns(Type::ClassName(item))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SubroutineParameter {
    var_type: Type,
    var_name: Id,
}
impl SubroutineParameter {
    pub fn new(var_type: Type, var_name: Id) -> Self {
        Self { var_type, var_name }
    }
}
impl<'a> Parses<'a> for SubroutineParameter {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        map(
            pair(
                move |input| Type::parse_into(input),
                move |input| Token::id(input),
            ),
            |(var_type, var_name)| Self::new(var_type, var_name),
        )
        .parse(input)
    }
}

impl From<(Type, Id)> for SubroutineParameter {
    fn from(item: (Type, Id)) -> Self {
        let (t, id) = item;
        Self::new(t, id)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParameterList {
    vars: Vec<SubroutineParameter>,
}
impl ParameterList {
    pub fn new(vars: Vec<SubroutineParameter>) -> Self {
        Self { vars }
    }
}
impl<'a> Parses<'a> for ParameterList {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        map(
            ok(pair(
                move |input| SubroutineParameter::parse_into(input),
                range(
                    right(Sym::Comma, move |input| {
                        SubroutineParameter::parse_into(input)
                    }),
                    0..,
                ),
            )),
            |vars_o: Option<(SubroutineParameter, Vec<SubroutineParameter>)>| {
                Self::new(
                    vars_o
                        .map(|(var, vars)| -> Vec<SubroutineParameter> {
                            vec![vec![var], vars].concat()
                        })
                        .unwrap_or_else(Vec::new),
                )
            },
        )
        .parse(input)
    }
}

impl From<Vec<SubroutineParameter>> for ParameterList {
    fn from(items: Vec<SubroutineParameter>) -> Self {
        Self::new(items)
    }
}

impl From<SubroutineParameter> for ParameterList {
    fn from(item: SubroutineParameter) -> Self {
        Self::new(vec![item])
    }
}

impl From<(Type, Id)> for ParameterList {
    fn from(item: (Type, Id)) -> Self {
        Self::new(vec![item.into()])
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VarDec {
    var_type: Type,
    var_names: Vec<Id>,
}
impl VarDec {
    pub fn new(var_type: Type, var_names: Vec<Id>) -> Self {
        Self {
            var_type,
            var_names,
        }
    }
}
impl<'a> Parses<'a> for VarDec {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        map(
            right(
                Keyword::Var,
                pair(
                    move |input| Type::parse_into(input),
                    left(
                        map(
                            pair(
                                move |input| Token::id(input),
                                range(right(Sym::Comma, move |input| Token::id(input)), 0..),
                            ),
                            |(id, vars)| vec![vec![id], vars].concat(),
                        ),
                        Sym::Semi,
                    ),
                ),
            ),
            |(var_type, var_names)| Self::new(var_type, var_names),
        )
        .parse(input)
    }
}
impl From<(Type, Id)> for VarDec {
    fn from(item: (Type, Id)) -> Self {
        let (t, id) = item;
        Self::new(t, vec![id])
    }
}

impl From<(Type, Vec<Id>)> for VarDec {
    fn from(item: (Type, Vec<Id>)) -> Self {
        let (t, ids) = item;
        Self::new(t, ids)
    }
}

impl From<(Type, Id, Id)> for VarDec {
    fn from(item: (Type, Id, Id)) -> Self {
        let (t, id0, id1) = item;
        Self::new(t, vec![id0, id1])
    }
}

impl From<(Type, Id, Id, Id)> for VarDec {
    fn from(item: (Type, Id, Id, Id)) -> Self {
        let (t, id0, id1, id2) = item;
        Self::new(t, vec![id0, id1, id2])
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SubroutineBody {
    var_decs: Vec<VarDec>,
    statements: Statements,
}
impl SubroutineBody {
    pub fn new(var_decs: Vec<VarDec>, statements: Statements) -> Self {
        Self {
            var_decs,
            statements,
        }
    }
}
impl<'a> Parses<'a> for SubroutineBody {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        map(
            right(
                Sym::LCurly,
                left(
                    pair(
                        range(move |input| VarDec::parse_into(input), 0..),
                        move |input| Statements::parse_into(input),
                    ),
                    Sym::RCurly,
                ),
            ),
            |(var_decs, statements)| Self::new(var_decs, statements),
        )
        .parse(input)
    }
}

impl From<Statement> for SubroutineBody {
    fn from(item: Statement) -> Self {
        Self::new(Vec::new(), item.into())
    }
}

impl From<Statements> for SubroutineBody {
    fn from(item: Statements) -> Self {
        Self::new(Vec::new(), item)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SubroutineDec {
    kind: SubroutineKind,
    ret: ReturnType,
    name: Id,
    params: ParameterList,
    body: SubroutineBody,
}
impl SubroutineDec {
    pub fn new(
        kind: SubroutineKind,
        ret: ReturnType,
        name: Id,
        params: ParameterList,
        body: SubroutineBody,
    ) -> Self {
        Self {
            kind,
            ret,
            name,
            params,
            body,
        }
    }
}
impl<'a> Parses<'a> for SubroutineDec {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        map(
            pair(
                move |input| SubroutineKind::parse_into(input),
                pair(
                    move |input| ReturnType::parse_into(input),
                    pair(
                        move |input| Token::id(input),
                        pair(
                            right(
                                Sym::LRound,
                                left(move |input| ParameterList::parse_into(input), Sym::RRound),
                            ),
                            move |input| SubroutineBody::parse_into(input),
                        ),
                    ),
                ),
            ),
            |(kind, (ret, (name, (params, body))))| Self::new(kind, ret, name, params, body),
        )
        .parse(input)
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

    fn token_result(tokens: &[Token]) -> Result<SubroutineDec, Option<Token>> {
        let parser = SubroutineDec::parse_into;
        transform_result(parser.parse(tokens))
    }

    #[test]
    fn parsing() {
        let cases = vec![
            (
                r"
                constructor List new() {
                    return this;
                }
                ",
                Ok(SubroutineDec::new(
                    SubroutineKind::Constructor,
                    Id::from("List").into(),
                    Id::from("new"),
                    Vec::new().into(),
                    Statement::new_return_this().into(),
                )),
            ),
            (
                r"
                /**
                 * Computes sum of a list of integers.
                 */
                function int sum(List values) {
                    var int total, index;
                    // init total and index to 0
                    let total = 0;
                    let index = 0;

                    while(index < values.size()) {
                        let total = total + values.get(index);
                        let index = index + 1;
                    }
                    return total;
                }
                ",
                Ok(SubroutineDec::new(
                    SubroutineKind::Function,
                    Type::Int.into(),
                    Id::from("sum"),
                    (Id::from("List").into(), Id::from("values")).into(),
                    SubroutineBody::new(
                        vec![(Type::Int, Id::from("total"), Id::from("index")).into()],
                        vec![
                            Statement::new_let_var(Id::from("total"), IntConst::zero().into()),
                            Statement::new_let_var(Id::from("index"), IntConst::zero().into()),
                            Statement::new_while(
                                (
                                    Id::from("index").into(),
                                    Op::Lt(
                                        SubroutineCall::new_qual(
                                            Id::from("values"),
                                            Id::from("size"),
                                        )
                                        .into(),
                                    ),
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
                                        (
                                            Id::from("index").into(),
                                            Op::Plus(IntConst::one().into()),
                                        )
                                            .into(),
                                    ),
                                ]
                                .into(),
                            ),
                            Statement::new_return(Id::from("total").into()),
                        ]
                        .into(),
                    ),
                )),
            ),
        ];
        assert_tokens(cases, token_result);
    }
}
