use crate::parse::*;

use super::atoms::Type;
use super::id::Id;
use super::keyword::Keyword;
use super::statement::Statements;
use super::sym::Sym;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClassVarKind {
    Static,
    Field,
}
impl Parses<ClassVarKind> for ClassVarKind {
    fn parse<'a>(input: &'a str) -> ParseResult<'a, ClassVarKind> {
        or_else(
            map(Keyword::Static, |_| Self::Static),
            map(Keyword::Field, |_| Self::Field),
        )
        .parse(input)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassVarDec {
    var_kind: ClassVarKind,
    var_type: Type,
    var_names: Vec<Id>,
}
impl ClassVarDec {
    pub fn new(var_kind: ClassVarKind, var_type: Type, var_names: Vec<Id>) -> Self {
        Self {
            var_kind,
            var_type,
            var_names,
        }
    }
}
impl Parses<ClassVarDec> for ClassVarDec {
    fn parse<'a>(input: &'a str) -> ParseResult<'a, ClassVarDec> {
        map(
            pair(
                left(move |input| ClassVarKind::parse(input), comspace()),
                pair(
                    left(move |input| Type::parse(input), comspace()),
                    map(
                        pair(
                            left(move |input| Id::parse(input), ok(comspace())),
                            left(
                                range(
                                    right(
                                        left(Sym::Comma, ok(comspace())),
                                        left(move |input| Id::parse(input), ok(comspace())),
                                    ),
                                    0..,
                                ),
                                Sym::Semi,
                            ),
                        ),
                        |(id, ids)| -> Vec<Id> { vec![vec![id], ids].concat() },
                    ),
                ),
            ),
            |(var_kind, (var_type, var_names))| Self::new(var_kind, var_type, var_names),
        )
        .parse(input)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SubroutineKind {
    Constructor,
    Function,
    Method,
}
impl Parses<SubroutineKind> for SubroutineKind {
    fn parse<'a>(input: &'a str) -> ParseResult<'a, SubroutineKind> {
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
impl Parses<ReturnType> for ReturnType {
    fn parse<'a>(input: &'a str) -> ParseResult<'a, ReturnType> {
        or_else(
            map(Keyword::Void, |_| Self::Void),
            map(move |input| Type::parse(input), |t| Self::Returns(t)),
        )
        .parse(input)
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
impl Parses<SubroutineParameter> for SubroutineParameter {
    fn parse<'a>(input: &'a str) -> ParseResult<'a, SubroutineParameter> {
        map(
            pair(
                left(move |input| Type::parse(input), comspace()),
                move |input| Id::parse(input),
            ),
            |(var_type, var_name)| Self::new(var_type, var_name),
        )
        .parse(input)
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
impl Parses<ParameterList> for ParameterList {
    fn parse<'a>(input: &'a str) -> ParseResult<'a, ParameterList> {
        map(
            ok(pair(
                move |input| SubroutineParameter::parse(input),
                range(
                    right(
                        right(ok(comspace()), left(Sym::Comma, ok(comspace()))),
                        move |input| SubroutineParameter::parse(input),
                    ),
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
impl Parses<VarDec> for VarDec {
    fn parse<'a>(input: &'a str) -> ParseResult<'a, VarDec> {
        map(
            right(
                left(Keyword::Var, comspace()),
                pair(
                    left(move |input| Type::parse(input), comspace()),
                    left(
                        map(
                            pair(
                                left(move |input| Id::parse(input), ok(comspace())),
                                range(
                                    right(
                                        left(Sym::Comma, ok(comspace())),
                                        left(move |input| Id::parse(input), ok(comspace())),
                                    ),
                                    0..,
                                ),
                            ),
                            |(id, vars)| vec![vec![id], vars].concat(),
                        ),
                        Sym::Semi,
                    ),
                ),
            ),
            |(var_type, var_names)| VarDec::new(var_type, var_names),
        )
        .parse(input)
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
impl Parses<SubroutineBody> for SubroutineBody {
    fn parse<'a>(input: &'a str) -> ParseResult<'a, SubroutineBody> {
        right(
            left(Sym::LCurly, ok(comspace())),
            left(
                map(
                    pair(
                        range(left(move |input| VarDec::parse(input), ok(comspace())), 0..),
                        left(move |input| Statements::parse(input), ok(comspace())),
                    ),
                    |(var_decs, statements)| SubroutineBody::new(var_decs, statements),
                ),
                right(ok(comspace()), Sym::RCurly),
            ),
        )
        .parse(input)
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
impl Parses<SubroutineDec> for SubroutineDec {
    fn parse<'a>(input: &'a str) -> ParseResult<'a, SubroutineDec> {
        map(
            pair(
                left(move |input| SubroutineKind::parse(input), comspace()),
                pair(
                    left(move |input| ReturnType::parse(input), comspace()),
                    pair(
                        left(move |input| Id::parse(input), ok(comspace())),
                        pair(
                            right(
                                left(Sym::LRound, ok(comspace())),
                                left(
                                    left(move |input| ParameterList::parse(input), ok(comspace())),
                                    left(Sym::RRound, ok(comspace())),
                                ),
                            ),
                            left(move |input| SubroutineBody::parse(input), ok(comspace())),
                        ),
                    ),
                ),
            ),
            |(kind, (ret, (name, (params, body))))| {
                SubroutineDec::new(kind, ret, name, params, body)
            },
        )
        .parse(input)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Class {
    name: Id,
    vars: Vec<ClassVarDec>,
    subs: Vec<SubroutineDec>,
}

impl Class {
    fn new(name: Id, vars: Vec<ClassVarDec>, subs: Vec<SubroutineDec>) -> Self {
        Self { name, vars, subs }
    }
}
impl Parses<Class> for Class {
    fn parse<'a>(input: &'a str) -> ParseResult<'a, Class> {
        right(
            left(Keyword::Class, comspace()),
            map(
                pair(
                    left(move |input| Id::parse(input), ok(comspace())),
                    right(
                        left(Sym::LCurly, ok(comspace())),
                        left(
                            pair(
                                left(
                                    range(move |input| ClassVarDec::parse(input), 0..),
                                    ok(comspace()),
                                ),
                                left(
                                    range(move |input| SubroutineDec::parse(input), 0..),
                                    ok(comspace()),
                                ),
                            ),
                            Sym::RCurly,
                        ),
                    ),
                ),
                |(id, (vars, subs))| Self::new(id, vars, subs),
            ),
        )
        .parse(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_simple() {
        let parser = move |input| Class::parse(input);
        assert_eq!(
            Ok((
                "",
                Class::new(Id::new("MyClass".to_owned()), Vec::new(), Vec::new())
            )),
            parser.parse("class MyClass {}")
        );
        assert_eq!(
            Ok((
                " extra",
                Class::new(Id::new("MyClass".to_owned()), Vec::new(), Vec::new())
            )),
            parser.parse("class MyClass {} extra")
        );

        assert_eq!(
            Ok((
                " extra",
                Class::new(
                    Id::new("MyClass".to_owned()),
                    vec![ClassVarDec::new(
                        ClassVarKind::Static,
                        Type::Int,
                        vec![Id::new("count".to_owned())]
                    )],
                    Vec::new()
                )
            )),
            parser.parse(
                r"class MyClass {
                static int count// some inline comment
                /* 
                some multiline comment;
                 */
                ;
            } extra"
            )
        );
    }
}
