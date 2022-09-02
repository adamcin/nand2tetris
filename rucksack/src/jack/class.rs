use crate::parse::*;

use super::atoms::Type;
use super::id::Id;
use super::keyword::Keyword;
use super::statement::Statements;
use super::sym::Sym;

pub enum ClassVarKind {
    Static,
    Field,
}
impl Parseable for ClassVarKind {
    fn parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        Box::new(or_else(
            map(unbox(Keyword::Static.matcher(ctx)), |()| Self::Static),
            map(unbox(Keyword::Field.matcher(ctx)), |()| Self::Field),
        ))
    }
}

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
impl Parseable for ClassVarDec {
    fn parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        Box::new(map(
            pair(
                left(unbox(ClassVarKind::parser(ctx)), comspace()),
                pair(
                    left(unbox(Type::parser(ctx)), comspace()),
                    map(
                        pair(
                            unbox(Id::parser(ctx)),
                            range(
                                right(
                                    left(left(comspace(), match_literal(",")), comspace()),
                                    unbox(Id::parser(ctx)),
                                ),
                                0..,
                            ),
                        ),
                        |(id, ids)| -> Vec<Id> { vec![vec![id], ids].concat() },
                    ),
                ),
            ),
            |(var_kind, (var_type, var_names))| Self::new(var_kind, var_type, var_names),
        ))
    }
}

pub enum SubroutineKind {
    Constructor,
    Function,
    Method,
}
impl Parseable for SubroutineKind {
    fn parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        Box::new(or_else(
            map(unbox(Keyword::Constructor.matcher(ctx)), |()| {
                Self::Constructor
            }),
            or_else(
                map(unbox(Keyword::Function.matcher(ctx)), |()| Self::Function),
                map(unbox(Keyword::Method.matcher(ctx)), |()| Self::Method),
            ),
        ))
    }
}

pub enum ReturnType {
    Void,
    Returns(Type),
}
impl Parseable for ReturnType {
    fn parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        Box::new(or_else(
            map(unbox(Keyword::Void.matcher(ctx)), |()| Self::Void),
            map(unbox(Type::parser(ctx)), |t| Self::Returns(t)),
        ))
    }
}

#[derive(Clone)]
pub struct SubroutineParameter {
    var_type: Type,
    var_name: Id,
}
impl SubroutineParameter {
    pub fn new(var_type: Type, var_name: Id) -> Self {
        Self { var_type, var_name }
    }
}
impl Parseable for SubroutineParameter {
    fn parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        Box::new(map(
            pair(
                left(unbox(Type::parser(ctx)), comspace()),
                left(unbox(Id::parser(ctx)), comspace()),
            ),
            |(var_type, var_name)| Self::new(var_type, var_name),
        ))
    }
}

pub struct ParameterList {
    vars: Vec<SubroutineParameter>,
}
impl ParameterList {
    pub fn new(vars: Vec<SubroutineParameter>) -> Self {
        Self { vars }
    }
}
impl Parseable for ParameterList {
    fn parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        Box::new(right(
            left(unbox(Sym::LRound.matcher(ctx)), ok(comspace())),
            left(
                map(
                    ok(pair(
                        unbox(SubroutineParameter::parser(ctx)),
                        until(
                            right(
                                ok(comspace()),
                                right(
                                    unbox(Sym::Comma.matcher(ctx)),
                                    right(ok(comspace()), unbox(SubroutineParameter::parser(ctx))),
                                ),
                            ),
                            0..,
                            Sym::RRound.as_str(),
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
                ),
                right(ok(comspace()), unbox(Sym::RRound.matcher(ctx))),
            ),
        ))
    }
}

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
impl Parseable for VarDec {
    fn parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        Box::new(map(
            right(
                unbox(Keyword::Var.matcher(ctx)),
                right(
                    comspace(),
                    pair(
                        left(unbox(Type::parser(ctx)), comspace()),
                        left(
                            map(
                                pair(
                                    unbox(Id::parser(ctx)),
                                    until(
                                        right(
                                            ok(comspace()),
                                            right(
                                                unbox(Sym::Comma.matcher(ctx)),
                                                right(ok(comspace()), unbox(Id::parser(ctx))),
                                            ),
                                        ),
                                        0..,
                                        Sym::Semi.as_str(),
                                    ),
                                ),
                                |(id, vars)| vec![vec![id], vars].concat(),
                            ),
                            right(ok(comspace()), unbox(Sym::Semi.matcher(ctx))),
                        ),
                    ),
                ),
            ),
            |(var_type, var_names)| VarDec::new(var_type, var_names),
        ))
    }
}

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
impl Parseable for SubroutineBody {
    fn parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        Box::new(right(
            left(unbox(Sym::LCurly.matcher(ctx)), ok(comspace())),
            left(
                map(
                    pair(
                        range(left(unbox(VarDec::parser(ctx)), ok(comspace())), 0..),
                        left(unbox(Statements::parser(ctx)), ok(comspace())),
                    ),
                    |(var_decs, statements)| SubroutineBody::new(var_decs, statements),
                ),
                right(ok(comspace()), unbox(Sym::RCurly.matcher(ctx))),
            ),
        ))
    }
}

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
impl Parseable for SubroutineDec {
    fn parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        Box::new(map(
            pair(
                left(unbox(SubroutineKind::parser(ctx)), comspace()),
                pair(
                    left(unbox(ReturnType::parser(ctx)), comspace()),
                    pair(
                        left(unbox(Id::parser(ctx)), ok(comspace())),
                        pair(
                            left(unbox(ParameterList::parser(ctx)), ok(comspace())),
                            left(unbox(SubroutineBody::parser(ctx)), ok(comspace())),
                        ),
                    ),
                ),
            ),
            |(kind, (ret, (name, (params, body))))| {
                SubroutineDec::new(kind, ret, name, params, body)
            },
        ))
    }
}

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

impl Parseable for Class {
    fn parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        use Keyword::Class;
        use Sym::{LCurly, RCurly};
        Box::new(right(
            left(unbox(Class.matcher(ctx)), comspace()),
            map(
                pair(
                    left(unbox(Id::parser(ctx)), ok(comspace())),
                    right(
                        left(unbox(LCurly.matcher(ctx)), ok(comspace())),
                        left(
                            pair(
                                left(range(unbox(ClassVarDec::parser(ctx)), 0..), ok(comspace())),
                                left(
                                    range(unbox(SubroutineDec::parser(ctx)), 0..),
                                    ok(comspace()),
                                ),
                            ),
                            unbox(RCurly.matcher(ctx)),
                        ),
                    ),
                ),
                |(id, (vars, subs))| Self::new(id, vars, subs),
            ),
        ))
    }
}
