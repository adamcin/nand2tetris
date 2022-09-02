use crate::parse::*;

pub enum Keyword {
    Class,
    Constructor,
    Function,
    Method,
    Field,
    Static,
    Var,
    Int,
    Char,
    Boolean,
    Void,
    True,
    False,
    Null,
    This,
    Let,
    Do,
    If,
    Else,
    While,
    Return,
}
impl Keyword {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Class => "class",
            Self::Constructor => "constructor",
            Self::Function => "function",
            Self::Method => "method",
            Self::Field => "field",
            Self::Static => "static",
            Self::Var => "var",
            Self::Int => "int",
            Self::Char => "char",
            Self::Boolean => "boolean",
            Self::Void => "void",
            Self::True => "true",
            Self::False => "false",
            Self::Null => "null",
            Self::This => "this",
            Self::Let => "let",
            Self::Do => "do",
            Self::If => "if",
            Self::Else => "else",
            Self::While => "while",
            Self::Return => "return",
        }
    }

    pub fn all() -> Vec<Self> {
        use Keyword::*;
        vec![
            Class,
            Constructor,
            Function,
            Method,
            Field,
            Static,
            Var,
            Int,
            Char,
            Boolean,
            Void,
            True,
            False,
            Null,
            This,
            Let,
            Do,
            If,
            Else,
            While,
            Return,
        ]
    }

    pub fn is_reserved(token: &str) -> bool {
        Self::all().iter().any(|k| k.as_str() == token)
    }
}

impl Matchable for Keyword {
    fn matcher<'a>(&self, _ctx: &'a Ctx) -> Box<dyn Parser<'a, ()> + 'a> {
        Box::new(match_literal(self.as_str()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn keywords() {
        let ctx = Ctx {};

        assert_eq!(Ok(("", ())), Keyword::Boolean.matcher(&ctx).parse("boolean"));
        
    }
}