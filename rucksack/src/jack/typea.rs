use crate::parse::*;

use super::{id::Id, keyword::Keyword, token::Token};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Int,
    Char,
    Boolean,
    ClassName(Id),
}
impl<'a> Parses<'a> for Type {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        or_else(
            map(Keyword::Int, |_| Self::Int),
            or_else(
                map(Keyword::Char, |_| Self::Char),
                or_else(
                    map(Keyword::Boolean, |_| Self::Boolean),
                    map(move |input| Token::id(input), |id| Self::ClassName(id)),
                ),
            ),
        )
        .parse(input)
    }
}
impl From<Id> for Type {
    fn from(item: Id) -> Self {
        Self::ClassName(item)
    }
}

#[cfg(test)]
mod tests {
    use crate::jack::{
        common::testutil::{assert_tokens, transform_result},
        token::IntConst,
    };

    use super::*;

    fn token_result_to_type(tokens: &[Token]) -> Result<Type, Option<Token>> {
        let parser = Type::parse_into;
        transform_result(parser.parse(tokens))
    }

    #[test]
    fn types() {
        let cases = vec![
            ("int", Ok(Type::Int)),
            ("char", Ok(Type::Char)),
            ("boolean", Ok(Type::Boolean)),
            ("MyClass", Ok(Type::ClassName(Id::from("MyClass")))),
            ("12345", Err(Some(IntConst::new(12345).expect("").into()))),
            ("1Class", Err(Some(IntConst::new(1).expect("").into()))),
        ];
        assert_tokens(cases, token_result_to_type);
    }
}
