use crate::parse::*;

use super::{id::Id, keyword::Keyword};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IntConst(i16);
impl IntConst {
    pub fn new(value: i16) -> Result<Self, String> {
        if value < 0 {
            Err("value must be >= 0".to_owned())
        } else {
            Ok(IntConst(value))
        }
    }
}
impl Parses<IntConst> for IntConst {
    fn parse<'a>(input: &'a str) -> ParseResult<'a, IntConst> {
        and_then(
            map(range(digit_char(), 1..), |chars| -> String {
                chars.into_iter().collect()
            }),
            |token| {
                i16::from_str_radix(token.as_str(), 10)
                    .map_err(|err| err.to_string())
                    .and_then(|num| IntConst::new(num))
            },
        )
        .parse(input)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StringConst(String);
impl StringConst {
    pub fn new(value: String) -> Self {
        Self(value)
    }
}
impl Parses<StringConst> for StringConst {
    fn parse<'a>(input: &'a str) -> ParseResult<'a, StringConst> {
        map(
            right(
                match_literal("\""),
                left(
                    map(until(non_nl_char(), 0.., "\""), |chars| -> String {
                        chars.into_iter().collect()
                    }),
                    match_literal("\""),
                ),
            ),
            |text| StringConst(text),
        )
        .parse(input)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Int,
    Char,
    Boolean,
    ClassName(Id),
}
impl Parses<Type> for Type {
    fn parse<'a>(input: &'a str) -> ParseResult<'a, Type> {
        or_else(
            map(Keyword::Int, |_| Self::Int),
            or_else(
                map(Keyword::Char, |_| Self::Char),
                or_else(
                    map(Keyword::Boolean, |_| Self::Boolean),
                    map(move |input| Id::parse(input), |id| Self::ClassName(id)),
                ),
            ),
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
impl Parses<KeywordConst> for KeywordConst {
    fn parse<'a>(input: &'a str) -> ParseResult<'a, KeywordConst> {
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
    #[test]
    fn types() {

        let parser = move |input| Type::parse(input);

        assert_eq!(Ok(("", Type::Int)), parser.parse("int"));
        assert_eq!(Ok(("", Type::Char)), parser.parse("char"));
        assert_eq!(Ok(("", Type::Boolean)), parser.parse("boolean"));
        assert_eq!(
            Ok(("", Type::ClassName(Id::new("MyClass".to_owned())))),
            parser.parse("MyClass")
        );
        assert_eq!(Err("12345"), parser.parse("12345"));
        assert_eq!(Err("1Class"), parser.parse("1Class"));
    }

    #[test]
    fn string_const() {
        assert_eq!(
            Ok(("", StringConst("a string".to_owned()))),
            StringConst::parse("\"a string\"")
        );
        assert_eq!(Err("a string"), StringConst::parse("a string"));
    }

    #[test]
    fn int_const() {
        assert_eq!(Ok(("", IntConst(0))), IntConst::parse("0"));
        assert_eq!(Ok(("", IntConst(12345))), IntConst::parse("12345"));
        assert_eq!(Ok(("", IntConst(32767))), IntConst::parse("32767"));
        assert_eq!(Err("-1"), IntConst::parse("-1"));
        assert_eq!(Err("32768"), IntConst::parse("32768"));
        assert_eq!(Err("123456"), IntConst::parse("123456"));
    }
}
