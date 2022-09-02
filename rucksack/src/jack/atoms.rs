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
impl Parseable for IntConst {
    fn parser<'a>(_ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        Box::new(and_then(
            map(range(digit_char(), 1..), |chars| -> String {
                chars.into_iter().collect()
            }),
            |token| {
                i16::from_str_radix(token.as_str(), 10)
                    .map_err(|err| err.to_string())
                    .and_then(|num| IntConst::new(num))
            },
        ))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StringConst(String);
impl StringConst {
    pub fn new(value: String) -> Self {
        Self(value)
    }
}
impl Parseable for StringConst {
    fn parser<'a>(_ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        Box::new(map(
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
        ))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Int,
    Char,
    Boolean,
    ClassName(Id),
}
impl Parseable for Type {
    fn parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        Box::new(or_else(
            map(unbox(Keyword::Int.matcher(ctx)), |()| Self::Int),
            or_else(
                map(unbox(Keyword::Char.matcher(ctx)), |()| Self::Char),
                or_else(
                    map(unbox(Keyword::Boolean.matcher(ctx)), |()| Self::Boolean),
                    map(unbox(Id::parser(ctx)), |id| Self::ClassName(id)),
                ),
            ),
        ))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum KeywordConst {
    True,
    False,
    Null,
    This,
}
impl Parseable for KeywordConst {
    fn parser<'a>(ctx: &'a Ctx) -> Box<dyn Parser<'a, Self> + 'a> {
        Box::new(or_else(
            map(unbox(Keyword::True.matcher(ctx)), |()| Self::True),
            or_else(
                map(unbox(Keyword::False.matcher(ctx)), |()| Self::False),
                or_else(
                    map(unbox(Keyword::Null.matcher(ctx)), |()| Self::Null),
                    map(unbox(Keyword::This.matcher(ctx)), |()| Self::This),
                ),
            ),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn types() {
        let ctx = Ctx {};
        let parser = Type::parser(&ctx);

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
        let ctx = Ctx {};
        let parser = StringConst::parser(&ctx);

        assert_eq!(
            Ok(("", StringConst("a string".to_owned()))),
            parser.parse("\"a string\"")
        );
        assert_eq!(Err("a string"), parser.parse("a string"));
    }

    #[test]
    fn int_const() {
        let ctx = Ctx {};
        let parser = IntConst::parser(&ctx);

        assert_eq!(Ok(("", IntConst(0))), parser.parse("0"));
        assert_eq!(Ok(("", IntConst(12345))), parser.parse("12345"));
        assert_eq!(Ok(("", IntConst(32767))), parser.parse("32767"));
        assert_eq!(Err("-1"), parser.parse("-1"));
        assert_eq!(Err("32768"), parser.parse("32768"));
        assert_eq!(Err("123456"), parser.parse("123456"));
    }
}
