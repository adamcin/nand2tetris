use std::fmt::Display;

use crate::parse::*;

use super::{
    common::{XmlBody, XmlFormattable, XmlFormatter},
    id::Id,
    keyword::Keyword,
    sym::Sym,
};

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

    pub fn zero() -> Self {
        Self(0)
    }

    pub fn one() -> Self {
        Self(1)
    }

    pub fn copy(&self) -> Self {
        Self(self.0)
    }
}
impl<'a> Parses<'a> for IntConst {
    type Input = &'a str;
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        and_then(
            map(range(digit_char(), 1..), |chars| -> String {
                chars.into_iter().collect()
            }),
            |token| {
                i16::from_str_radix(token.as_str(), 10)
                    .map_err(|err| err.to_string())
                    .and_then(|num| Self::new(num))
            },
        )
        .parse(input)
    }
}

impl TryFrom<i16> for IntConst {
    type Error = String;
    fn try_from(value: i16) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StringConst(String);
impl StringConst {
    pub fn new(value: String) -> Self {
        Self(value)
    }
    pub fn copy(&self) -> Self {
        Self(self.0.to_owned())
    }
}
impl<'a> Parses<'a> for StringConst {
    type Input = &'a str;
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
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
            |text| Self(text),
        )
        .parse(input)
    }
}

impl From<&str> for StringConst {
    fn from(item: &str) -> Self {
        Self(item.to_owned())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Keyword(Keyword),
    Sym(Sym),
    IntConst(IntConst),
    StringConst(StringConst),
    Identifier(Id),
}
impl Token {
    pub fn key<'a>(input: &'a [Self], filter: Keyword) -> ParseResult<'a, &'a [Self], Keyword> {
        match input.split_first() {
            Some((&Self::Keyword(value), rem)) if value == filter => Ok((rem, value)),
            _ => Err(input),
        }
    }

    pub fn sym<'a>(input: &'a [Self], filter: Sym) -> ParseResult<'a, &'a [Self], Sym> {
        match input.split_first() {
            Some((&Self::Sym(value), rem)) if value == filter => Ok((rem, value)),
            _ => Err(input),
        }
    }

    pub fn id<'a>(input: &'a [Self]) -> ParseResult<'a, &'a [Self], Id> {
        match input.split_first() {
            Some((Self::Identifier(value), rem)) => Ok((rem, value.copy())),
            _ => Err(input),
        }
    }

    pub fn int<'a>(input: &'a [Self]) -> ParseResult<'a, &'a [Self], IntConst> {
        match input.split_first() {
            Some((Self::IntConst(value), rem)) => Ok((rem, value.copy())),
            _ => Err(input),
        }
    }

    pub fn str<'a>(input: &'a [Self]) -> ParseResult<'a, &'a [Self], StringConst> {
        match input.split_first() {
            Some((Self::StringConst(value), rem)) => Ok((rem, value.copy())),
            _ => Err(input),
        }
    }
}

impl<'a> Parses<'a> for Token {
    type Input = &'a str;
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        or_else(
            map(
                move |input| StringConst::parse_into(input),
                |v| Self::StringConst(v),
            ),
            or_else(
                map(
                    move |input| IntConst::parse_into(input),
                    |v| Self::IntConst(v),
                ),
                or_else(
                    map(move |input| Sym::parse_into(input), |v| Self::Sym(v)),
                    or_else(
                        map(move |input| Id::parse_into(input), |v| Self::Identifier(v)),
                        map(
                            move |input| Keyword::parse_into(input),
                            |v| Self::Keyword(v),
                        ),
                    ),
                ),
            ),
        )
        .parse(input)
    }
}

impl From<Keyword> for Token {
    fn from(item: Keyword) -> Self {
        Self::Keyword(item)
    }
}

impl From<Sym> for Token {
    fn from(item: Sym) -> Self {
        Self::Sym(item)
    }
}

impl From<Id> for Token {
    fn from(item: Id) -> Self {
        Self::Identifier(item)
    }
}

impl From<StringConst> for Token {
    fn from(item: StringConst) -> Self {
        Self::StringConst(item)
    }
}

impl From<IntConst> for Token {
    fn from(item: IntConst) -> Self {
        Self::IntConst(item)
    }
}

impl XmlFormattable for Token {
    fn xml_elem<'a>(&'a self) -> &str {
        match self {
            Self::Keyword(_) => "keyword",
            Self::Sym(_) => "symbol",
            Self::Identifier(_) => "identifier",
            Self::StringConst(_) => "stringConstant",
            Self::IntConst(_) => "integerConstant",
        }
    }

    fn xml_inline_body(&self) -> String {
        match self {
            Self::Keyword(keyword) => keyword.as_str().to_owned(),
            Self::Sym(Sym::Amp) => "&amp;".to_owned(),
            Self::Sym(Sym::LAngle) => "&lt;".to_owned(),
            Self::Sym(Sym::RAngle) => "&gt;".to_owned(),
            Self::Sym(sym) => sym.as_str().to_owned(),
            Self::Identifier(id) => id.as_str().to_owned(),
            Self::StringConst(StringConst(value)) => value.to_owned(),
            Self::IntConst(IntConst(value)) => format!("{}", value),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TokenStream {
    tokens: Vec<Token>,
}

impl TokenStream {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens }
    }
}

impl<'a> TokenStream {
    pub fn tokens(&'a self) -> &'a [Token] {
        self.tokens.as_slice()
    }
}

impl<'a> Parses<'a> for TokenStream {
    type Input = &'a str;
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        map(
            range(
                right(ok(comspace()), move |input| Token::parse_into(input)),
                0..,
            ),
            |tokens| Self::new(tokens),
        )
        .parse(input)
    }
}

impl Display for TokenStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}

impl XmlFormattable for TokenStream {
    fn xml_elem<'a>(&'a self) -> &str {
        "tokens"
    }

    fn xml_body_type(&self) -> XmlBody {
        XmlBody::Expanded
    }

    fn write_xml_body(
        &self,
        _next_indent: usize,
        _indent_inc: usize,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        for token in self.tokens.iter() {
            let fmtter = XmlFormatter::new(token, _next_indent, _indent_inc);
            write!(f, "{}", fmtter)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn string_const() {
        assert_eq!(
            Ok(("", StringConst("a string".to_owned()))),
            StringConst::parse_into("\"a string\"")
        );
        assert_eq!(Err("a string"), StringConst::parse_into("a string"));
    }

    #[test]
    fn class_tokens() {
        assert_eq!(
            Ok((
                " ",
                TokenStream::new(vec![
                    Keyword::Class.into(),
                    Id::from("MyClass").into(),
                    Sym::LCurly.into(),
                    Keyword::Static.into(),
                    Keyword::Int.into(),
                    Id::from("count").into(),
                    Sym::Semi.into(),
                    Sym::RCurly.into(),
                    Id::from("extra").into(),
                ])
            )),
            TokenStream::parse_into(
                r"class MyClass {
                static int count// some inline comment
                /* 
                some multiline comment;
                 */
                ;

            } extra "
            )
        );
    }

    #[test]
    fn int_const() {
        assert_eq!(Ok(("", IntConst(0))), IntConst::parse_into("0"));
        assert_eq!(Ok(("", IntConst(12345))), IntConst::parse_into("12345"));
        assert_eq!(Ok(("", IntConst(32767))), IntConst::parse_into("32767"));
        assert_eq!(Err("-1"), IntConst::parse_into("-1"));
        assert_eq!(Err("32768"), IntConst::parse_into("32768"));
        assert_eq!(Err("123456"), IntConst::parse_into("123456"));
    }

    #[test]
    fn xml_out() {
        let source = r#"
// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/10/ArrayTest/Main.jack

// (identical to projects/09/Average/Main.jack)

/** Computes the average of a sequence of integers. */
class Main {
    function void main() {
        var Array a;
        var int length;
        var int i, sum;
	
	let length = Keyboard.readInt("HOW MANY NUMBERS? ");
	let a = Array.new(length);
	let i = 0;
	
	while (i < length) {
	    let a[i] = Keyboard.readInt("ENTER THE NEXT NUMBER: ");
	    let i = i + 1;
	}
	
	let i = 0;
	let sum = 0;
	
	while (i < length) {
	    let sum = sum + a[i];
	    let i = i + 1;
	}
	
	do Output.printString("THE AVERAGE IS: ");
	do Output.printInt(sum / length);
	do Output.println();
	
	return;
    }
}
        "#;

        let expected = r#"<tokens>
<keyword> class </keyword>
<identifier> Main </identifier>
<symbol> { </symbol>
<keyword> function </keyword>
<keyword> void </keyword>
<identifier> main </identifier>
<symbol> ( </symbol>
<symbol> ) </symbol>
<symbol> { </symbol>
<keyword> var </keyword>
<identifier> Array </identifier>
<identifier> a </identifier>
<symbol> ; </symbol>
<keyword> var </keyword>
<keyword> int </keyword>
<identifier> length </identifier>
<symbol> ; </symbol>
<keyword> var </keyword>
<keyword> int </keyword>
<identifier> i </identifier>
<symbol> , </symbol>
<identifier> sum </identifier>
<symbol> ; </symbol>
<keyword> let </keyword>
<identifier> length </identifier>
<symbol> = </symbol>
<identifier> Keyboard </identifier>
<symbol> . </symbol>
<identifier> readInt </identifier>
<symbol> ( </symbol>
<stringConstant> HOW MANY NUMBERS?  </stringConstant>
<symbol> ) </symbol>
<symbol> ; </symbol>
<keyword> let </keyword>
<identifier> a </identifier>
<symbol> = </symbol>
<identifier> Array </identifier>
<symbol> . </symbol>
<identifier> new </identifier>
<symbol> ( </symbol>
<identifier> length </identifier>
<symbol> ) </symbol>
<symbol> ; </symbol>
<keyword> let </keyword>
<identifier> i </identifier>
<symbol> = </symbol>
<integerConstant> 0 </integerConstant>
<symbol> ; </symbol>
<keyword> while </keyword>
<symbol> ( </symbol>
<identifier> i </identifier>
<symbol> &lt; </symbol>
<identifier> length </identifier>
<symbol> ) </symbol>
<symbol> { </symbol>
<keyword> let </keyword>
<identifier> a </identifier>
<symbol> [ </symbol>
<identifier> i </identifier>
<symbol> ] </symbol>
<symbol> = </symbol>
<identifier> Keyboard </identifier>
<symbol> . </symbol>
<identifier> readInt </identifier>
<symbol> ( </symbol>
<stringConstant> ENTER THE NEXT NUMBER:  </stringConstant>
<symbol> ) </symbol>
<symbol> ; </symbol>
<keyword> let </keyword>
<identifier> i </identifier>
<symbol> = </symbol>
<identifier> i </identifier>
<symbol> + </symbol>
<integerConstant> 1 </integerConstant>
<symbol> ; </symbol>
<symbol> } </symbol>
<keyword> let </keyword>
<identifier> i </identifier>
<symbol> = </symbol>
<integerConstant> 0 </integerConstant>
<symbol> ; </symbol>
<keyword> let </keyword>
<identifier> sum </identifier>
<symbol> = </symbol>
<integerConstant> 0 </integerConstant>
<symbol> ; </symbol>
<keyword> while </keyword>
<symbol> ( </symbol>
<identifier> i </identifier>
<symbol> &lt; </symbol>
<identifier> length </identifier>
<symbol> ) </symbol>
<symbol> { </symbol>
<keyword> let </keyword>
<identifier> sum </identifier>
<symbol> = </symbol>
<identifier> sum </identifier>
<symbol> + </symbol>
<identifier> a </identifier>
<symbol> [ </symbol>
<identifier> i </identifier>
<symbol> ] </symbol>
<symbol> ; </symbol>
<keyword> let </keyword>
<identifier> i </identifier>
<symbol> = </symbol>
<identifier> i </identifier>
<symbol> + </symbol>
<integerConstant> 1 </integerConstant>
<symbol> ; </symbol>
<symbol> } </symbol>
<keyword> do </keyword>
<identifier> Output </identifier>
<symbol> . </symbol>
<identifier> printString </identifier>
<symbol> ( </symbol>
<stringConstant> THE AVERAGE IS:  </stringConstant>
<symbol> ) </symbol>
<symbol> ; </symbol>
<keyword> do </keyword>
<identifier> Output </identifier>
<symbol> . </symbol>
<identifier> printInt </identifier>
<symbol> ( </symbol>
<identifier> sum </identifier>
<symbol> / </symbol>
<identifier> length </identifier>
<symbol> ) </symbol>
<symbol> ; </symbol>
<keyword> do </keyword>
<identifier> Output </identifier>
<symbol> . </symbol>
<identifier> println </identifier>
<symbol> ( </symbol>
<symbol> ) </symbol>
<symbol> ; </symbol>
<keyword> return </keyword>
<symbol> ; </symbol>
<symbol> } </symbol>
<symbol> } </symbol>
</tokens>"#;

        let (str_rem, stream) = TokenStream::parse_into(source).expect("");
        let fmtter = XmlFormatter::new(&stream, 0, 0);
        let formatted = format!("{}", fmtter);
        for (expect, actual) in expected.trim().lines().zip(formatted.trim().lines()) {
            assert_eq!(expect, actual);
        }
        
    }
}
