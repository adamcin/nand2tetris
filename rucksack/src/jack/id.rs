use crate::common::err_invalid_input;
use crate::parse::*;

use super::keyword::Keyword;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Id(String);

impl Id {
    pub fn new(value: String) -> Self {
        Self(value)
    }

    fn is_ident_lead(c: &char) -> bool {
        c.is_ascii_alphabetic() || c == &'_'
    }

    fn is_ident(c: &char) -> bool {
        Self::is_ident_lead(c) || c.is_ascii_digit()
    }
}

impl Parses<Id> for Id {
    fn parse<'a>(input: &'a str) -> ParseResult<'a, Id> {
        and_then(
            map(
                map(
                    pair(
                        pred(any_char, |c| Self::is_ident_lead(c)),
                        range(pred(any_char, |c| Self::is_ident(c)), 0..),
                    ),
                    |(l, cs)| vec![vec![l], cs].concat(),
                ),
                |chars| chars.into_iter().collect(),
            ),
            |s: String| {
                if Keyword::is_reserved(s.as_str()) {
                    Err(err_invalid_input(format!("{} is a reserved keyword", s)))
                } else {
                    Ok(Id::new(s))
                }
            },
        ).parse(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::*;
    #[test]
    fn ids() {
        let parser = move |input| Id::parse(input);

        assert_eq!(Ok(("", Id("MyClass".to_owned()))), parser.parse("MyClass"));
        assert_eq!(Err("12345"), parser.parse("12345"));
        assert_eq!(Err("1Class"), parser.parse("1Class"));
        assert_eq!(Ok(("", Id("Class1".to_owned()))), parser.parse("Class1"));
        
    }
}
