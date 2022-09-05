use std::fmt::Display;

pub enum XmlBody {
    Inline,
    Expanded,
}

pub trait XmlFormattable {
    fn xml_elem<'a>(&'a self) -> &str;
    fn xml_body_type(&self) -> XmlBody {
        XmlBody::Inline
    }
    fn xml_inline_body(&self) -> String {
        "".to_owned()
    }
    fn write_xml_body(
        &self,
        _next_indent: usize,
        _indent_inc: usize,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self.xml_body_type() {
            XmlBody::Inline => write!(f, " {} ", self.xml_inline_body()),
            XmlBody::Expanded => write!(f, " {} ", self.xml_inline_body()),
        }
    }
}

pub struct XmlFormatter<'a, T: XmlFormattable> {
    indent: usize,
    indent_inc: usize,
    syntax: &'a T,
}

impl<'a, T> XmlFormatter<'a, T>
where
    T: XmlFormattable,
{
    pub fn new(syntax: &'a T, indent: usize, indent_inc: usize) -> Self {
        Self {
            indent,
            indent_inc,
            syntax,
        }
    }
}

impl<'a, T> Display for XmlFormatter<'a, T>
where
    T: XmlFormattable,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let indent_str = format!("{0:indent$}", "", indent = self.indent);
        write!(f, "{indent_str}<{}>", self.syntax.xml_elem())?;
        match self.syntax.xml_body_type() {
            XmlBody::Inline => {
                self.syntax
                    .write_xml_body(self.indent + self.indent_inc, self.indent_inc, f)?;
            }
            XmlBody::Expanded => {
                writeln!(f, "")?;
                self.syntax
                    .write_xml_body(self.indent + self.indent_inc, self.indent_inc, f)?;
                write!(f, "{indent_str}")?
            }
        }
        writeln!(f, "</{}>", self.syntax.xml_elem())?;
        Ok(())
    }
}

#[cfg(test)]
pub(crate) mod testutil {
    use std::fmt::Debug;

    use crate::{jack::token::*, parse::*};

    pub trait TokenResultFn<R> = Fn(&[Token]) -> Result<R, Option<Token>>;

    pub fn transform_result<'a, R>(
        result: ParseResult<'a, &'a [Token], R>,
    ) -> Result<R, Option<Token>> {
        result
            .map_err(|value| {
                println!("result: {:?}", value);
                value.first().cloned()
            })
            .map(|(rem, value)| value)
    }

    pub fn assert_tokens<F, R>(pairs: Vec<(&str, Result<R, Option<Token>>)>, pf: F)
    where
        F: TokenResultFn<R> + Copy, // Box<dyn Parser<'a, &'a [Token], R> + 'a> + Copy + 'a,
        R: Debug + std::cmp::PartialEq,
    {
        pairs.into_iter().for_each(|(source, expected)| {
            let src_str = source.to_owned();
            let (_, stream) = TokenStream::parse_into(src_str.as_str())
                .expect(format!("failed to tokenize {}", source).as_str());
            let result = pf(stream.tokens());
            assert_eq!(expected, result);
        });
    }
}
