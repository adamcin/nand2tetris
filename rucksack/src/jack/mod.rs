mod class;
mod common;
mod expression;
mod id;
mod keyword;
mod statement;
mod subroutine;
mod sym;
mod token;
mod typea;
use std::io::Error;

use crate::common::{err_invalid_input, Unit, UnitFactory};
use crate::parse::*;

use self::class::Class;
use self::id::Id;
use self::keyword::Keyword;
use self::token::TokenStream;

pub struct JackParser {}
impl<'c> JackParser {
    pub fn new() -> Self {
        Self {}
    }
}

pub struct JackUnit {
    src_path: String,
}

impl Unit for JackUnit {
    type Syntax = Class;

    fn src_path<'a>(&'a self) -> &'a str {
        self.src_path.as_str()
    }

    fn parse<'a>(&'a self) -> Result<Self::Syntax, Error> {
        let source = std::fs::read_to_string(self.src_path())?;
        let (_str_rem, stream) = TokenStream::parse_into(source.as_str()).expect("must work");
        let parser = move |input| Class::parse_into(input);
        let result = parser.parse(stream.tokens());
        match result {
            Ok((_rem, parsed)) => Ok(parsed),
            Err(error_at) => Err(err_invalid_input(format!("parse error at: {:?}", error_at))),
        }
    }
}

pub enum JackUnitType {
    FileUnit(),
    DirUnit(),
}

impl Unit for JackUnitType {
    type Syntax = Vec<Class>;

    fn src_path<'a>(&'a self) -> &'a str {
        unimplemented!("src_path")
    }

    fn parse(&self) -> Result<Self::Syntax, Error> {
        unimplemented!("parse")
    }
}

pub struct JackUnitFactory {}

impl UnitFactory for JackUnitFactory {
    type Unit = JackUnitType;
    fn unit_from(path: &str) -> Result<Self::Unit, Error> {
        unimplemented!("JackUnitFactory::read")
    }
}

pub struct JackAnalyzer {}

impl JackAnalyzer {
    pub fn do_main(args: Vec<&str>) -> Result<(), Error> {
        if args.is_empty() {
            Self::do_unit(".")?;
        } else {
            for arg in args {
                Self::do_unit(arg)?;
            }
        }
        Ok(())
    }

    fn do_unit(path: &str) -> Result<(), Error> {
        let unit =
            JackUnitFactory::unit_from(path).expect(&format!("failed to read unit {}", path));
        let code = unit.parse()?;
        //writeln!(std::io::stdout(), "{:?}", code).ok();
        //parser.save().expect("failed to save asm file");
        Ok(())
    }
}

#[cfg(test)]
mod test {

    #[test]
    fn simple_test() {}
}
