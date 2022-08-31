use std::io::Error;

use crate::common::{Unit, UnitFactory, LegacyParser};

pub type JackVMCode = &'static str;

pub enum JackParserType {
    FileParser(),
    DirParser(),
}

pub enum JackUnitType {
    FileUnit(),
    DirUnit(),
}

impl Unit for JackUnitType {
    type Syntax = JackParserType;
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
        let unit = JackUnitFactory::unit_from(path).expect(&format!("failed to read unit {}", path));
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
