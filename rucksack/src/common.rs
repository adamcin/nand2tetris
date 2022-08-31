use std::fmt::{Debug, Display};
use std::fs::File;
use std::io::Error;

pub trait UnitFactory {
    type Unit: crate::common::Unit;

    fn unit_from(src_path: &str) -> Result<Self::Unit, Error>;
}

pub trait Unit {
    type Syntax;

    fn src_path<'a>(&'a self) -> &'a str;

    fn parse(&self) -> Result<Self::Syntax, Error>;

    fn save(&self, syntax: &Self::Syntax) -> Result<(), Error>
    where
        Self::Syntax: Display + Debug,
    {
        let mut out = File::create(self.src_path())?;
        write!(out, "{}", syntax)
    }
}

use std::io::Write;

pub fn err_invalid_input<E>(msg: E) -> Error
where
    E: Into<Box<dyn std::error::Error + Send + Sync>>,
{
    Error::new(std::io::ErrorKind::InvalidInput, msg)
}
