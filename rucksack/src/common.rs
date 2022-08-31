use std::fmt::{Debug, Display};
use std::fs::File;
use std::io::Error;
use std::io::{BufRead, BufReader, Lines};

pub fn read_input_to_end(filename: &str) -> Result<Vec<String>, Error> {
    return read_input(filename)?.collect();
}

pub fn read_input(filename: &str) -> Result<Lines<BufReader<File>>, Error> {
    return File::open(filename)
        .map(|file| BufReader::new(file))
        .map(|reader| reader.lines());
}

pub fn read_to_string(filename: &str) -> Result<String, Error> {
    return read_input_to_end(filename).map(|lines| lines.join("\n"));
}

pub trait UnitFactory {
    type Unit: crate::common::Unit;

    fn unit_from(src_path: &str) -> Result<Self::Unit, Error>;
}

pub trait Unit {
    type Syntax;

    fn src_path<'a>(&'a self) -> &'a str;
    //    fn out_path<'a>(&'a self) -> &'a str;

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

use crate::parse::Parser;
pub trait LegacyParser {
    type Code;

    fn out_path<'a>(&'a self) -> &'a str;

    fn to_code<'a>(&'a self) -> &'a Self::Code;

    fn save(&self) -> Result<(), Error>
    where
        Self::Code: Display + Debug,
    {
        let mut out = File::create(self.out_path())?;
        write!(out, "{}", self.to_code())
    }
}

pub fn err_invalid_input<E>(msg: E) -> Error
where
    E: Into<Box<dyn std::error::Error + Send + Sync>>,
{
    Error::new(std::io::ErrorKind::InvalidInput, msg)
}
