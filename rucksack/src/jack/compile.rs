use std::io::Error;

use crate::vm::VMParsed;

use super::class::Class;

pub struct JackCompiler {}

impl JackCompiler {
    pub fn compile(&self, classes: Vec<Class>) -> Result<Vec<VMParsed>, Error> {
        classes
            .into_iter()
            .map(|class| self.compile_class(class))
            .collect()
    }

    fn compile_class(&self, class: Class) -> Result<VMParsed, Error> {
        Ok(VMParsed::new(class.name().to_owned(), Vec::new()))
    }
}
