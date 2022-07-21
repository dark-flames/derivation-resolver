use pest::RuleType;
use std::fmt::Error as FmtError;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("NonIntegerValue")]
    NonIntegerValue,
    #[error("NonBooleanValue")]
    NonBooleanValue,
    #[error("UnknownIdentifier")]
    UnknownIdentifier,
    #[error("ApplyOnNonFunctionValue")]
    ApplyOnNonFunctionValue,
    #[error("ToTokenError: {0}")]
    ToTokenError(String),
    #[error("AssertionError: expect the term \n {0} \n eval to `{1}`, go `{2}`")]
    AssertionError(String, String, String),
    #[error("ParseError: {0}")]
    ParseError(String),
}

impl<R: RuleType> From<pest::error::Error<R>> for Error {
    fn from(p: pest::error::Error<R>) -> Self {
        Error::ParseError(p.to_string())
    }
}

impl From<FmtError> for Error {
    fn from(r: FmtError) -> Self {
        Error::ToTokenError(r.to_string())
    }
}
