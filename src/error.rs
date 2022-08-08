use pest::RuleType;
use std::fmt::Error as FmtError;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("NonIntegerValue")]
    NonIntegerValue,
    #[error("NonBooleanValue")]
    NonBooleanValue,
    #[error("UnknownIdentifier: {0}")]
    UnknownIdentifier(String),
    #[error("ApplyOnNonFunctionValue")]
    ApplyOnNonFunctionValue,
    #[error("ToTokenError: {0}")]
    ToTokenError(String),
    #[error("AssertionError: expect the term \n {0} \n eval to `{1}`, go `{2}`")]
    AssertionError(String, String, String),
    #[error("ParseError: {0}")]
    ParseError(String),
    #[error("NotEvalToJudgement")]
    NotEvalToJudgement,
    #[error("UnsupportedSyntax")]
    UnsupportedSyntax,

    #[error("UnsupportedPattern")]
    UnsupportedPattern,

    #[error("UnknownHole:{0}")]
    UnknownHole(usize),
    #[error("UnificationError: Cannot normalize {0} with {1}")]
    UnificationError(String, String),
    #[error("TypeCheckAsError: Cannot type check {0} as {1}")]
    TypeCheckAsError(String, String),
    #[error("TypeCheckError: Cannot type check {0} without assertion")]
    TypeCheckError(String),
    #[error("SubstitutionError")]
    SubstitutionError,
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
