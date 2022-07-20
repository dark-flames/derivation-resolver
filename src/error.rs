use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("MultiApplicableRules")]
    MultiApplicableRules,
    #[error("NoAvailableRules")]
    NoAvailableRules,
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
}
