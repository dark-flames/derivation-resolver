use crate::interface::ParseAs;
use pest::error::{Error, ErrorVariant};
use pest::iterators::Pairs;
use pest::{RuleType, Span};

pub fn error_span<R: RuleType>(span: Span, message: String) -> Error<R> {
    Error::new_from_span(ErrorVariant::CustomError { message }, span)
}
