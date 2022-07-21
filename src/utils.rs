use crate::interface::Parse;
use pest::error::{Error, ErrorVariant};
use pest::iterators::Pair;
use pest::{Position, RuleType, Span};

pub fn error_span<R: RuleType>(span: Span, message: String) -> Error<R> {
    Error::new_from_span(ErrorVariant::CustomError { message }, span)
}

pub fn error_pos<R: RuleType>(pos: Position, message: String) -> Error<R> {
    Error::new_from_pos(ErrorVariant::CustomError { message }, pos)
}

impl<R: RuleType> Parse<R> for i64 {
    fn parse(entry_pair: Pair<R>) -> Result<i64, Error<R>>
    where
        Self: Sized,
    {
        entry_pair
            .as_str()
            .parse()
            .map_err(|_| error_span(entry_pair.as_span(), "expect an integer here".to_string()))
    }
}

impl<R: RuleType> Parse<R> for bool {
    fn parse(entry_pair: Pair<R>) -> Result<bool, Error<R>>
    where
        Self: Sized,
    {
        entry_pair
            .as_str()
            .parse()
            .map_err(|_| error_span(entry_pair.as_span(), "expect a boolean here".to_string()))
    }
}

impl<R: RuleType> Parse<R> for String {
    fn parse(entry_pair: Pair<R>) -> Result<String, Error<R>>
    where
        Self: Sized,
    {
        entry_pair.as_str().parse().map_err(|_| {
            error_span(
                entry_pair.as_span(),
                "expect an identifier here".to_string(),
            )
        })
    }
}
