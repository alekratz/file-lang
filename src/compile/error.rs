use crate::{common::span::Span, syn};
use snafu::Snafu;

#[derive(Debug, Snafu)]
pub enum CompileError {
    #[snafu(display("{}", what))]
    InvalidOp { span: Span, what: String },

    #[snafu(display("{}", what))]
    InvalidLValue { span: Span, what: String },

    #[snafu(display("{}", what))]
    InvalidStringEscape { span: Span, what: char },

    #[snafu(display("{}", error))]
    SyntaxError { error: syn::error::SyntaxError },
}

impl From<syn::error::SyntaxError> for CompileError {
    fn from(other: syn::error::SyntaxError) -> Self {
        CompileError::SyntaxError { error: other }
    }
}

pub type Result<T> = std::result::Result<T, CompileError>;
