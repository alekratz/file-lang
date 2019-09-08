use crate::common::span::Span;
use snafu::Snafu;

#[derive(Debug, Snafu)]
pub enum SyntaxError {
    #[snafu(display("expected {}, but got {} instead", expected, got))]
    ExpectedGot {
        span: Span,
        expected: String,
        got: String,
    },

    #[snafu(display("unexpected {}", what))]
    Unexpected {
        span: Span,
        what: String,
    },

    #[snafu(display("invalid {}: {}", what, why))]
    Invalid {
        span: Span,
        what: String,
        why: String,
    },
}

pub type Result<T> = std::result::Result<T, SyntaxError>;
