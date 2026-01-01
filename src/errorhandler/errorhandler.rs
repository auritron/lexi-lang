use core::fmt;
use std::fmt::Display;

use crate::lexer::tokenlist::*;
use crate::errorhandler::errorlist::*;

impl Display for PreRuntimeError {

    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Compilation Unsuccessful! Error: {} at line: {}, column: {}.", self.errortype, self.line, self.column)
    }

}

impl Display for PreRuntimeErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PreRuntimeErrorType::SyntaxError(err) => write!(f, "{}", err),
        }
    }
}

impl Display for SyntaxErrorType {

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SyntaxErrorType::InvalidIdentifierNameError => {
                write!(f, "Invalid identifier name detected,")
            },
            SyntaxErrorType::InvalidOperationError => {
                write!(f, "Invalid operation detected,")
            },
            SyntaxErrorType::TypeMismatchError(expected, found) => {
                write!(f, "Unexpected type, expected: {expected}, got: {found},")
            },
            SyntaxErrorType::UnknownTypeError(expected) => {
                write!(f, "Unexpected type, expected: {expected}, got unknown,")
            },
            SyntaxErrorType::UnTermCharLitError => {
                write!(f, "Unterminated char literal,")
            },
            SyntaxErrorType::UnTermStrLitError => {
                write!(f, "Unterminated str literal,")
            },
            SyntaxErrorType::UnknownCharError => {
                write!(f, "Unknown char detected which cannot be tokenized,")
            },
            SyntaxErrorType::UnknownEscapeSeqError => {
                write!(f, "Unknown escape sequence detected,")
            },
        }
    }

}