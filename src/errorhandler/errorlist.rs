use crate::lexer::tokenlist::*;

#[derive(Debug, Clone)]
pub struct PreRuntimeError {
    pub errortype: PreRuntimeErrorType,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone)]
pub enum PreRuntimeErrorType {
    SyntaxError(SyntaxErrorType),
}

#[derive(Debug, Clone)]
pub enum SyntaxErrorType {
    UnTermStrLitError, //unterminated string literal
    UnTermCharLitError, //unterminated char literal
    TypeMismatchError(DataType, DataType), //type mismatch between expected and received
    UnknownTypeError(DataType), //unknown type detected error
    UnknownCharError, //unknown char that cannot be tokenized
    UnknownEscapeSeqError, //unknown escape sequence
    InvalidIdentifierNameError, //invalid identifier name
    InvalidOperationError, //invalid operation error
    StrFmtSyntaxError, //unable to format expression inside string due to syntax error
}

#[derive(Debug, Clone)]
pub enum RuntimeErrorType {

}