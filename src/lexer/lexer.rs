use std::fmt::Display;

use crate::lexer::tokenlist;
use crate::lexer::tokenlist::*;
use crate::errorhandler::errorhandler::*;
use crate::errorhandler::errorlist::*;

#[derive(Debug, Clone, PartialEq)]
pub enum LexMode {
    StringMode, //strings and chars
    CommentMode, //comments
    IntLiteralMode, //integer datatypes
    FloatLiteralMode, //float datatypes
    OpOrPuncMode(bool), //operators and punctuators, false for not yet detected paired op, true for detected paired op
    QuotesMode(bool), //specifically for quotations, both single and double, true is opening and false is closing
    WordMode, //alphanumerical tokens, incl. boolean literals
    NullMode, //whitespace and chars
    UnknownMode, //handle unknown characters
}

impl Display for LexMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::StringMode => { write!(f, "StringMode") },
            Self::CommentMode => { write!(f, "CommentMode") },
            Self::IntLiteralMode => { write!(f, "IntLiteralMode") },
            Self::FloatLiteralMode => { write!(f, "FloatLiteralMode") },
            Self::OpOrPuncMode(val) => { write!(f, "OpOrPuncMode({})", val) },
            Self::QuotesMode(val) => { write!(f, "QuotesMode({})", val) },
            Self::WordMode => { write!(f, "WordMode") },
            Self::NullMode => { write!(f, "NullMode") },
            Self::UnknownMode => { write!(f, "UnknownMode") },
        }
    }
}

pub enum LexState {
    PushChar,
    EmitToken,
    EmitAndPush,
    Idle,
}

pub struct Lexer {
    tokens: Vec<Token>, //tokens list
    lex_mode: LexMode, //lexing mode per char
    prev_mode: LexMode, //previous lexing mode
    lex_state: LexState, //state of the lexer
    buffer: String, //temporary string for tokenization
    escape_mode: u8, //if char is escape character '\'
    line: usize, //line of the character
    column: usize, //column of the character
}

impl Lexer {

    pub fn init() -> Self {
        Self {
            tokens: Vec::new(),
            lex_mode: LexMode::NullMode,
            prev_mode: LexMode::NullMode,
            lex_state: LexState::Idle,
            buffer: String::new(),
            escape_mode: 0,
            line: 1,
            column: 1,
        }
    }

    pub fn tokenize(&mut self, mut input: String) -> Result<Vec<Token>, PreRuntimeError> {
        
        input.push(' ');
        let charlist = input.chars(); // push ' ' to push the final character to treat as nullmode

        for c in charlist {

            //1. Set the new LexMode
            self.set_lexmode(c)?;

            //2. Set the corresponding LexState
            self.set_lexstate()?;

            //3. Perform an operation based on the current LexState (Push, Emit etc.)
            self.execute(c)?;

            //4. Update line and char
            if c == '\n' {
                    self.line += 1;
                    self.column = 1;
                } else {
                    self.column += 1;
                }

            //5. Update previous LexMode
            self.prev_mode = self.lex_mode.clone();

        }

        if self.lex_mode == LexMode::StringMode {
            return Err(self.raise_error(SyntaxErrorType::UnTermStrLitError));
        }

        self.tokens.push(Token::EOF);
        Ok(self.tokens.clone())

    }

    fn set_lexmode(&mut self, c: char) -> Result<(), PreRuntimeError> {

        if self.lex_mode == LexMode::CommentMode { // CommentMode

            if c == '\n' {
                self.lex_mode = LexMode::NullMode;
            }

        } else {
            
            if matches!(c, tokenlist::STR_QUOTES | tokenlist::CHAR_QUOTES) && self.escape_mode == 0 { // switch to quotesmode

                if matches!(self.lex_mode, LexMode::StringMode | LexMode::QuotesMode(true)) {
                    self.lex_mode = LexMode::QuotesMode(false);
                } else {
                    self.lex_mode = LexMode::QuotesMode(true);
                }

            } else if self.lex_mode == LexMode::QuotesMode(true) { // switch from quotes mode for string initialization or termination

                self.lex_mode = LexMode::StringMode;

                if c == tokenlist::ESCAPE_CHAR {
                    self.escape_mode = 1;
                }

            } else if self.lex_mode == LexMode::StringMode { 

                if c == tokenlist::ESCAPE_CHAR && self.escape_mode == 0 {
                    self.escape_mode = 1;
                }

            } else { // other modes are accounted for when NOT a string

                if c == tokenlist::COMMENT_CHAR { //set comment mode

                    self.lex_mode = LexMode::CommentMode;

                } else if c.is_numeric() && self.lex_mode != LexMode::WordMode { // convert int numliteral

                    if self.lex_mode == LexMode::FloatLiteralMode {
                        self.lex_mode = LexMode::FloatLiteralMode;
                    } else {
                        self.lex_mode = LexMode::IntLiteralMode;
                    }

                } else if c == tokenlist::DECIMAL_CHAR && self.lex_mode == LexMode::IntLiteralMode {

                        self.lex_mode = LexMode::FloatLiteralMode;

                } else if c == tokenlist::DECIMAL_CHAR && self.lex_mode == LexMode::FloatLiteralMode {

                        return Err(self.raise_error(SyntaxErrorType::UnknownTypeError(DataType::Float64)));
                    
                } else if tokenlist::OPS_AND_PUNCS_CHARS.contains(&c) { // when its a non quotes operator that can be tokenized
                    
                    match self.lex_mode {
                        LexMode::OpOrPuncMode(_) => {
                            let mut s = self.buffer.to_string();
                            s.push(c);
                            if tokenlist::OPS_AND_PUNCS.contains_key(s.as_str()) {
                                self.lex_mode = LexMode::OpOrPuncMode(true);
                            } else {
                                self.lex_mode = LexMode::OpOrPuncMode(false);
                            }
                        },
                        _ => { self.lex_mode = LexMode::OpOrPuncMode(false); },
                    }

                } else if c.is_ascii_alphanumeric() || c == tokenlist::UNDERSCORE { //when a valid identifier character

                    self.lex_mode = LexMode::WordMode;

                } else if tokenlist::WHITESPACE_CHARS.contains(&c) { // set to null mode when whitespace type char

                    self.lex_mode = LexMode::NullMode;

                } else { //unknown character, lexing will be terminated later in the cycle

                    self.lex_mode = LexMode::UnknownMode;

                }
             
            }

        }
        Ok(())

    }

    fn set_lexstate(&mut self) -> Result<(), PreRuntimeError> {

        match &self.prev_mode {

            // word mode
            LexMode::WordMode => match &self.lex_mode {
                LexMode::WordMode => self.lex_state = LexState::PushChar,
                LexMode::NullMode | LexMode::CommentMode => self.lex_state = LexState::EmitToken,
                LexMode::QuotesMode(true) | LexMode::OpOrPuncMode(false) => self.lex_state = LexState::EmitAndPush,
                LexMode::UnknownMode => { return Err(self.raise_error(SyntaxErrorType::UnknownCharError)); },
                LexMode::IntLiteralMode | LexMode::FloatLiteralMode | LexMode::StringMode | LexMode::QuotesMode(false) | LexMode::OpOrPuncMode(true) => self.panic(),
            },

            // num literal modes
            LexMode::IntLiteralMode | LexMode::FloatLiteralMode => match &self.lex_mode {
                LexMode::WordMode | LexMode::IntLiteralMode | LexMode::FloatLiteralMode => self.lex_state = LexState::PushChar,
                LexMode::NullMode | LexMode::CommentMode => self.lex_state = LexState::EmitToken,
                LexMode::QuotesMode(true) | LexMode::OpOrPuncMode(false) => self.lex_state = LexState::EmitAndPush,
                LexMode::UnknownMode => { return Err(self.raise_error(SyntaxErrorType::UnknownCharError)); },
                LexMode::StringMode | LexMode::QuotesMode(false) | LexMode::OpOrPuncMode(true) => self.panic(),
            },

            //closing quotes mode
            LexMode::QuotesMode(false) => match &self.lex_mode {
                LexMode::NullMode | LexMode::CommentMode => self.lex_state = LexState::EmitToken,
                LexMode::OpOrPuncMode(false) | LexMode::QuotesMode(true) | LexMode::WordMode | LexMode::IntLiteralMode | LexMode::FloatLiteralMode => self.lex_state = LexState::EmitAndPush,
                LexMode::UnknownMode => { return Err(self.raise_error(SyntaxErrorType::UnknownCharError)); },
                LexMode::QuotesMode(false) | LexMode::StringMode | LexMode::OpOrPuncMode(true) => self.panic(),
            },

            //opening quotes mode
            LexMode::QuotesMode(true) => match &self.lex_mode {
                LexMode::StringMode => self.lex_state = LexState::EmitAndPush,
                _ => self.panic(),
            },

            //operation and punctuation mode
            LexMode::OpOrPuncMode(_) => match &self.lex_mode {
                LexMode::OpOrPuncMode(true) => self.lex_state = LexState::PushChar,
                LexMode::NullMode | LexMode::CommentMode => self.lex_state = LexState::EmitToken,
                LexMode::WordMode | LexMode::QuotesMode(true) | LexMode::IntLiteralMode | LexMode::FloatLiteralMode | LexMode::OpOrPuncMode(false) => self.lex_state = LexState::EmitAndPush,
                LexMode::UnknownMode => { return Err(self.raise_error(SyntaxErrorType::UnknownCharError)); },
                LexMode::StringMode | LexMode::QuotesMode(false) => self.panic(),
            },

            //strings and chars
            LexMode::StringMode => match &self.lex_mode {
                LexMode::StringMode => self.lex_state = LexState::PushChar,
                LexMode::QuotesMode(false) => self.lex_state = LexState::EmitAndPush,
                _ => self.panic(),
            },

            //nullmode - whitespace type characters
            LexMode::NullMode => match &self.lex_mode {
                LexMode::NullMode | LexMode::CommentMode => self.lex_state = LexState::Idle,
                LexMode::WordMode | LexMode::IntLiteralMode | LexMode::FloatLiteralMode | LexMode::OpOrPuncMode(false) | LexMode::QuotesMode(true) => self.lex_state = LexState::PushChar,
                LexMode::UnknownMode => { return Err(self.raise_error(SyntaxErrorType::UnknownCharError)); },
                LexMode::StringMode | LexMode::QuotesMode(false) | LexMode::OpOrPuncMode(true) => self.panic(),
            },

            //comment mode, ignores everything
            LexMode::CommentMode => self.lex_state = LexState::Idle,

            //there is no way the previous state can be unknown mode lmao, something MUST have gone SERIOUSLY WRONG
            LexMode::UnknownMode => self.panic(),

        }
        Ok(())

    }

    fn execute(&mut self, c: char) -> Result<(), PreRuntimeError> {

        match &self.lex_state {
            LexState::PushChar => self.push_char(c),
            LexState::EmitToken => self.emit_token(),
            LexState::EmitAndPush => {
                self.emit_token()?;
                self.push_char(c)?;
                Ok(())
            },
            LexState::Idle => Ok(()), //Do nothing lmao
        }

    }

    fn push_char(&mut self, c: char) -> Result<(), PreRuntimeError> {
        if self.lex_mode == LexMode::StringMode && self.escape_mode > 0 {
            if self.escape_mode == 1 { //c is the escape character
                self.escape_mode += 1;
            } else {
                if let Some(ch) = tokenlist::ESCAPE_SEQ.get(&c) { 
                    self.buffer.push(ch.clone());
                } else {
                    return Err(self.raise_error(SyntaxErrorType::UnknownEscapeSeqError));
                }
                self.escape_mode = 0;
            }
        } else {
            self.buffer.push(c);
        }
        Ok(())
    }

    fn emit_token(&mut self) -> Result<(), PreRuntimeError> {
        match &self.prev_mode {
            LexMode::WordMode => {
                if let Some(keyword_token) = tokenlist::KEYWORDS.get(self.buffer.as_str()) { self.tokens.push(keyword_token.clone()); }
                else if let Some(datatype_token) = tokenlist::DATATYPES.get(self.buffer.as_str()) { self.tokens.push(datatype_token.clone()); }
                else if let Some(boolean_token) = tokenlist::BOOL_VALUES.get(self.buffer.as_str()) { self.tokens.push(boolean_token.clone()); }
                else { self.tokens.push(Token::Identifier(self.buffer.clone())); }
            },
            LexMode::OpOrPuncMode(_) => {
                if let Some(op) = tokenlist::OPS_AND_PUNCS.get(self.buffer.as_str()) { self.tokens.push(op.clone()); }
                else { return Err(self.raise_error(SyntaxErrorType::InvalidOperationError)); }
            },
            LexMode::QuotesMode(_) => {
                let thisschar = self.buffer.chars().next();
                if let Some(ch) = thisschar {
                    if ch == tokenlist::STR_QUOTES { self.tokens.push(Token::Operator(OpType::StrQuotes)); }
                    else if ch == tokenlist::CHAR_QUOTES { self.tokens.push(Token::Operator(OpType::CharQuotes)); }
                    else { self.panic(); }
                } else {
                    self.panic();
                }
            },
            LexMode::IntLiteralMode => {
                match self.buffer.parse::<u64>() {
                    Ok(val) => self.tokens.push(Token::Literal(Literal::Int(val))),
                    Err(_) => { return Err(self.raise_error(SyntaxErrorType::UnknownTypeError(DataType::Int64))); },
                }
            },
            LexMode::FloatLiteralMode => {
                match self.buffer.parse::<f64>() {
                    Ok(val) => self.tokens.push(Token::Literal(Literal::Float(val))),
                    Err(_) => { return Err(self.raise_error(SyntaxErrorType::UnknownTypeError(DataType::Float64))); },
                }
            },
            LexMode::StringMode => {
                self.tokens.push(Token::Literal(Literal::Str(self.buffer.clone())));
            },
            _ => { /*should ideally not happen and panic instead, but leaving it blank for now*/ },
        }
        self.buffer.clear();
        Ok(())
    }

    // helper functions
    fn raise_error(&self, error_type: SyntaxErrorType) -> PreRuntimeError {
        PreRuntimeError {
            errortype: PreRuntimeErrorType::SyntaxError(error_type),
            line: self.line,
            column: self.column,
        }
    }

    fn panic(&self) {
        panic!("This message shouldn't show up. If it does, then something is seriously wrong. Check your code! Here's some helpful info :D - 
        \nLine: {}, 
        \nColumn: {}, 
        \nTokens: {:?}, 
        \nbuffer: {}, 
        \nPrevMode: {}, 
        \nLexMode: {}", 
        self.line, self.column, self.tokens, self.buffer, self.prev_mode, self.lex_mode);
    }

}