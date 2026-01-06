use std::collections::{HashMap, HashSet};
use std::sync::LazyLock;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {

    // identifiers and literals
    Identifier(String),
    Literal(Literal),

    // keywords and types
    Keyword(Keyword),
    DataType(DataType),

    // operators and punctuations
    Operator(OpType),

    // other
    EOF,

}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {

    Int(u64),
    Float(f64),
    Bool(bool),
    Str(String),

}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Keyword {

    //functional and definitions
    Func,
    Var,
    Param,
    Const,
    Return,

    //control flow
    If,
    Elif,
    Else,

    //loop flow
    While,
    Do,
    For,

    //container
    In,

}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DataType {

    //integer
    Int8,
    Int16,
    Int32,
    Int64,

    //unsigned integer
    UInt8,
    UInt16,
    UInt32,
    UInt64,

    //floating point
    Float32,
    Float64,
    
    //bool
    Bool,

    //char and string
    Char,
    Str,

    //void return type
    Void, // ~

}

impl std::fmt::Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            DataType::Int8 => "int8",
            DataType::Int16 => "int16",
            DataType::Int32 => "int32",
            DataType::Int64 => "int64",
            DataType::UInt8 => "uint8",
            DataType::UInt16 => "uint16",
            DataType::UInt32 => "uint32",
            DataType::UInt64 => "uint64",
            DataType::Float32 => "float32",
            DataType::Float64 => "float64",
            DataType::Bool => "bool",
            DataType::Char => "char",
            DataType::Str => "str",
            DataType::Void => "~",
        };
        write!(f, "{s}")
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OpType {

    // unary and binary basic ops
    Plus,       // +
    Minus,      // -

    // arithmetic
    Mul,        // *
    Div,        // /
    Mod,        // %
    Exp,        // **

    // logic
    Not,        // `!
    Or,         // `|
    And,        // `&
    Xor,        // `^

    //bitwise
    BitNot,     // !
    BitOr,      // |
    BitAnd,     // &
    BitXor,     // ^
    ShiftLeft,  // <<
    ShiftRight, // >>

    //assignment
    Assign,        // =
    PlusAssign,    // +=
    SubAssign,     // -=
    MulAssign,     // *=
    DivAssign,     // /=

    // comparison
    Equal,      // ==
    NotEqual,   // !=
    Greater,    // >
    Lesser,     // <
    GTEq,       // >=
    LTEq,       // <=

    // format
    FmtOpen,    // ${
    FmtClose,   // $}

    // punctuations and symbols
    Period,        // .
    Arrow,         // ->
    Colon,         // :
    Semicolon,     // ;
    Comma,         // ,
    LParen,        // (
    RParen,        // )
    LBrace,        // {
    RBrace,        // }
    LSquare,       // [
    RSquare,       // ]
    VerticalBar,   // |
    Ampersand,     // &
    BackTick,      // `
    Underscore,    // _
    CharQuotes,    // '
    StrQuotes,     // "
    ParenClosure,  // ()
    SquareClosure, // []
    BraceClosure,  // {}
    AngledClosure, // <>

}

pub static KEYWORDS: LazyLock<HashMap<&'static str, Token>> = LazyLock::new(|| {
    let mut map = HashMap::new();

    map.insert("func", Token::Keyword(Keyword::Func));
    map.insert("var", Token::Keyword(Keyword::Var));
    map.insert("param", Token::Keyword(Keyword::Param));
    map.insert("const", Token::Keyword(Keyword::Const));
    map.insert("return", Token::Keyword(Keyword::Return));
    map.insert("if", Token::Keyword(Keyword::If));
    map.insert("elif", Token::Keyword(Keyword::Elif));
    map.insert("else", Token::Keyword(Keyword::Else));
    map.insert("while", Token::Keyword(Keyword::While));
    map.insert("do", Token::Keyword(Keyword::Do));
    map.insert("for", Token::Keyword(Keyword::For));
    map.insert("in", Token::Keyword(Keyword::In));
    map
});

pub static DATATYPES: LazyLock<HashMap<&'static str, Token>> = LazyLock::new(|| {
    let mut map = HashMap::new();

    map.insert("int8", Token::DataType(DataType::Int8));
    map.insert("int16", Token::DataType(DataType::Int16));
    map.insert("int32", Token::DataType(DataType::Int32));
    map.insert("int64", Token::DataType(DataType::Int64));
    map.insert("uint8", Token::DataType(DataType::UInt8));
    map.insert("uint16", Token::DataType(DataType::UInt16));
    map.insert("uint32", Token::DataType(DataType::UInt32));
    map.insert("uint64", Token::DataType(DataType::UInt64));
    map.insert("float32", Token::DataType(DataType::Float32));
    map.insert("float64", Token::DataType(DataType::Float64));
    map.insert("bool", Token::DataType(DataType::Bool));
    map.insert("char", Token::DataType(DataType::Char));
    map.insert("str", Token::DataType(DataType::Str));
    map
});

pub static OPS_AND_PUNCS_CHARS: LazyLock<HashSet<char>> = LazyLock::new(|| {
    HashSet::from(['+', '/', '%', '|', '&', '^', '-', '*', '!', '>', '<', '~', '.', ':', ';', ',', '(', ')', '{', '}', '[', ']', '=', '`'])
});

pub static OPS_AND_PUNCS: LazyLock<HashMap<&'static str, Token>> = LazyLock::new(|| {
    let mut map = HashMap::new();

    //single 

    //operations
    map.insert("+", Token::Operator(OpType::Plus));
    map.insert("/", Token::Operator(OpType::Div));
    map.insert("%", Token::Operator(OpType::Mod));
    map.insert("|", Token::Operator(OpType::BitOr));
    map.insert("&", Token::Operator(OpType::BitAnd));
    map.insert("^", Token::Operator(OpType::BitXor));
    map.insert("-", Token::Operator(OpType::Minus));
    map.insert("*", Token::Operator(OpType::Mul));
    map.insert("!", Token::Operator(OpType::BitNot));
    map.insert(">", Token::Operator(OpType::Greater));
    map.insert("<", Token::Operator(OpType::Lesser));
    map.insert("~", Token::DataType(DataType::Void)); //exception

    //punctuations
    map.insert(".", Token::Operator(OpType::Period));
    map.insert(":", Token::Operator(OpType::Colon));
    map.insert(";", Token::Operator(OpType::Semicolon));
    map.insert(",", Token::Operator(OpType::Comma));
    map.insert("(", Token::Operator(OpType::LParen));
    map.insert(")", Token::Operator(OpType::RParen));
    map.insert("{", Token::Operator(OpType::LBrace));
    map.insert("}", Token::Operator(OpType::RBrace));
    map.insert("[", Token::Operator(OpType::LSquare));
    map.insert("]", Token::Operator(OpType::RSquare));
    map.insert("=", Token::Operator(OpType::Assign));
    map.insert("`", Token::Operator(OpType::BackTick));

    //paired

    //operations
    map.insert("**", Token::Operator(OpType::Exp));
    map.insert("`!", Token::Operator(OpType::Not));
    map.insert("`|", Token::Operator(OpType::Or));
    map.insert("`&", Token::Operator(OpType::And));
    map.insert("`^", Token::Operator(OpType::Xor));
    map.insert("==", Token::Operator(OpType::Equal));
    map.insert("!=", Token::Operator(OpType::NotEqual));
    map.insert(">=", Token::Operator(OpType::GTEq));
    map.insert("<=", Token::Operator(OpType::LTEq));
    map.insert("<<", Token::Operator(OpType::ShiftLeft));
    map.insert(">>", Token::Operator(OpType::ShiftRight));
    map.insert("+=", Token::Operator(OpType::PlusAssign));
    map.insert("-=", Token::Operator(OpType::SubAssign));
    map.insert("*=", Token::Operator(OpType::MulAssign));
    map.insert("/=", Token::Operator(OpType::DivAssign));
    
    //punctuations
    map.insert("->", Token::Operator(OpType::Arrow));
    map.insert("()", Token::Operator(OpType::ParenClosure));
    map.insert("[]", Token::Operator(OpType::SquareClosure));
    map.insert("{}", Token::Operator(OpType::BraceClosure));
    map.insert("<>", Token::Operator(OpType::AngledClosure));

    map
});


pub static ESCAPE_SEQ: LazyLock<HashMap<char, char>> = LazyLock::new(|| {
    let mut map = HashMap::new();

    map.insert('\\', '\\');
    map.insert('$', '$');
    map.insert('"', '\"');
    map.insert('n', '\n');
    map.insert('r', '\r');
    map.insert('t', '\t');
    map
});

pub static BOOL_VALUES: LazyLock<HashMap<&'static str, Token>> = LazyLock::new(|| {
    let mut map = HashMap::new();

    map.insert("true", Token::Literal(Literal::Bool(true)));
    map.insert("false", Token::Literal(Literal::Bool(false)));
    map
});

pub static WHITESPACE_CHARS: LazyLock<HashSet<char>> = LazyLock::new(|| {
    HashSet::from([' ', '\n', '\r', '\t'])
});

pub const ESCAPE_CHAR: char = '\\';
pub const CHAR_QUOTES: char = '\'';
pub const STR_QUOTES: char = '"';
pub const COMMENT_CHAR: char = '#';
pub const DECIMAL_CHAR: char = '.';
pub const UNDERSCORE: char = '_';
pub const FMT_INDICATOR: char = '$';
pub const FMT_OPEN: char = '{';
pub const FMT_CLOSE: char = '}';