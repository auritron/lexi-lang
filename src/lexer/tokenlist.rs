use std::collections::{HashMap, HashSet};
use std::sync::LazyLock;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {

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

pub static KEYWORDS: LazyLock<HashMap<&'static str, TokenKind>> = LazyLock::new(|| {
    let mut map = HashMap::new();

    map.insert("func", TokenKind::Keyword(Keyword::Func));
    map.insert("var", TokenKind::Keyword(Keyword::Var));
    map.insert("param", TokenKind::Keyword(Keyword::Param));
    map.insert("const", TokenKind::Keyword(Keyword::Const));
    map.insert("return", TokenKind::Keyword(Keyword::Return));
    map.insert("if", TokenKind::Keyword(Keyword::If));
    map.insert("elif", TokenKind::Keyword(Keyword::Elif));
    map.insert("else", TokenKind::Keyword(Keyword::Else));
    map.insert("while", TokenKind::Keyword(Keyword::While));
    map.insert("do", TokenKind::Keyword(Keyword::Do));
    map.insert("for", TokenKind::Keyword(Keyword::For));
    map.insert("in", TokenKind::Keyword(Keyword::In));
    map
});

pub static DATATYPES: LazyLock<HashMap<&'static str, TokenKind>> = LazyLock::new(|| {
    let mut map = HashMap::new();

    map.insert("int8", TokenKind::DataType(DataType::Int8));
    map.insert("int16", TokenKind::DataType(DataType::Int16));
    map.insert("int32", TokenKind::DataType(DataType::Int32));
    map.insert("int64", TokenKind::DataType(DataType::Int64));
    map.insert("uint8", TokenKind::DataType(DataType::UInt8));
    map.insert("uint16", TokenKind::DataType(DataType::UInt16));
    map.insert("uint32", TokenKind::DataType(DataType::UInt32));
    map.insert("uint64", TokenKind::DataType(DataType::UInt64));
    map.insert("float32", TokenKind::DataType(DataType::Float32));
    map.insert("float64", TokenKind::DataType(DataType::Float64));
    map.insert("bool", TokenKind::DataType(DataType::Bool));
    map.insert("char", TokenKind::DataType(DataType::Char));
    map.insert("str", TokenKind::DataType(DataType::Str));
    map
});

pub static OPS_AND_PUNCS_CHARS: LazyLock<HashSet<char>> = LazyLock::new(|| {
    HashSet::from(['+', '/', '%', '|', '&', '^', '-', '*', '!', '>', '<', '~', '.', ':', ';', ',', '(', ')', '{', '}', '[', ']', '=', '`'])
});

pub static OPS_AND_PUNCS: LazyLock<HashMap<&'static str, TokenKind>> = LazyLock::new(|| {
    let mut map = HashMap::new();

    //single 

    //operations
    map.insert("+", TokenKind::Operator(OpType::Plus));
    map.insert("/", TokenKind::Operator(OpType::Div));
    map.insert("%", TokenKind::Operator(OpType::Mod));
    map.insert("|", TokenKind::Operator(OpType::BitOr));
    map.insert("&", TokenKind::Operator(OpType::BitAnd));
    map.insert("^", TokenKind::Operator(OpType::BitXor));
    map.insert("-", TokenKind::Operator(OpType::Minus));
    map.insert("*", TokenKind::Operator(OpType::Mul));
    map.insert("!", TokenKind::Operator(OpType::BitNot));
    map.insert(">", TokenKind::Operator(OpType::Greater));
    map.insert("<", TokenKind::Operator(OpType::Lesser));
    map.insert("~", TokenKind::DataType(DataType::Void)); //exception

    //punctuations
    map.insert(".", TokenKind::Operator(OpType::Period));
    map.insert(":", TokenKind::Operator(OpType::Colon));
    map.insert(";", TokenKind::Operator(OpType::Semicolon));
    map.insert(",", TokenKind::Operator(OpType::Comma));
    map.insert("(", TokenKind::Operator(OpType::LParen));
    map.insert(")", TokenKind::Operator(OpType::RParen));
    map.insert("{", TokenKind::Operator(OpType::LBrace));
    map.insert("}", TokenKind::Operator(OpType::RBrace));
    map.insert("[", TokenKind::Operator(OpType::LSquare));
    map.insert("]", TokenKind::Operator(OpType::RSquare));
    map.insert("=", TokenKind::Operator(OpType::Assign));
    map.insert("`", TokenKind::Operator(OpType::BackTick));

    //paired

    //operations
    map.insert("**", TokenKind::Operator(OpType::Exp));
    map.insert("`!", TokenKind::Operator(OpType::Not));
    map.insert("`|", TokenKind::Operator(OpType::Or));
    map.insert("`&", TokenKind::Operator(OpType::And));
    map.insert("`^", TokenKind::Operator(OpType::Xor));
    map.insert("==", TokenKind::Operator(OpType::Equal));
    map.insert("!=", TokenKind::Operator(OpType::NotEqual));
    map.insert(">=", TokenKind::Operator(OpType::GTEq));
    map.insert("<=", TokenKind::Operator(OpType::LTEq));
    map.insert("<<", TokenKind::Operator(OpType::ShiftLeft));
    map.insert(">>", TokenKind::Operator(OpType::ShiftRight));
    map.insert("+=", TokenKind::Operator(OpType::PlusAssign));
    map.insert("-=", TokenKind::Operator(OpType::SubAssign));
    map.insert("*=", TokenKind::Operator(OpType::MulAssign));
    map.insert("/=", TokenKind::Operator(OpType::DivAssign));
    
    //punctuations
    map.insert("->", TokenKind::Operator(OpType::Arrow));
    map.insert("()", TokenKind::Operator(OpType::ParenClosure));
    map.insert("[]", TokenKind::Operator(OpType::SquareClosure));
    map.insert("{}", TokenKind::Operator(OpType::BraceClosure));
    map.insert("<>", TokenKind::Operator(OpType::AngledClosure));

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

pub static BOOL_VALUES: LazyLock<HashMap<&'static str, TokenKind>> = LazyLock::new(|| {
    let mut map = HashMap::new();

    map.insert("true", TokenKind::Literal(Literal::Bool(true)));
    map.insert("false", TokenKind::Literal(Literal::Bool(false)));
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