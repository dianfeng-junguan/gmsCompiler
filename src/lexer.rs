use std::{cmp, fmt::{Debug, Display}, vec::Vec};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd ,Ord)]
pub enum SeparatorType {
    Semicolon,
    Colon,
    Comma,
    OpenParen,//左小括号
    CloseParen,//右小括号
    OpenBrace,
    CloseBrace,
    Space,
    SlashN,
    Tab,
    CarriageReturn,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd ,Ord)]
pub enum OperatorType {
    //运算
    Plus,
    Minus,
    Multiply,
    Divide,
    Assign,
    //比较
    Equal,
    NotEqual,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    //分割
    Comma
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd ,Ord)]
pub enum KeywordType {
    Int,
    Return,
    If,
    Else,
    While,
    For,
    Fn
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd ,Ord)]
pub enum ConstantType {
    Integer
}
static KEYWORDS: [(KeywordType,&str);7] = [
    (KeywordType::Int, "int"),
    (KeywordType::Return, "return"),
    (KeywordType::If, "if"),
    (KeywordType::Else, "else"),
    (KeywordType::While, "while"),
    (KeywordType::For, "for"),
    (KeywordType::Fn, "fn")
];
static OPERATORS: [(OperatorType,&str);12] = [
    (OperatorType::Plus, "+"),
    (OperatorType::Minus, "-"),
    (OperatorType::Multiply, "*"),
    (OperatorType::Divide, "/"),
    (OperatorType::Assign, "="),
    (OperatorType::Equal, "=="),
    (OperatorType::NotEqual, "!="),
    (OperatorType::Greater, ">"),
    (OperatorType::Less, "<"),
    (OperatorType::GreaterEqual, ">="),
    (OperatorType::LessEqual, "<="),
    (OperatorType::Comma, ",")
];
static SEPARATORS: [(SeparatorType,&str);11] = [
    (SeparatorType::Semicolon, ";"),
    (SeparatorType::Colon, ":"),
    (SeparatorType::Comma, ","),
    (SeparatorType::OpenParen, "("),
    (SeparatorType::CloseParen, ")"),
    (SeparatorType::OpenBrace, "{"),
    (SeparatorType::CloseBrace, "}"),
    (SeparatorType::Space, " "),
    (SeparatorType::SlashN, "\n"),
    (SeparatorType::Tab, "\t"),
    (SeparatorType::CarriageReturn, "\r"),
];
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd ,Ord)]
pub enum TokenType {
    Keyword(KeywordType),
    Identifier,
    Constant(ConstantType),
    Operator(OperatorType),
    Separator(SeparatorType),
    //以下在lexer中用不到
    Expression,
    TypeKeyword,
}
impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{:?}",self))
    }
}
#[derive(Clone)]
pub struct Token {
    pub kind: TokenType,
    /// token的字符串值
    pub value: String,
}
impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("({}, {})", self.kind, self.value))
    }
}
impl Token {
    pub fn new(kind: TokenType, value: &str) -> Self {
        Token {
            kind,
            value: value.to_string(),
        }
    }
}
//在扫描中，一个种类允许出现的字符集合
static LETTERS_ALLOWED: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_";
static DIGITS_ALLOWED: &str = "0123456789";
static OPERATORS_ALLOWED: &str = "+-*/=<>!";
static SEPARATORS_ALLOWED: &str = ":;,(){} \n\t\r";
/// 扫描字符串开头连续的allowed字符，返回扫描到的字符数
/// # Arguments
/// * `toscan` - 要扫描的字符串
/// * `allowed` - 允许的字符集合
/// # Returns
/// 扫描到的字符数
fn scan_as_long_as_allowed(toscan:&str, allowed:&str, maxlen:usize) -> usize {
    let mut ptr:usize=0;
    for c in toscan.chars() {
        if allowed.contains(c) {
            ptr+=1;
        } else {
            break;
        }
        if ptr>=maxlen {
            break;
        }
    }
    ptr
}
/// 扫描指定允许出现的组合
fn scan(toscan:&str, possibleset:&Vec<&str>, maxlen:usize) -> usize {
    let realmaxlen=cmp::min(maxlen, toscan.len());
    for i in 0..realmaxlen {
        //从长度大的开始查找
        let slicedwd=&toscan[0..(realmaxlen-i)];
        if let Some(_) = possibleset.iter().find(|&&candi| candi==slicedwd) {
            return realmaxlen-i;
        }
    }
    0
}

pub fn do_lex(rawstr:&str)->Vec<Token>{
    let mut startptr:usize=0;
    let mut pioneerptr:usize=0;
    let mut tokens:Vec<Token>=Vec::new();
    while startptr<rawstr.len() {
        // 字母部分
        let firstchar=&rawstr[pioneerptr..pioneerptr+1];
        if LETTERS_ALLOWED.contains(firstchar) {
            let wordlen=scan_as_long_as_allowed(&rawstr[startptr..], LETTERS_ALLOWED, rawstr.len()-startptr);
            let word=&rawstr[startptr..startptr+wordlen];
            //检查是否是关键字
            if let Some((kwenum, kwstr)) = KEYWORDS.iter().find(|(kwenum,kwstr)| {
                *kwstr==word
            }) {
                //是关键字
                tokens.push(Token::new(TokenType::Keyword(*kwenum), kwstr));
            }else {
                //不是关键字，是标识符
                tokens.push(Token::new(TokenType::Identifier, word));
            }
            startptr+=wordlen;
            pioneerptr=startptr;
        }else if DIGITS_ALLOWED.contains(firstchar) {
            let wordlen=scan_as_long_as_allowed(&rawstr[startptr..], DIGITS_ALLOWED, rawstr.len()-startptr);
            let word=&rawstr[startptr..startptr+wordlen];
            tokens.push(Token::new(TokenType::Constant(ConstantType::Integer), word));
            startptr+=wordlen;
            pioneerptr=startptr;
        }else if OPERATORS_ALLOWED.contains(firstchar) {
            let all_possible_operators:Vec<&str>=OPERATORS.clone().into_iter().unzip::<OperatorType, &str, Vec<OperatorType>,Vec<&str>>().1;
            let wordlen=scan(&rawstr[startptr..], &all_possible_operators, 2);
            let word=&rawstr[startptr..startptr+wordlen];
            //检查是哪种操作符
            if let Some((openum, opstr)) = OPERATORS.iter().find(|(openum,opstr)| {
                *opstr==word
            }) {
                tokens.push(Token::new(TokenType::Operator(*openum), opstr));
            }else {
                panic!("Unknown operator: {}", word);
            }
            startptr+=wordlen;
            pioneerptr=startptr;
        }else if SEPARATORS_ALLOWED.contains(firstchar) {
            let all_possible_separators:Vec<&str>=SEPARATORS.clone().into_iter().unzip::<SeparatorType, &str, Vec<SeparatorType>,Vec<&str>>().1;
            let wordlen=scan(&rawstr[startptr..], &all_possible_separators, 1);
            let word=&rawstr[startptr..startptr+wordlen];
            //检查是哪种分隔符
            if let Some((sepenum, sepstr)) = SEPARATORS.iter().find(|(sepenum,sepstr)| {
                *sepstr==word
            }) {
                tokens.push(Token::new(TokenType::Separator(*sepenum), sepstr));
            }else {
                panic!("Unknown separator: {}", word);
            }
            startptr+=wordlen;
            pioneerptr=startptr;
        }else {
            // met unrecognized character
            panic!("Unrecognized character: {}", firstchar);
        }
    }
    println!("Tokens:");
    for token in tokens.iter() {
        println!("{:?}", token);
    }
    println!("Phase 2: removing whitespaces...");
    let tokens_no_whitespace:Vec<Token>=tokens.into_iter().filter(|token| {
        match token.kind {
            TokenType::Separator(SeparatorType::Space) |
            TokenType::Separator(SeparatorType::SlashN) |
            TokenType::Separator(SeparatorType::Tab) |
            TokenType::Separator(SeparatorType::CarriageReturn) => false,
            _ => true,
        }
    }).collect();
    println!("Tokens without whitespaces:");
    for token in tokens_no_whitespace.iter() {
        println!("{:?}", token);
    }
    tokens_no_whitespace
}