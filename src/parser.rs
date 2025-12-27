use core::slice;
use std::{collections::{self, BTreeMap, HashMap}, fmt::Debug, iter::Peekable, sync::{Arc, LazyLock, Mutex}, vec};

use crate::lexer::{self, ConstantType, KeywordType, OperatorType, SeparatorType, Token, TokenType};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum ExprNodeType{
    ADD,
    SUB,
    MUL,
    DIV,
    FUNCCALL,//f(expr)
    PROPERTYVISIT,//a.b
    COMMALIST,// expr, expr...
    VALUE// 一个单纯的值或者标识符
}
#[derive(Clone)]
struct ExprNode{
    nodetype:ExprNodeType,
    left:Option<Box<ExprNode>>,
    value:Option<String>,
    right:Option<Box<ExprNode>>
}
impl Debug for ExprNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.nodetype {
            ExprNodeType::VALUE=>{
                f.write_str(&format!("VALUE({})",self.value.as_ref().unwrap()))
            },
            _=>{
                f.write_str(&format!("{:?}(",self.nodetype))?;
                if let Some(leftnode) = &self.left {
                    f.write_str(&format!("{:?}",leftnode))?;
                }
                f.write_str(",")?;
                if let Some(rightnode) = &self.right {
                    f.write_str(&format!("{:?}",rightnode))?;
                }
                f.write_str(")")
            }
        }
    }
}
impl ExprNode {
    pub fn value_node(token:&Token, tokens:&mut Peekable<slice::Iter<Token>>)->Self{
        Self{
            nodetype: ExprNodeType::VALUE,
            left: None,
            value: Some(token.value.clone()),
            right: None,
        }
    }

    pub fn minus_node(token:&Token, tokens:&mut Peekable<slice::Iter<Token>>)->Self{
        Self{
            nodetype: ExprNodeType::VALUE,
            left: scan_expr(tokens, PRECEDENCES[&TokenType::Operator(OperatorType::Minus)])
            .map_or_else(|| None, |v| Some(Box::new(v))),
            value: None,
            right: None,
        }
    }
    // infix handlers
    pub fn add_node(left:ExprNode, tokens:&mut Peekable<slice::Iter<Token>>)->Self{
        Self{
            nodetype: ExprNodeType::ADD,
            left: Some(Box::new(left)),
            value: None,
            right: scan_expr(tokens, PRECEDENCES[&TokenType::Operator(OperatorType::Plus)])
            .map_or_else(|| None, |v| Some(Box::new(v))),

        }
    }
    pub fn sub_node(left:ExprNode, tokens:&mut Peekable<slice::Iter<Token>>)->Self{
        Self{
            nodetype: ExprNodeType::SUB,
            left: Some(Box::new(left)),
            value: None,
            right: scan_expr(tokens, PRECEDENCES[&TokenType::Operator(OperatorType::Minus)])
            .map_or_else(|| None, |v| Some(Box::new(v))),
            
        }
    }
    pub fn multiply_node(left:ExprNode, tokens:&mut Peekable<slice::Iter<Token>>)->Self{
        Self{
            nodetype: ExprNodeType::MUL,
            left: Some(Box::new(left)),
            value: None,
            right: scan_expr(tokens, PRECEDENCES[&TokenType::Operator(OperatorType::Multiply)])
            .map_or_else(|| None, |v| Some(Box::new(v))),
        }
    }
    pub fn divide_node(left:ExprNode, tokens:&mut Peekable<slice::Iter<Token>>)->Self{
        Self{
            nodetype: ExprNodeType::DIV,
            left: Some(Box::new(left)),
            value: None,
            right: scan_expr(tokens, PRECEDENCES[&TokenType::Operator(OperatorType::Divide)])
            .map_or_else(|| None, |v| Some(Box::new(v))),
        }
    }
}
static PRECEDENCES:LazyLock<BTreeMap<TokenType,usize>>=LazyLock::new(||{BTreeMap::from([
    (TokenType::Operator(OperatorType::Plus),120),
    (TokenType::Operator(OperatorType::Minus),120),
    (TokenType::Operator(OperatorType::Comma),100),
    (TokenType::Operator(OperatorType::Multiply),130),
    (TokenType::Operator(OperatorType::Divide),130),
    (TokenType::Separator(SeparatorType::Semicolon),0)
])});
type PrefixHandler=fn(&Token,&mut Peekable<slice::Iter<Token>>)->ExprNode;
type InfixHandler=fn(ExprNode, &mut Peekable<slice::Iter<Token>>)->ExprNode;
static PREFIX_HANDLERS:LazyLock<BTreeMap<TokenType, PrefixHandler>>=LazyLock::new(|| 
    BTreeMap::from([
    (TokenType::Identifier, ExprNode::value_node as PrefixHandler),
    (TokenType::Constant(ConstantType::Integer), ExprNode::value_node),
    (TokenType::Operator(OperatorType::Minus), ExprNode::minus_node),

]));
static INFIX_HANDLERS:LazyLock<BTreeMap<TokenType,InfixHandler>>=LazyLock::new(
    || 
        BTreeMap::from([
            (TokenType::Operator(OperatorType::Plus), ExprNode::add_node as InfixHandler),
            (TokenType::Operator(OperatorType::Minus), ExprNode::sub_node as InfixHandler),
            (TokenType::Operator(OperatorType::Multiply), ExprNode::multiply_node as InfixHandler),
            (TokenType::Operator(OperatorType::Divide), ExprNode::divide_node as InfixHandler),
        ])
    
);
/// scan an expression using Pratt Parsing algorithm.
fn scan_expr(tokens:&mut Peekable<slice::Iter<Token>>,precedence:usize)->Option<ExprNode>{
    //consume one token
    let lefttoken=tokens.next();
    if lefttoken.is_none() {
        return None;
    }
    let lefttoken=lefttoken.unwrap();
    //get the left node by calling the handler of this kind of token
    if PREFIX_HANDLERS.iter().find(|(toktype,handler)| *toktype==&lefttoken.kind).is_none() {
        return None;
    }
    let mut left=PREFIX_HANDLERS[&lefttoken.kind](lefttoken, tokens);
    if cfg!(test) {
        println!("taking left token {:?} to make prefix node {:?}",lefttoken,left);
    }
    loop {
        
        let righttoken=if let Some(&tok) = tokens.peek() {
            tok
        } else {
            break;
        }.clone();
        if cfg!(test) {
            println!("peeking token {:?}",righttoken);
        }
        if PRECEDENCES.iter().find(|(toktyp,_)| *toktyp==&righttoken.kind).is_none() {
            // not an infix operator
            println!("scan_expr met an unknown infix token {:?}",righttoken);
            return None;
        }
        // if the precedence of the new token is higher than ours, take it.
        if PRECEDENCES[&righttoken.kind]<=precedence {
            // precedence lower than us. dont take it
            if cfg!(test) {
                println!("precedence too low. abandon.");
            }
            break;
        }
        tokens.next();
        // confirmed, take it. calc the bigger expr node with left node taken
        // sothat we insert the prior left node into the tree of the new one.
        left=INFIX_HANDLERS[&righttoken.kind](left,tokens);
        if cfg!(test) {
            println!("making a bigger node {:?}",left);
        }
        // a new round and we make a bigger tree
    }
    Some(left)
}
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum StatementType {
    Definition,
    Assign,
    SingleExpr
}
#[derive(Clone, Debug)]
struct StatementNode{
    stmttype:StatementType,
    typekw:Token,
    id:Token,
    expr:ExprNode
}
impl StatementNode {
    pub const fn new(stmttype:StatementType,typekw:Token,id:Token,expr:ExprNode)->Self{
        Self{
            stmttype,
            typekw,
            id,
            expr
        }
    }
}
#[derive(Clone)]
struct FunctionNode{
    fnkw:Token,
    id:Token,
    returntypekw:Token,
    params:Vec<(Token,Token)>,// (param id, param type)
    stmts:Vec<StatementNode>
}
impl Debug for FunctionNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("====FunctionNode===\n{} {}(",self.fnkw.value, self.id.value))?;
        for (id,typ) in self.params.iter() {
            f.write_str(&format!("{}:{}, ",id.value, typ.value))?;
        }
        f.write_str(&format!("):{}{{\n",self.returntypekw.value))?;
        for stmt in self.stmts.iter() {
            f.write_str(&format!("{:?}\n",stmt))?;
        }
        f.write_str("}\n================\n")
    }
}
impl FunctionNode {
    pub fn new(fnkw:Token, id:Token, returntypekw:Token, params:Vec<(Token,Token)>, stmts:Vec<StatementNode>)->Self{
        Self{
            fnkw,
            id,
            returntypekw,
            params,
            stmts
        }
    }
}
/// scan a specific type of token from the tokens.
fn scan_token(toktype:TokenType, tokens:&mut Peekable<slice::Iter<Token>>)->Option<Token>{
    if let Some(tok) = tokens.peek() {
        if cfg!(test) {
            println!("peeking token {:?}, token type {}, required {}",tok, tok.kind, toktype);
        }
        if tok.kind==toktype {
            return tokens.next().cloned();
        }
    }
    None
}
/// scan a statement from the tokens.
fn scan_stmt(tokens:&mut Peekable<slice::Iter<Token>>)->Option<StatementNode> {
    //1. define
    // backup the iter in case the scanning fails
    let mut backupiter=tokens.clone();
    let typekw=scan_token(TokenType::Keyword(KeywordType::Int), &mut backupiter);
    let id=scan_token(TokenType::Identifier, &mut backupiter);
    let eqop=scan_token(TokenType::Operator(OperatorType::Assign), &mut backupiter);
    let rexpr=scan_expr(&mut backupiter, 0);
    let ending=scan_token(TokenType::Separator(SeparatorType::Semicolon), &mut backupiter);
    if typekw.is_some()&&id.is_some()&&eqop.is_some()&&rexpr.is_some()&&ending.is_some() {
        *tokens=backupiter;
        return Some(StatementNode::new(StatementType::Definition, typekw.unwrap(), id.unwrap(), rexpr.unwrap()));
    }

    //2. assign
    let mut backupiter=tokens.clone();
    let id=scan_token(TokenType::Identifier, &mut backupiter);
    let eqop=scan_token(TokenType::Operator(OperatorType::Assign), &mut backupiter);
    let rexpr=scan_expr(&mut backupiter, 0);
    let ending=scan_token(TokenType::Separator(SeparatorType::Semicolon), &mut backupiter);
    if id.is_some()&&eqop.is_some()&&rexpr.is_some()&&ending.is_some() {
        *tokens=backupiter;
        return Some(StatementNode::new(StatementType::Assign, Token::new(TokenType::Keyword(KeywordType::Int), ""), id.unwrap(), rexpr.unwrap()));
    }

    //3. single expr
    let mut backupiter=tokens.clone();
    let rexpr=scan_expr(&mut backupiter, 0);
    let ending=scan_token(TokenType::Separator(SeparatorType::Semicolon), &mut backupiter);
    if rexpr.is_some()&&ending.is_some() {
        *tokens=backupiter;
        return Some(StatementNode::new(StatementType::SingleExpr, Token::new(TokenType::Keyword(KeywordType::Int), ""), Token::new(TokenType::Identifier, ""), rexpr.unwrap()));
    }
    None
}
/// scan a function from the tokens.
fn scan_func(tokens:&mut Peekable<slice::Iter<Token>>)->Option<FunctionNode>{
    /*
    function:
    fn id (id:typekw,...):typekw {
        stmt;
        ...
    }
    */
    let mut backupiter=tokens.clone();
    let fnkw=scan_token(TokenType::Keyword(KeywordType::Fn), &mut backupiter);
    let id=scan_token(TokenType::Identifier, &mut backupiter);
    let openparen=scan_token(TokenType::Separator(SeparatorType::OpenParen), &mut backupiter);
    // checkpoint
    if fnkw.is_none()||id.is_none()||openparen.is_none() {
        return None;
    }
    let mut params:Vec<(Token,Token)>=Vec::new();
    // scan the param list
    loop {
        let paramid=scan_token(TokenType::Identifier, &mut backupiter);
        let colonsep=scan_token(TokenType::Separator(SeparatorType::Colon), &mut backupiter);
        let paramtype=scan_token(TokenType::Keyword(KeywordType::Int), &mut backupiter);
        if paramid.is_none()||colonsep.is_none()||paramtype.is_none() {
            break;
        }
        params.push((paramid.unwrap(), paramtype.unwrap()));
        let comma=scan_token(TokenType::Operator(OperatorType::Comma), &mut backupiter);
        if comma.is_none() {
            break;
        }
    }
    let closeparentok=scan_token(TokenType::Separator(SeparatorType::CloseParen), &mut backupiter);
    let colontok=scan_token(TokenType::Separator(SeparatorType::Colon), &mut backupiter);
    let returntypekw=scan_token(TokenType::Keyword(KeywordType::Int), &mut backupiter);
    let openbracetok=scan_token(TokenType::Separator(SeparatorType::OpenBrace), &mut backupiter);
    //checkpoint
    if closeparentok.is_none()||colontok.is_none()||returntypekw.is_none()||openbracetok.is_none() {
        return None;
    }
    // scan the stmts
    let mut stmts:Vec<StatementNode>=Vec::new();
    loop {
        let stmt=scan_stmt(&mut backupiter);
        if stmt.is_none() {
            break;
        }
        stmts.push(stmt.unwrap());
    }
    let closebracetok=scan_token(TokenType::Separator(SeparatorType::CloseBrace), &mut backupiter);
    // last we check the closing brace
    if closebracetok.is_none() {
        return None;
    }
    *tokens=backupiter;
    Some(FunctionNode::new(fnkw.unwrap(), id.unwrap(), returntypekw.unwrap(), params, stmts))
}
#[test]
fn test_func_scanner(){
    let rawfuncstr="fn main():int {
        int a=1;
        int b=2;
        int c=3;
        c=a+b;
        a;
    }";
    println!("Function Tokens:");
    let tokens=lexer::do_lex(rawfuncstr);
    // let tokens=vec![Token::new(TokenType::Identifier, "a"),
    // Token::new(TokenType::Operator(OperatorType::Plus), "+"),
    // Token::new(TokenType::Identifier, "b")];
    let mut tokiter=tokens.iter().peekable();
    let func=scan_func(&mut tokiter);
    if let Some(funcnode) = func {
        println!("{:?}",funcnode);
        assert!(true);
    }else {
        println!("func scanning failed");
        assert!(false);
    }
}
#[test]
fn test_stmt_scanner(){
    let rawstmtstr="int a = b + c * 2;";
    println!("Statement Tokens:");
    let tokens=lexer::do_lex(rawstmtstr);
    // let tokens=vec![Token::new(TokenType::Identifier, "a"),
    // Token::new(TokenType::Operator(OperatorType::Plus), "+"),
    // Token::new(TokenType::Identifier, "b")];
    let mut tokiter=tokens.iter().peekable();
    let stmt=scan_stmt(&mut tokiter);
    if let Some(stmtnode) = stmt {
        println!("{:?}",stmtnode);
        assert!(true);
    }else {
        println!("stmt scanning failed");
        assert!(false);
    }
}
#[test]
fn test_expr_scanner(){
    let rawexprstr="b+a*c-1";
    println!("Expr Tokens:");
    let tokens=lexer::do_lex(rawexprstr);
    // let tokens=vec![Token::new(TokenType::Identifier, "a"),
    // Token::new(TokenType::Operator(OperatorType::Plus), "+"),
    // Token::new(TokenType::Identifier, "b")];
    let mut tokiter=tokens.iter().peekable();
    let expr=scan_expr(&mut tokiter, 0);
    if let Some(exprs) = expr {
        println!("{:?}",exprs);
        assert!(true);
    }else {
        println!("expr scanning failed");
        assert!(false);
    }
}