use core::slice;
use std::{collections::{self, BTreeMap, HashMap}, fmt::Debug, iter::Peekable, os::macos::raw::stat, sync::{Arc, LazyLock, Mutex}, vec};

use crate::{errs::{ERR_PARSER, cry_err}, lexer::{self, ConstantType, KeywordType, OperatorType, SeparatorType, Token, TokenType}};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum ExprNodeType{
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,
    EQUAL,
    GT,//greater than
    LT,
    GE,
    LE,
    FUNCCALL,//f(expr)
    PROPERTYVISIT,//a.b
    COMMALIST,// expr, expr...
    VALUE// 一个单纯的值或者标识符
}
#[derive(Clone)]
pub struct ExprNode{
    pub nodetype:ExprNodeType,
    pub left:Option<Box<ExprNode>>,
    pub value:Option<String>,
    pub right:Option<Box<ExprNode>>
}
impl Debug for ExprNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.nodetype {
            ExprNodeType::VALUE=>{
                let v=if self.value.is_some() { self.value.as_ref().unwrap() } else { "" };
                f.write_str(&format!("VALUE({})",v))
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
    pub fn empty()->Self{
        Self{
            nodetype: ExprNodeType::VALUE,
            left: None,
            value: Some(String::from("0")),
            right: None,
        }
    }
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

    pub fn left_paren_prefix(token:&Token, tokens:&mut Peekable<slice::Iter<Token>>)->Self{
        // scan the expr inside the paren
        let exprnode=scan_expr(tokens, 0).unwrap();
        // expect a closing paren
        let closeparen=scan_token(TokenType::Separator(SeparatorType::CloseParen), tokens);
        if closeparen.is_none() {
            cry_err(ERR_PARSER, "syntax error: missing closing parenthesis", 0, 0);
        }
        exprnode
    }

    pub fn right_paren_prefix(token:&Token, tokens:&mut Peekable<slice::Iter<Token>>)->Self{
        cry_err(ERR_PARSER, "syntax error: unexpected closing parenthesis", 0, 0);
        Self::empty()
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

    pub fn left_paren_infix(left:ExprNode, tokens:&mut Peekable<slice::Iter<Token>>)->Self{
        // function call
        // scan the argument list inside the paren
        // comma list
        let args=scan_expr(tokens, 0).unwrap();
        // expect a closing paren
        let closeparen=scan_token(TokenType::Separator(SeparatorType::CloseParen), tokens);
        if closeparen.is_none() {
            cry_err(ERR_PARSER,"syntax error: missing closing parenthesis in function call", 0, 0);
            return Self::empty();
        }
        Self{
            nodetype: ExprNodeType::FUNCCALL,
            left: Some(Box::new(left)),
            value: None,
            right: Some(Box::new(args)),
        }
    }
    pub fn right_paren_infix(left:ExprNode, tokens:&mut Peekable<slice::Iter<Token>>)->Self{
        cry_err(ERR_PARSER,"syntax error: unexpected closing parenthesis in infix position",0,0);
        Self::empty()
    }
}
/*
    some explanation:
    parensis
    prefix handler:
    left paren:
    when a parensis prefix handler is called, it means it follows an operator or it is the first token in an expr.
    because an operator's infix handler will call scan_expr to get its right operand
    and the parensis is fetched as the left token in scan_expr, then called the parensis prefix handler
    so we need to scan the expr inside the parensis, and then expect a closing parensis.
    finally return the expr node inside the parensis as the result of this prefix handler and drop the paren pair.
    
    right paren:
    the right paren is supposed to be handled and dropped by the hanlders of the left paren.
    if the prefix handler of the right paren is called, it means there is a syntax error in the source code.

    infix handler:
    left paren:
    when a parensis infix handler is called, it means it follows an identifier or constant.
    in this case, the paren is the start of a function call.
    so we need to scan the argument list inside the parensis, and then expect a closing parensis.
    finally return a FUNCCALL expr node with the function name as left child and the argument list as right child.

    right paren:
    the right paren is supposed to be handled and dropped by the hanlders of the left paren.
    if the infix handler of the right paren is called, it means there is a syntax error in the source code.

    precedence:
    the opening paren should have a very high precedence to ensure it is always taken when seen.
    yet the closing paren should have a very low precedence to ensure it is never taken as an infix operator.

    comma
    prefix_handler:
    the comma is not supposed to be handled in prefix position.
    
    infix_handler:
    the comma is used to separate expressions in a comma list.
    the comma list can be either a function argument list or simply a comma separated expression list in a bigger expression.
    the value of such expression is defined here the value of the rightmost expr-just like c.

    precedence:
    the comma should have a low precedence to ensure it is not taken when dealing with other operators.
*/
static PRECEDENCES:LazyLock<BTreeMap<TokenType,usize>>=LazyLock::new(||{BTreeMap::from([
    (TokenType::Operator(OperatorType::Plus),120),
    (TokenType::Operator(OperatorType::Minus),120),
    (TokenType::Operator(OperatorType::Multiply),130),
    (TokenType::Operator(OperatorType::Divide),130),
    (TokenType::Operator(OperatorType::Mod),130),
    (TokenType::Operator(OperatorType::Equal),110),
    (TokenType::Operator(OperatorType::Greater),110),
    (TokenType::Operator(OperatorType::Less),110),
    (TokenType::Operator(OperatorType::GreaterEqual),110),
    (TokenType::Operator(OperatorType::LessEqual),110),
    (TokenType::Separator(SeparatorType::OpenParen),1000),
    (TokenType::Separator(SeparatorType::CloseParen),0),
    (TokenType::Separator(SeparatorType::Comma),100),
    (TokenType::Separator(SeparatorType::Semicolon),0),
    (TokenType::Separator(SeparatorType::OpenBrace),0)
])});
type PrefixHandler=fn(&Token,&mut Peekable<slice::Iter<Token>>)->ExprNode;
type InfixHandler=fn(ExprNode, &mut Peekable<slice::Iter<Token>>)->ExprNode;
static PREFIX_HANDLERS:LazyLock<BTreeMap<TokenType, PrefixHandler>>=LazyLock::new(|| 
    BTreeMap::from([
    (TokenType::Identifier, ExprNode::value_node as PrefixHandler),
    (TokenType::Constant(ConstantType::Integer), ExprNode::value_node),
    (TokenType::Constant(ConstantType::Float), ExprNode::value_node),
    (TokenType::Constant(ConstantType::String), ExprNode::value_node),
    (TokenType::Operator(OperatorType::Minus), ExprNode::minus_node),
    (TokenType::Separator(SeparatorType::OpenParen), ExprNode::left_paren_prefix),
    (TokenType::Separator(SeparatorType::CloseParen), ExprNode::right_paren_prefix),
    (TokenType::Separator(SeparatorType::Comma), |token, tokiter|{
        cry_err(ERR_PARSER, "syntax error: unexpected comma in prefix position", 0, 0);
        return ExprNode::empty();
    })

]));
static INFIX_HANDLERS:LazyLock<BTreeMap<TokenType,InfixHandler>>=LazyLock::new(
    || 
        BTreeMap::from([
            (TokenType::Operator(OperatorType::Plus), ExprNode::add_node as InfixHandler),
            (TokenType::Operator(OperatorType::Minus), ExprNode::sub_node as InfixHandler),
            (TokenType::Operator(OperatorType::Multiply), ExprNode::multiply_node as InfixHandler),
            (TokenType::Operator(OperatorType::Divide), ExprNode::divide_node as InfixHandler),
            // compare
            (TokenType::Operator(OperatorType::Equal), |left, tokens|{
                ExprNode{
                    nodetype: ExprNodeType::EQUAL,
                    left: Some(Box::new(left)),
                    value: None,
                    right: scan_expr(tokens, PRECEDENCES[&TokenType::Operator(OperatorType::Equal)])
                    .map_or_else(|| None, |v| Some(Box::new(v))),
                }
            }),
            (TokenType::Operator(OperatorType::Greater), |left, tokens|{
                ExprNode{
                    nodetype: ExprNodeType::GT,
                    left: Some(Box::new(left)),
                    value: None,
                    right: scan_expr(tokens, PRECEDENCES[&TokenType::Operator(OperatorType::Greater)])
                    .map_or_else(|| None, |v| Some(Box::new(v))),
                }
            }),
            (TokenType::Operator(OperatorType::Less), |left, tokens|{
                ExprNode{
                    nodetype: ExprNodeType::LT,
                    left: Some(Box::new(left)),
                    value: None,
                    right: scan_expr(tokens, PRECEDENCES[&TokenType::Operator(OperatorType::Less)])
                    .map_or_else(|| None, |v| Some(Box::new(v))),
                }
            }),
            (TokenType::Operator(OperatorType::GreaterEqual), |left, tokens|{
                ExprNode{
                    nodetype: ExprNodeType::GE,
                    left: Some(Box::new(left)),
                    value: None,
                    right: scan_expr(tokens, PRECEDENCES[&TokenType::Operator(OperatorType::GreaterEqual)])
                    .map_or_else(|| None, |v| Some(Box::new(v))),
                }
            }),
            (TokenType::Operator(OperatorType::LessEqual), |left, tokens|{
                ExprNode{
                    nodetype: ExprNodeType::LE,
                    left: Some(Box::new(left)),
                    value: None,
                    right: scan_expr(tokens, PRECEDENCES[&TokenType::Operator(OperatorType::LessEqual)])
                    .map_or_else(|| None, |v| Some(Box::new(v))),
                }
            }),
            (TokenType::Operator(OperatorType::Mod), |left, tokens|{
                ExprNode{
                    nodetype: ExprNodeType::MOD,
                    left: Some(Box::new(left)),
                    value: None,
                    right: scan_expr(tokens, PRECEDENCES[&TokenType::Operator(OperatorType::Mod)])
                    .map_or_else(|| None, |v| Some(Box::new(v))),
                }
            }),
            (TokenType::Separator(SeparatorType::OpenParen), ExprNode::left_paren_infix as InfixHandler),
            (TokenType::Separator(SeparatorType::CloseParen), ExprNode::right_paren_infix as InfixHandler),
            (TokenType::Separator(SeparatorType::Comma), |left, tokens|{
                ExprNode{
                    nodetype: ExprNodeType::COMMALIST,
                    left: Some(Box::new(left)),
                    value: None,
                    right: scan_expr(tokens, PRECEDENCES[&TokenType::Separator(SeparatorType::Comma)])
                    .map_or_else(|| None, |v| Some(Box::new(v))),
                }
            }),
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
        cry_err(ERR_PARSER, &format!("cannot find prefix handler of this token :{}",lefttoken.kind), 0, 0);
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
            // println!("peeking token {:?}",righttoken);
        }
        if PRECEDENCES.iter().find(|(toktyp,_)| *toktyp==&righttoken.kind).is_none() {
            // not an infix operator
            cry_err(ERR_PARSER, &format!("scan_expr met an unknown infix token {:?}",righttoken), 0, 0);
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
pub enum StatementType {
    Definition,
    Assign,
    SingleExpr,
    If,
    While,
    Return
}
#[derive(Clone, Debug)]
pub struct StatementNode{
    pub stmttype:StatementType,
    pub typekw:Token,
    pub id:Token,
    pub expr:ExprNode,
    //if else area
    pub ifnode:Option<IfNode>,
    pub elseifnodes:Option<Vec<ElseIfNode>>,
    pub elsenode:Option<ElseNode>,
    // while area
    pub body:Vec<StatementNode>
}
impl StatementNode {
    pub const fn new(stmttype:StatementType,typekw:Token,id:Token,expr:ExprNode)->Self{
        Self{
            stmttype,
            typekw,
            id,
            expr,
            ifnode: None,
            elseifnodes: None,
            elsenode: None,
            body: vec![],
        }
    }
    pub fn new_if(ifnode:IfNode,elseifnodes:Option<Vec<ElseIfNode>>,elsenode:Option<ElseNode>)->Self{
        Self{
            stmttype:StatementType::If,
            typekw: Token::new(TokenType::Keyword(KeywordType::If), "if"),
            id: Token::new(TokenType::Identifier, ""),
            expr: ExprNode{ nodetype: ExprNodeType::VALUE, left: None, value: None, right: None },
            ifnode: Some(ifnode),
            elseifnodes: elseifnodes,
            elsenode: elsenode,
            body: vec![],
        }
    }
    pub fn new_while(condition:ExprNode, body:Vec<StatementNode>)->Self{
        Self{
            stmttype:StatementType::While,
            typekw: Token::new(TokenType::Keyword(KeywordType::While), "while"),
            id: Token::new(TokenType::Identifier, ""),
            expr: condition,
            ifnode: None,
            elseifnodes: None,
            elsenode: None,
            body,
        }
    }
}
#[derive(Clone)]
pub struct FunctionNode{
    pub fnkw:Token,
    pub id:Token,
    pub returntypekw:Token,
    pub params:Vec<(Token,Token)>,// (param id, param type)
    pub stmts:Vec<StatementNode>
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

#[derive(Clone, Debug)]
pub struct ASTNode{
    pub functions:Vec<FunctionNode>
}
impl ASTNode {
    pub fn new(functions:Vec<FunctionNode>)->Self{
        Self{
            functions
        }   
    }
}
/// scan a specific type of token from the tokens.
fn scan_token(toktype:TokenType, tokens:&mut Peekable<slice::Iter<Token>>)->Option<Token>{
    if let Some(tok) = tokens.peek() {
        if cfg!(test) {
            println!("peeking token {:?}, hoping {}",tok, toktype);
        }
        if tok.kind==toktype {
            return tokens.next().cloned();
        }
    }
    None
}
/// scan a statement from the tokens.
fn scan_stmt(tokens:&mut Peekable<slice::Iter<Token>>)->Option<StatementNode> {
    // early stop if meet tokens not supposed to be a part of statement
    if {
        let tok=tokens.peek()?;
        tok.kind==TokenType::Separator(SeparatorType::CloseBrace)||
        tok.kind==TokenType::Separator(SeparatorType::OpenBrace)
    } {
        return None;
    }


    //1. define
    // backup the iter in case the scanning fails
    let mut backupiter=tokens.clone();
    let typekw=scan_token(TokenType::Keyword(KeywordType::Let), &mut backupiter);
    let id=scan_token(TokenType::Identifier, &mut backupiter);
    let eqop=scan_token(TokenType::Operator(OperatorType::Assign), &mut backupiter);
    // checkpoint 
    if typekw.is_some()&&id.is_some()&&eqop.is_some() {
        let rexpr=scan_expr(&mut backupiter, 0);
        let ending=scan_token(TokenType::Separator(SeparatorType::Semicolon), &mut backupiter);
        if rexpr.is_some()&&ending.is_some() {
            *tokens=backupiter;
            return Some(StatementNode::new(StatementType::Definition, typekw.unwrap(), id.unwrap(), rexpr.unwrap()));
        }
    }

    
    if cfg!(test) {
        println!("definition scan failed, try return...");
    }


    // return 
    let mut backupiter=tokens.clone();
    let returnkw=scan_token(TokenType::Keyword(KeywordType::Return), &mut backupiter);
    if returnkw.is_some() {
        let rexpr=scan_expr(&mut backupiter, 0);
        let ending=scan_token(TokenType::Separator(SeparatorType::Semicolon), &mut backupiter);
        if rexpr.is_some()&&ending.is_some() {
            *tokens=backupiter;
            return Some(StatementNode::new(StatementType::Return, returnkw.unwrap(), Token::new(TokenType::Identifier, ""), rexpr.unwrap()));
        }else if rexpr.is_none()&&ending.is_some() {
            // empty return
            *tokens=backupiter;
            return Some(StatementNode::new(StatementType::Return, returnkw.unwrap(), Token::new(TokenType::Identifier, ""), ExprNode::empty()));
        }
    }

    //2. assign
    let mut backupiter=tokens.clone();
    let id=scan_token(TokenType::Identifier, &mut backupiter);
    let eqop=scan_token(TokenType::Operator(OperatorType::Assign), &mut backupiter);
    // checkpoint
    if id.is_some()&&eqop.is_some() {
        let rexpr=scan_expr(&mut backupiter, 0);
        let ending=scan_token(TokenType::Separator(SeparatorType::Semicolon), &mut backupiter);
        if rexpr.is_some()&&ending.is_some() {
            *tokens=backupiter;
            return Some(StatementNode::new(StatementType::Assign, Token::new(TokenType::Keyword(KeywordType::Int), ""), id.unwrap(), rexpr.unwrap()));
        }
    }

    if cfg!(test) {
        println!("assign scan failed, try if-elseif-else...");
    }

    //3. if-elseif-else
    let mut backupiter=tokens.clone();
    let ifnode=scan_if(&mut backupiter);
    let elseifnodes={
        let mut elseifnodes:Vec<ElseIfNode>=Vec::new();
        loop {
            let elseifnode=scan_elseif(&mut backupiter);
            if elseifnode.is_none() {
                break;
            }
            elseifnodes.push(elseifnode.unwrap());
        }
        elseifnodes
    };
    let elsenode=scan_else(&mut backupiter);
    if ifnode.is_some() {
        *tokens=backupiter;
        return Some(StatementNode::new_if(ifnode.unwrap(), if elseifnodes.len()==0 {None} else {Some(elseifnodes)}, elsenode));
    }

    if cfg!(test) {
        println!("if-elseif-else scan failed, try single expr...");
    }
    // while
    let mut backupiter=tokens.clone();
    let whiletok=scan_token(TokenType::Keyword(KeywordType::While), &mut backupiter);
    if whiletok.is_some() {
        let condition_expr=scan_expr(&mut backupiter, 0);
        let openbracetok=scan_token(TokenType::Separator(SeparatorType::OpenBrace), &mut backupiter);
        // checkpoint
        if condition_expr.is_none() {
            //incomplete while statement
            cry_err(ERR_PARSER, "incomplete while statment: lack of condition", 0, 0);
            return None;
        }else if openbracetok.is_none() {
            cry_err(ERR_PARSER, "incomplete while statment: lacking '{'", 0, 0);
            return None;
        }
        // scan the stmts
        let stmts=scan_stmts(&mut backupiter).unwrap();
        let closebracetok=scan_token(TokenType::Separator(SeparatorType::CloseBrace), &mut backupiter);
        //checkpoint
        if closebracetok.is_none() {
            cry_err(ERR_PARSER, "incomplete while statement: lacking }", 0, 0);
            return None;
        }
        *tokens=backupiter;
        return Some(StatementNode::new_while(condition_expr.unwrap(), stmts));
    }
    //4. single expr
    let mut backupiter=tokens.clone();
    let rexpr=scan_expr(&mut backupiter, 0);
    let ending=scan_token(TokenType::Separator(SeparatorType::Semicolon), &mut backupiter);
    if rexpr.is_some()&&ending.is_some() {
        *tokens=backupiter;
        return Some(StatementNode::new(StatementType::SingleExpr, Token::new(TokenType::Keyword(KeywordType::Int), ""), Token::new(TokenType::Identifier, ""), rexpr.unwrap()));
    }

    // cry_err(ERR_PARSER, "cannot parse the tokens as statement", 0, 0);
    None
}
fn scan_stmts(tokens:&mut Peekable<slice::Iter<Token>>)->Option<Vec<StatementNode>>{
    let mut stmts:Vec<StatementNode>=Vec::new();
    loop {
        let stmt=scan_stmt(tokens);
        if stmt.is_none() {
            break;
        }
        stmts.push(stmt.unwrap());
    }
    Some(stmts)
}
#[derive(Clone, Debug)]
pub struct IfNode{
    pub ifkw:Token,
    pub condition:ExprNode,
    pub stmts:Vec<StatementNode>
}
impl IfNode {
    pub fn new(ifkw:Token, condition:ExprNode, stmts:Vec<StatementNode>)->Self{
        Self{
            ifkw,
            condition,
            stmts
        }
    }
}
#[derive(Clone, Debug)]
pub struct ElseIfNode{
    pub elseifkw:Token,
    pub condition:ExprNode,
    pub stmts:Vec<StatementNode>
}
impl ElseIfNode {
    pub fn new(elseifkw:Token, condition:ExprNode, stmts:Vec<StatementNode>)->Self{
        Self{
            elseifkw,
            condition,
            stmts
        }
    }
}
#[derive(Clone, Debug)]
pub struct ElseNode{
    pub elsekw:Token,
    pub stmts:Vec<StatementNode>
}
impl ElseNode {
    pub fn new(elsekw:Token, stmts:Vec<StatementNode>)->Self{
        Self{
            elsekw,
            stmts
        }
    }
}
/// scan an if-else statement from the tokens.
fn scan_if(tokens:&mut Peekable<slice::Iter<Token>>)->Option<IfNode>{
    /*
    if expr {
        stmts
    } else if expr {
        stmts
    } else {
        stmts
    }
    */
    let mut backupiter=tokens.clone();
    let ifkw=scan_token(TokenType::Keyword(KeywordType::If), &mut backupiter);
    let condition_expr=scan_expr(&mut backupiter, 0);
    let openbracetok=scan_token(TokenType::Separator(SeparatorType::OpenBrace), &mut backupiter);
    // checkpoint
    if ifkw.is_none() {
        //it might not be an if statement
        return None;
    }else if condition_expr.is_none() {
        //incomplete if statement
        cry_err(ERR_PARSER, "incomplete if statment: lack of condition", 0, 0);
        return None;
    }else if openbracetok.is_none() {
        cry_err(ERR_PARSER, "incomplete if statment: lacking '{'", 0, 0);
        return None;
    }
    // scan the stmts
    let stmts=scan_stmts(&mut backupiter).unwrap();
    let closebracetok=scan_token(TokenType::Separator(SeparatorType::CloseBrace), &mut backupiter);
    //checkpoint
    if closebracetok.is_none() {
        cry_err(ERR_PARSER, "incomplete if statement: lacking }", 0, 0);
        return None;
    }
    *tokens=backupiter;
    Some(IfNode::new(ifkw.unwrap(), condition_expr.unwrap(), stmts))
}
fn scan_elseif(tokens:&mut Peekable<slice::Iter<Token>>)->Option<ElseIfNode>{
    /*
    else if expr {
        stmts
    }
    */
    let mut backupiter=tokens.clone();
    let elseifkw=scan_token(TokenType::Keyword(KeywordType::Else), &mut backupiter);
    let elseifkw=scan_token(TokenType::Keyword(KeywordType::If), &mut backupiter);
    // checkpoint
    if elseifkw.is_none(){
        return None;
    }
    let condition_expr=scan_expr(&mut backupiter, 0);
    let openbracetok=scan_token(TokenType::Separator(SeparatorType::OpenBrace), &mut backupiter);
    // checkpoint
    if condition_expr.is_none() {
        //incomplete else if statement
        cry_err(ERR_PARSER, "incomplete else if statment: lack of condition", 0, 0);
        return None;
    }else if openbracetok.is_none() {
        cry_err(ERR_PARSER, "incomplete else if statment: lacking '{'", 0, 0);
        return None;
    }
    // scan the stmts
    let stmts=scan_stmts(&mut backupiter).unwrap();
    let closebracetok=scan_token(TokenType::Separator(SeparatorType::CloseBrace), &mut backupiter);
    //checkpoint
    if closebracetok.is_none() {
        cry_err(ERR_PARSER, "incomplete else if statement: lacking }", 0, 0);
        return None;
    }
    *tokens=backupiter;
    Some(ElseIfNode::new(elseifkw.unwrap(), condition_expr.unwrap(), stmts))
}
fn scan_else(tokens:&mut Peekable<slice::Iter<Token>>)->Option<ElseNode>{
    /*
    else {
        stmts
    }
    */
    let mut backupiter=tokens.clone();
    let elsekw=scan_token(TokenType::Keyword(KeywordType::Else), &mut backupiter);
    let openbracetok=scan_token(TokenType::Separator(SeparatorType::OpenBrace), &mut backupiter);
    // checkpoint
    if elsekw.is_none() {
        return None;
    }else if openbracetok.is_none() {
        cry_err(ERR_PARSER, "incomplete else statment: lacking '{'", 0, 0);
        return None;
    }
    // scan the stmts
    let stmts=scan_stmts(&mut backupiter).unwrap();
    let closebracetok=scan_token(TokenType::Separator(SeparatorType::CloseBrace), &mut backupiter);
    //checkpoint
    if closebracetok.is_none() {
        cry_err(ERR_PARSER, "incomplete else statement: lacking }", 0, 0);
        return None;
    }
    *tokens=backupiter;
    Some(ElseNode::new(elsekw.unwrap(), stmts))
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
    if fnkw.is_none(){
        return None;
    }else if id.is_none() {
        cry_err(ERR_PARSER, "incomplete function definition: lack of function name", 0, 0);
        return None;
    }else if openparen.is_none() {
        cry_err(ERR_PARSER, "incomplete function definition: lack of '('", 0, 0);
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
        let comma=scan_token(TokenType::Separator(SeparatorType::Comma), &mut backupiter);
        if comma.is_none() {
            break;
        }
    }
    let closeparentok=scan_token(TokenType::Separator(SeparatorType::CloseParen), &mut backupiter);
    let colontok=scan_token(TokenType::Separator(SeparatorType::Colon), &mut backupiter);
    let returntypekw=scan_token(TokenType::Keyword(KeywordType::Int), &mut backupiter);
    let openbracetok=scan_token(TokenType::Separator(SeparatorType::OpenBrace), &mut backupiter);
    //checkpoint
    if closeparentok.is_none() {
        return None;
    }else if colontok.is_none(){
        cry_err(ERR_PARSER, "incomplete function: lacking ':'", 0, 0);
        return None;
    }else if returntypekw.is_none(){
        cry_err(ERR_PARSER, "incomplete function: lacking return type", 0, 0);
        return None;
    }else if openbracetok.is_none(){
        cry_err(ERR_PARSER, "incomplete function: lacking '{'", 0, 0);
        return None;
    }
    // scan the stmts
    let stmts=scan_stmts(&mut backupiter).unwrap();
    let closebracetok=scan_token(TokenType::Separator(SeparatorType::CloseBrace), &mut backupiter);
    // last we check the closing brace
    if closebracetok.is_none() {
        cry_err(ERR_PARSER, "incomplete function: lacking '}'", 0, 0);
        return None;
    }
    *tokens=backupiter;
    Some(FunctionNode::new(fnkw.unwrap(), id.unwrap(), returntypekw.unwrap(), params, stmts))
}
/// parser work-generate AST from tokens.
pub fn generate_ast(tokens:&Vec<Token>)->Option<ASTNode>{
    let mut tokiter=tokens.iter().peekable();
    let mut functions:Vec<FunctionNode>=Vec::new();
    loop {
        if let Some(funcnode) = scan_func(&mut tokiter) {
            functions.push(funcnode);
        }else {
            break;
        }
    }
    Some(ASTNode::new(functions))
}
#[test]
fn test_func_scanner(){
    let rawfuncstr="fn main():int {
        let a=1;
        let b=2;
        let c=3;
        c=a+b;
        a;
        f(a,1);
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
    let rawstmtstrs=vec![
        "let a = b + c * 2;",
        "a = b - 3 / d;",
        "foo(a, b, c);",
        "if a > b {
            c = a + b;
        } else if a == b {
            c = a - b;
        } else {
            c = 0;
        }"
    ];
    let mut statements:Vec<StatementNode>=Vec::new();
    for raws in rawstmtstrs.iter() {
        println!("Statement Tokens:");
        let tokens=lexer::do_lex(raws);
        // let tokens=vec![Token::new(TokenType::Identifier, "a"),
        // Token::new(TokenType::Operator(OperatorType::Plus), "+"),
        // Token::new(TokenType::Identifier, "b")];
        let mut tokiter=tokens.iter().peekable();
        let stmt=scan_stmt(&mut tokiter);
        if let Some(stmtnode) = stmt {
            statements.push(stmtnode);  
        }else {
            println!("stmt scanning failed");
            assert!(false);
        }
    }
    statements.iter().for_each(|stmt|{
        println!("{:?}",stmt);
    });
}
#[test]
fn test_expr_scanner(){
    let rawexprstr="b+(a*c-1),a+b,func(a,b,c);";
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