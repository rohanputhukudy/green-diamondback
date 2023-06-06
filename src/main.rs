use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

use im::HashMap;
use im::HashSet;

#[derive(Debug)]
enum Label { YES, NO, IFELSE, IFEND, LOOP, ENDLOOP, }

#[derive(Debug)]
enum Val { Reg(Reg), Imm(i64), NegRegOffset(Reg, i64), RegOffset(Reg, i64), Error, Label(Label, i64), Print, 
    Func(String), Main, }

#[derive(Debug)]
enum Reg { RAX, RSP, RDI, RBX, RSI, }

#[derive(Debug)]
enum Instr { 
    IMov(Val, Val), 
    IAdd(Val, Val), 
    ISub(Val, Val), 
    IMul(Val, Val), 
    SAR(Val, Val), 
    SHL(Val, Val),
    And(Val, Val),
    Neg(Val),
    Or(Val, Val),
    Xor(Val, Val),
    Test(Val, Val),
    Label(Val),
    Jmp(Val),
    Je(Val),
    Jne(Val),
    Jo(Val),
    Jg(Val),
    Jl(Val),
    Jge(Val),
    Jle(Val),
    Cmp(Val, Val),
    CMov(Val, Val),
    Push(Val),
    Pop(Val),
    Call(Val),
    Ret,
}

#[derive(Debug)]
enum Op1 { Add1, Sub1, IsNum, IsBool, Print, }

#[derive(Debug)]
enum Op2 { Plus, Minus, Times, Equal, Greater, GreaterEqual, Less, LessEqual, }

#[derive(Debug)]
enum Expr {
    Number(i64),
    Boolean(bool),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
    Block(Vec<Expr>),
    Func(String, Vec<Expr>),
}

#[derive(Debug)]
enum Defn {
    NoArg(String, Box<Expr>),
    Args(String, Vec<String>, Box<Expr>),
}

#[derive(Debug)]
enum Prog {
    NoFun(Box<Expr>),
    Funs(Vec<Defn>, Box<Expr>),
}

// Parses an S-exp. and returns that variable as a string if not a keyword
fn parse_var(s: &Sexp) -> String {
    let keywords:HashSet<String> = HashSet::from_iter(vec!["+".to_string(), "-".to_string(), 
                                                          "*".to_string(), "add1".to_string(), 
                                                          "sub1".to_string(), "let".to_string(), 
                                                          "input".to_string(), "=".to_string(), 
                                                          "<".to_string(), ">".to_string(), 
                                                          ">=".to_string(), "<=".to_string(), 
                                                          "isnum".to_string(), "isbool".to_string(), 
                                                          "set!".to_string(), "if".to_string(), 
                                                          "block".to_string(), "loop".to_string(), 
                                                          "break".to_string(), "true".to_string(), 
                                                          "false".to_string()]);
    match s {
        Sexp::Atom(S(id)) => {
            if !keywords.contains(id) {
                id.to_string()
            } else {
                panic!("Cannot use keyword {}", id)
            }
        }
        _ => panic!("Invalid"),
    }
}

// Parses an s-expression corresponding to a binding expression
fn parse_bind(s: &Sexp) -> (String, Expr) {
    match s {
        Sexp::List(vec) => {
            match &vec[..] {
                [s, e] => (parse_var(s), parse_expr(e)),
                _ => panic!("Invalid"),
            }
        },
        _ => panic!("Invalid"),
    }
}

// Parses a list of variable bindings by iteratively parsing each binding
fn parse_binders(v: &Vec<Sexp>) -> Vec<(String, Expr)> {
    let mut binders: Vec<(String, Expr)> = Vec::new();

    for s in v {
        binders.push(parse_bind(s));
    }
    binders
}

/* Parses a list of arguments iteratively parsing each of them
fn parse_args(v: &Vec<Sexp>) -> Vec<String> {
    let mut args: Vec<String> = Vec::new();

    for s in v {
        match s {
            Sexp::Atom(s) => args.push(s.to_string()),
            _ => panic!("Invalid"),
        }
    }
    args
}
*/

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => Expr::Number(i64::try_from(*n).unwrap()),
        Sexp::Atom(S(id)) => {
            if id == "true" { Expr::Boolean(true) }
            else if id == "false" { Expr::Boolean(false) }
            else { Expr::Id(id.to_string()) }
        }
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(op)), Sexp::List(v), e] if op == "let" => 
                    Expr::Let(parse_binders(v), Box::new(parse_expr(e))),
               [Sexp::Atom(S(op)), e] => {
                    match op.as_str() {
                        "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
                        "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
                        "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
                        "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
                        "print" => Expr::UnOp(Op1::Print, Box::new(parse_expr(e))),
                        "loop" => Expr::Loop(Box::new(parse_expr(e))),
                        "break" => Expr::Break(Box::new(parse_expr(e))),
                        _ => Expr::Func(op.to_string(), [e].into_iter().map(parse_expr).collect()),
                    }
                }
                [Sexp::Atom(S(op)), e1, e2] => {
                    match op.as_str() {
                        "+" => Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                        "-" => Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                        "*" => Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                        "=" => Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                        ">" => Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                        "<" => Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                        ">="=>Expr::BinOp(Op2::GreaterEqual,Box::new(parse_expr(e1)),Box::new(parse_expr(e2))),
                        "<=" => Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)),Box::new(parse_expr(e2))),
                        "set!" => Expr::Set(parse_var(e1), Box::new(parse_expr(e2))),
                        _ => Expr::Func(op.to_string(), [e1, e2].into_iter().map(parse_expr).collect()),
                    }
                }
                [Sexp::Atom(S(op)), e1, e2, e3] => {
                    match op.as_str() {
                        "if" => Expr::If(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)), 
                                         Box::new(parse_expr(e3))),
                        _ => Expr::Func(op.to_string(), [e1, e2, e3].into_iter().map(parse_expr).collect()),
                    }
                }
                [Sexp::Atom(S(op)), rest @ ..] => {
                    match op.as_str() {
                        "block" => Expr::Block(rest.into_iter().map(parse_expr).collect()),
                        _ => Expr::Func(op.to_string(), rest.into_iter().map(parse_expr).collect()),
                    }
                }
                 _ => panic!("Invalid"),
            }
        },
        _ => panic!("Invalid"),
    }
}

// Parses an s-expression of a function definition into a defn object
fn parse_defn(s: &Sexp) -> Defn {
    match s {
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(op)), Sexp::List(v), e]  if op == "fun" => {
                    if v.len() == 0 { panic!("Invalid"); }
                    else if v.len() == 1 { Defn::NoArg(parse_var(&v[0]), Box::new(parse_expr(e))) }
                    else { 
                        Defn::Args(parse_var(&v[0]), v[1..].into_iter().map(parse_var).collect(), 
                                      Box::new(parse_expr(e)))
                    }
                }
                _ => panic!("Invalid"),
            }
        }
        _ => panic!("Invalid"),
    }
}

// parses an s-expression of a system program into a Prog object
fn parse_prog(s: &Sexp, has_funcs: bool) -> Prog {
    if !has_funcs { Prog::NoFun(Box::new(parse_expr(s))) } 
    else {
        // if it has functions, it must be a list of s-expressions, where the last will be an expr
        // and the rest will be defn's. I.e., want to return a Prog::Funs(...) object
        match s {
            Sexp::List(vec) =>
                Prog::Funs(vec[0..vec.len()-1].into_iter().map(parse_defn).collect(), 
                           Box::new(parse_expr(&vec[vec.len()-1]))),
            _ => panic!("Invalid"),
        }
    }
}

fn label_to_str(l: &Label) -> String {
    match l {
        Label::YES => "yes".to_string(),
        Label::NO => "no".to_string(),
        Label::IFELSE => "ifelse".to_string(),
        Label::IFEND => "ifend".to_string(),
        Label::LOOP => "loop".to_string(),
        Label::ENDLOOP => "endloop".to_string(),
    }
}

fn reg_to_str(r: &Reg) -> String {
    match r {
        Reg::RAX => "rax".to_string(),
        Reg::RSP => "rsp".to_string(),
        Reg::RDI => "rdi".to_string(),
        Reg::RBX => "rbx".to_string(),
        Reg::RSI => "rsi".to_string(),
    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(r) => reg_to_str(r),
        Val::Imm(n) => n.to_string(),
        Val::NegRegOffset(r, n) => format!("[{reg} - {si}]", reg=reg_to_str(r), si=&(n*8).to_string()),
        Val::RegOffset(r, n) => format!("[{reg} + {si}]", reg=reg_to_str(r), si=&(n*8).to_string()),
        Val::Error => "throw_error".to_string(),
        Val::Print => "snek_print".to_string(),
        Val::Main => "our_code_starts_here".to_string(),
        Val::Label(l, n) => format!("{lbl}_{n}", lbl=label_to_str(l)),
        Val::Func(s) => format!("{func}", func=s.to_string()),
    }
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(v1, v2) => format!("mov {s1}, {s2}", s1=val_to_str(v1), s2=val_to_str(v2)),
        Instr::IAdd(v1, v2) => format!("add {s1}, {s2}", s1=val_to_str(v1), s2=val_to_str(v2)),
        Instr::ISub(v1, v2) => format!("sub {s1}, {s2}", s1=val_to_str(v1), s2=val_to_str(v2)),
        Instr::IMul(v1, v2) => format!("imul {s1}, {s2}", s1=val_to_str(v1), s2=val_to_str(v2)),
        Instr::SAR(v1, v2) => format!("sar {r}, {v}", r=val_to_str(v1), v=val_to_str(v2)),
        Instr::SHL(v1, v2) => format!("shl {r}, {v}", r=val_to_str(v1), v=val_to_str(v2)),
        Instr::And(v1, v2) => format!("and {r}, {v}", r=val_to_str(v1), v=val_to_str(v2)),
        Instr::Neg(v) => format!("neg {r}", r=val_to_str(v)),
        Instr::Or(v1, v2) => format!("or {r}, {v}", r=val_to_str(v1), v=val_to_str(v2)),
        Instr::Xor(v1, v2) => format!("xor {r}, {v}", r=val_to_str(v1), v=val_to_str(v2)),
        Instr::Label(v) => format!("{l}:", l=val_to_str(v)),
        Instr::Jmp(v) => format!("jmp {l}", l=val_to_str(v)),
        Instr::Je(v) => format!("je {l}", l=val_to_str(v)),
        Instr::Jne(v) => format!("jne {l}", l=val_to_str(v)),
        Instr::Jo(v) => format!("jo {l}", l=val_to_str(v)),
        Instr::Jg(v) => format!("jg {l}", l=val_to_str(v)),
        Instr::Jl(v) => format!("jl {l}", l=val_to_str(v)),
        Instr::Jge(v) => format!("jge {l}", l=val_to_str(v)),
        Instr::Jle(v) => format!("jle {l}", l=val_to_str(v)),
        Instr::Test(v1, v2) => format!("test {r}, {v}", r=val_to_str(v1), v=val_to_str(v2)),
        Instr::Cmp(v1, v2) => format!("cmp {r}, {v}", r=val_to_str(v1), v=val_to_str(v2)),
        Instr::CMov(v1, v2) => format!("cmove {r}, {v}", r=val_to_str(v1), v=val_to_str(v2)),
        Instr::Push(v) => format!("push {l}", l=val_to_str(v)),
        Instr::Pop(v) => format!("pop {l}", l=val_to_str(v)),
        Instr::Call(v) => format!("call {l}", l=val_to_str(v)),
        Instr::Ret => format!("ret"),
    }
}

// Checks that the value in the two registers are both numbers. If either of them are not, then it
// raises a dynamic error with error code 1 (invalid argument)
//
// ANDs v1 against v2, using RBX as an intermediary. Jumps based on result
// Assumption: v1 and v2 are registers or register offsets.
fn arith_type_check(v1: Val, v2: Val) -> Vec<Instr> {
    let mut instrs = Vec::new();
    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), v1));
    instrs.push(Instr::Or(Val::Reg(Reg::RBX), v2));
    instrs.push(Instr::Neg(Val::Reg(Reg::RBX)));
    instrs.push(Instr::Test(Val::Reg(Reg::RBX), Val::Imm(1)));
    instrs.push(Instr::IMov(Val::Reg(Reg::RSI), Val::Imm(1)));
    instrs.push(Instr::Jne(Val::Error));
    instrs
}

fn compile_expr(e: &Expr, mut si: i64, env: &HashMap<String, i64>, lbl: i64, target: &Val) -> Vec<Instr> {
    match e {
        // Shift numbers to the left by 1 so that the rightmost bit flags type (int vs bool)
        // x & 1 will equal 1 iff x is a bool
        Expr::Number(n) => {
            if *n < -4611686018427387904 || *n > 4611686018427387903 {
                panic!("Invalid");
            }
            vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n << 1))]
        }
        Expr::Boolean(b) => {
            if *b { vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))] } 
            else { vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))] }
        }
        Expr::Id(s) => {
            if s == "input" {
                vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))]
            } else {
                match env.get(s) {
                    Some(val) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, *val))],
                    None => panic!("Unbound variable identifier {}", s),
                }
            }
        }
        Expr::UnOp(op, e) => {
            let mut instrs = compile_expr(e, si, env, lbl+1, target);
            match op {
                Op1::Add1 => {
                    // Checks that the argument to add1 is indeed a number
                    instrs.extend(arith_type_check(Val::Reg(Reg::RAX), Val::Imm(2)));
                    instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));

                    // Sets error code to 2 and jumps if there is overflow
                    instrs.push(Instr::IMov(Val::Reg(Reg::RSI), Val::Imm(2)));
                    instrs.push(Instr::Jo(Val::Error));
                    instrs
                }
                Op1::Sub1 => {
                    // Checks that the argument to sub1 is indeed a number
                    instrs.extend(arith_type_check(Val::Reg(Reg::RAX), Val::Imm(2)));
                    instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));

                    // Sets error code to 2 and jumps if there is overflow
                    instrs.push(Instr::IMov(Val::Reg(Reg::RSI), Val::Imm(2)));
                    instrs.push(Instr::Jo(Val::Error));
                    instrs
                }
                Op1::IsNum => {
                    // Sends num => 000, and bool => 001
                    instrs.push(Instr::And(Val::Reg(Reg::RAX), Val::Imm(1)));
                    // Sends 000 => 000, and 001 => 010
                    instrs.push(Instr::SHL(Val::Reg(Reg::RAX), Val::Imm(1)));
                    // Sends 000 => 000, and 010 => (-2)
                    instrs.push(Instr::Neg(Val::Reg(Reg::RAX)));
                    // Sends 000 => 011, and (-2) => 001
                    instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(3)));
                    instrs
                }
                Op1::IsBool => {
                    // Starts the same as IsNum
                    instrs.push(Instr::And(Val::Reg(Reg::RAX), Val::Imm(1)));
                    // Flips the bit
                    instrs.push(Instr::Xor(Val::Reg(Reg::RAX), Val::Imm(1)));
                    // Does the rest of IsNum, but with the two values now flipped
                    instrs.push(Instr::SHL(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::Neg(Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(3)));
                    instrs
                }
                Op1::Print => {
                    let mut offset = si*8;
                    if si%2 == 1 { offset += 8; }

                    // shifts the location of the RSP register by the stack offset
                    instrs.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(offset)));
                    
                    // stores value of rdi on the stack before overwriting it
                    instrs.push(Instr::Push(Val::Reg(Reg::RDI)));

                    // Moves RAX into RDI as per Rust calling convention (rdi stores first arg)
                    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::Call(Val::Print));

                    // Restores the value of RDI and position of RSP
                    instrs.push(Instr::Pop(Val::Reg(Reg::RDI)));
                    instrs.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(offset)));
                    instrs
                }
            }
        }
        Expr::BinOp(Op2::Plus, e1, e2) => {
            let mut i1 = compile_expr(e1, si, env, lbl+1, target);
            let i2 = compile_expr(e2, si+1, env, lbl+2, target);
            i1.push(Instr::IMov(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX)));


            i1.extend(i2);
            // Checks that both args are numbers
            i1.extend(arith_type_check(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)));

            i1.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)));
            // Sets error code to 2 and jumps if there is overflow
            i1.push(Instr::IMov(Val::Reg(Reg::RSI), Val::Imm(2)));
            i1.push(Instr::Jo(Val::Error));

            i1
        }
        Expr::BinOp(Op2::Minus, e1, e2) => {
            let mut i2 = compile_expr(e2, si, env, lbl+1, target);
            let i1 = compile_expr(e1, si+1, env, lbl+2, target);
            i2.push(Instr::IMov(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX)));
            
            i2.extend(i1);
            // Checks that both args are numbers
            i2.extend(arith_type_check(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)));

            i2.push(Instr::ISub(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)));
            // Sets error code to 2 and jumps if there is overflow
            i2.push(Instr::IMov(Val::Reg(Reg::RSI), Val::Imm(2)));
            i2.push(Instr::Jo(Val::Error));

            i2
        }
        Expr::BinOp(Op2::Times, e1, e2) => {
            let mut i1 = compile_expr(e1, si, env, lbl+1, target);
            let i2 = compile_expr(e2, si+1, env, lbl+2, target);
            i1.push(Instr::IMov(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX)));
            i1.extend(i2);

            // Checks that both args are numbers
            i1.extend(arith_type_check(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)));

            i1.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)));
            // Right shift the result by 1 to accomodate our representation of integers as twice
            // their value. This rep. implies that products are 4 times their true values.
            i1.push(Instr::SAR(Val::Reg(Reg::RAX), Val::Imm(1)));
            
            // Sets error code to 2 and jumps if there is overflow
            i1.push(Instr::IMov(Val::Reg(Reg::RSI), Val::Imm(2)));
            i1.push(Instr::Jo(Val::Error));

            i1
        }
        Expr::BinOp(Op2::Equal, e1, e2) => {
            let mut i1 = compile_expr(e1, si, env, lbl+1, target);
            let i2 = compile_expr(e2, si+1, env, lbl+2, target);
            i1.push(Instr::IMov(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX)));
            i1.extend(i2);
            
            // Sets error code to 1 if the types of the arguments are mismatched
            // Checks RAX and [RSP-si*8] against each other, using RBX
            i1.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            i1.push(Instr::Xor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, si)));
            i1.push(Instr::Test(Val::Reg(Reg::RBX), Val::Imm(1)));
            i1.push(Instr::IMov(Val::Reg(Reg::RSI), Val::Imm(1)));
            i1.push(Instr::Jne(Val::Error));

            // proceeds if there is no type mismatch
            i1.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)));
            i1.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
            i1.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            i1.push(Instr::CMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));

            i1
        }
        Expr::BinOp(Op2::Greater, e1, e2) => {
            let mut i1 = compile_expr(e1, si, env, lbl+1, target);
            let i2 = compile_expr(e2, si+1, env, lbl+2, target);
            i1.push(Instr::IMov(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX)));
            i1.extend(i2);

            // Checks that both args are numbers
            i1.extend(arith_type_check(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)));

            // proceeds if there is no issue
            i1.push(Instr::Cmp(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX)));
            i1.push(Instr::Jg(Val::Label(Label::YES, lbl)));
            i1.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            i1.push(Instr::Jle(Val::Label(Label::NO, lbl)));
            i1.push(Instr::Label(Val::Label(Label::YES, lbl)));
            i1.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
            i1.push(Instr::Label(Val::Label(Label::NO, lbl)));

            i1
        }
        Expr::BinOp(Op2::Less, e1, e2) => {
            let mut i1 = compile_expr(e1, si, env, lbl+1, target);
            let i2 = compile_expr(e2, si+1, env, lbl+2, target);
            i1.push(Instr::IMov(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX)));
            i1.extend(i2);

            // Checks that both args are numbers
            i1.extend(arith_type_check(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)));

            // proceeds if there is no issue
            i1.push(Instr::Cmp(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX)));
            i1.push(Instr::Jl(Val::Label(Label::YES, lbl)));
            i1.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            i1.push(Instr::Jge(Val::Label(Label::NO, lbl)));
            i1.push(Instr::Label(Val::Label(Label::YES, lbl)));
            i1.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
            i1.push(Instr::Label(Val::Label(Label::NO, lbl)));

            i1
        }
        Expr::BinOp(Op2::GreaterEqual, e1, e2) => {
            let mut i1 = compile_expr(e1, si, env, lbl+1, target);
            let i2 = compile_expr(e2, si+1, env, lbl+2, target);
            i1.push(Instr::IMov(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX)));
            i1.extend(i2);

            // Checks that both args are numbers
            i1.extend(arith_type_check(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)));

            // proceeds if there is no issue
            i1.push(Instr::Cmp(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX)));
            i1.push(Instr::Jge(Val::Label(Label::YES, lbl)));
            i1.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            i1.push(Instr::Jl(Val::Label(Label::NO, lbl)));
            i1.push(Instr::Label(Val::Label(Label::YES, lbl)));
            i1.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
            i1.push(Instr::Label(Val::Label(Label::NO, lbl)));

            i1
        }
        Expr::BinOp(Op2::LessEqual, e1, e2) => {
            let mut i1 = compile_expr(e1, si, env, lbl+1, target);
            let i2 = compile_expr(e2, si+1, env, lbl+2, target);
            i1.push(Instr::IMov(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX)));
            i1.extend(i2);

            // Checks that both args are numbers
            i1.extend(arith_type_check(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)));

            // proceeds if there is no issue
            i1.push(Instr::Cmp(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX)));
            i1.push(Instr::Jle(Val::Label(Label::YES, lbl)));
            i1.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            i1.push(Instr::Jg(Val::Label(Label::NO, lbl)));
            i1.push(Instr::Label(Val::Label(Label::YES, lbl)));
            i1.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
            i1.push(Instr::Label(Val::Label(Label::NO, lbl)));

            i1
        }
        Expr::If(cond, thn, els) => {
            let mut i1 = compile_expr(cond, si, env, lbl+1, target);
            let i2 = compile_expr(thn, si+1, env, lbl+2, target);
            let i3 = compile_expr(els, si+2, env, lbl+3, target);
            
            i1.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::Imm(1)));
            i1.push(Instr::Je(Val::Label(Label::IFELSE, lbl)));
            i1.extend(i2);
            i1.push(Instr::Jmp(Val::Label(Label::IFEND, lbl)));
            i1.push(Instr::Label(Val::Label(Label::IFELSE, lbl)));
            i1.extend(i3);
            i1.push(Instr::Label(Val::Label(Label::IFEND, lbl)));
            i1
        }
        Expr::Let(v, body) => {
            let mut instr_list: Vec<Instr> = Vec::new();
            let mut nenv: HashMap<String, i64> = env.clone();
            let mut scope: HashSet<String> = HashSet::new();

            if v.is_empty() {
                panic!("Invalid");
            }

            for (s, e) in v {
                // Checks if variable s is already bound in this scope
                // This should only trigger if var is being rebound in the same scope
                // In particular, we should allow var shadowing (rebind var in smaller scopes)
                if scope.contains(s) {
                    panic!("Duplicate binding");
                } else {
                    instr_list.extend(compile_expr(e, si, &nenv, lbl, target));
                    instr_list.push(Instr::IMov(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX)));
                    nenv = nenv.update(s.to_string(), si);
                    scope.insert(s.to_string());
                    si += 1;
                }
            }
            instr_list.extend(compile_expr(body, si, &nenv, lbl, target));
            instr_list
        }
        Expr::Block(v) => {
            let mut instrs: Vec<Instr> = Vec::new();

            if v.is_empty() {
                panic!("Invalid");
            }

            for e in v {
                instrs.extend(compile_expr(e, si, env, lbl+1, target));
            }
            instrs
        }
        Expr::Set(x, e) => {
            let mut instrs = compile_expr(e, si, env, lbl+1, target);
            match env.get(x) {
                Some(val) => {
                    instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, *val), Val::Reg(Reg::RAX)));
                    instrs
                }
                None => panic!("Unbound variable identifier {}", x)
            }
        }
        Expr::Loop(e) => {
            let break_target = Val::Label(Label::ENDLOOP, lbl);
            let mut instrs = vec![Instr::Label(Val::Label(Label::LOOP, lbl))];
            instrs.extend(compile_expr(e, si, env, lbl+1, &break_target));
            instrs.push(Instr::Jmp(Val::Label(Label::LOOP, lbl)));
            instrs.push(Instr::Label(break_target));
            instrs
        }
        Expr::Break(e) => {
            let mut instrs = compile_expr(e, si, env, lbl+1, target);
            // need to jump to the ENDLOOP label, but we don't know what number is appended to it??
            // -> need to propogate a break target through recursive calls
            // if there is no break target, then we panic!("break")
            match target {
                Val::Label(Label::ENDLOOP, lbl) => {
                    instrs.push(Instr::Jmp(Val::Label(Label::ENDLOOP, *lbl)));
                    instrs
                }
                _ => panic!("no break target!"),
            }
        }
        Expr::Func(s, v) => {
            // TODO
            if v.len() == 0 {
                vec![Instr::Call(Val::Func(s.to_string()))]
            } else {
                let mut instrs:Vec<Instr> = Vec::new();
                for (i, e) in v.iter().enumerate() {
                    instrs.extend(compile_expr(e, si+(i as i64), env, lbl+1, target));
                    instrs.push(Instr::IMov(Val::NegRegOffset(Reg::RSP,depth(e) + 1 + (i as i64)), 
                                            Val::Reg(Reg::RAX)));
                }
                instrs.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm((v.len() as i64)*8)));
                instrs.push(Instr::Call(Val::Func(s.to_string())));
                instrs.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm((v.len() as i64)*8)));
                instrs
            }
        }
    }
}

// helper function that computes the stack depth needed for function definitions
fn depth(e: &Expr) -> i64 {
    match e {
        Expr::Number(_) => 0,
        Expr::Boolean(_) => 0,
        Expr::Id(_) => 0,
        Expr::Let(bindings, body) => {
            let mut maximum = depth(&bindings[0].1);
            for (i, (_, expr)) in bindings.iter().enumerate() {
                maximum = maximum.max(depth(expr) + (i as i64));
            }
            maximum.max(depth(body) + (bindings.len() as i64))
        }
        Expr::UnOp(_, expr) => depth(expr),
        Expr::BinOp(_, expr1, expr2) => depth(expr1).max(depth(expr2) + 1),
        Expr::If(expr1, expr2, expr3) => depth(expr1).max(depth(expr2)).max(depth(expr3)),
        Expr::Loop(expr) => depth(expr),
        Expr::Break(expr) => depth(expr),
        Expr::Set(_, expr) => depth(expr),
        Expr::Block(expressions) => expressions.iter().map(|expr| depth(expr)).max().unwrap_or(0),
        Expr::Func(_, expressions) => {
            let mut maximum = depth(&expressions[0]);
            for (i, expr) in expressions.iter().enumerate() {
                maximum = maximum.max(depth(expr) + (i as i64));
            }
            maximum
        }
    }
}

// Compiles a function definition
fn compile_defn(f: &Defn) -> Vec<Instr> {
    // TODO
    match f {
        Defn::NoArg(name, e) => {
            let depth = depth(e);
            // empty environment bc no arguments to put inside it
            let env: HashMap<String, i64> = HashMap::new(); 
            let mut instrs = vec![Instr::Label(Val::Func(name.to_string()))];

            instrs.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(depth*8)));
            instrs.extend(compile_expr(e, 0, &env, 1, &Val::Error));
            instrs.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(depth*8)));
            instrs.push(Instr::Ret);
            instrs
        }
        Defn::Args(name, v, e) => {
            let depth = depth(e);
            let mut env: HashMap<String, i64> = HashMap::new();
            for (i, arg) in v.iter().enumerate() {
                env.insert(arg.to_string(), depth+(i as i64)+1);
            }
            let mut instrs = vec![Instr::Label(Val::Func(name.to_string()))];

            instrs.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(depth*8)));
            instrs.extend(compile_expr(e, 0, &env, 1, &Val::Error));
            instrs.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(depth*8)));
            instrs.push(Instr::Ret);
            instrs
        }
    }
}

// Compiles the parsed .snek program into instructions and then converts the instructions to a
// string
fn compile(p: &Prog) -> String {
    // Parameters for expr compilation:
    let vars: HashMap<String, i64> = HashMap::new();
    let si = 2;
    let lbl = 1;
    let target = Val::Error; // any val that is not a Val::Label will result in error, if try to
                             // break to this target

    let mut instrs: Vec<Instr> = Vec::new();
    match p {
        Prog::NoFun(e) => { instrs = compile_expr(e, si, &vars, lbl, &target); }
        Prog::Funs(funcs, e) => {
            // Compiles the definitions and expression to instructions
            for f in funcs {
                instrs.extend(compile_defn(&f));
            }
            instrs.push(Instr::Label(Val::Main));
            instrs.extend(compile_expr(e, si, &vars, lbl, &target));
        }
    }
    // Converts the vec of instructions to a string representation
    let mut strs: Vec<String> = Vec::new();
    for i in instrs {
        strs.push(instr_to_str(&i));
    }
    strs.join("\n")
}

// helper function to allow s-expr library to propertly lex the input
fn is_one_sexpr(s: &String) -> bool {
    let mut counter = 0;
    for (i, c) in s.chars().enumerate()  {
        // outputs false if first '(' gets matched before the whole string is processed
        if i > 0 && counter == 0 { return false; }

        if c == '(' { counter += 1; }
        else if c == ')' { counter -= 1; }
    }
    return true;
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;


    // Makes sure that the .snek file is 1 s-expression, by wrapping parens if needed
    let has_funcs = !is_one_sexpr(&in_contents);
    if has_funcs {
        in_contents = format!("({in_contents})");
    }
    
    // panics with "Invalid" if lexing fails
    let prog = parse_prog(&parse(&in_contents).expect("Invalid"), has_funcs);

    // You will make result hold the result of actually compiling
    let result = compile(&prog);

    let asm_program = format!(
        "
section .text
extern snek_error
extern snek_print
global our_code_starts_here
throw_error:
  mov rdi, rsi
  push rsp
  call snek_error
  {}
  ret
",
        result
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
