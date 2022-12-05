#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}

#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
}

#[derive(Debug)]
pub enum FuncType {
    Int,
}

#[derive(Debug)]
pub struct Block {
    pub stmt: Stmt,
}

#[derive(Debug)]
pub enum Stmt {
    ReturnStmt(Exp), 
}

#[derive(Debug)]
pub enum Exp {
    InnerLOrExp(LOrExp),
}

#[derive(Debug)]
pub enum UnaryExp {
    InnerPrimaryExp(Box<PrimaryExp>),
    InnerUnaryExp(UnaryOp, Box<UnaryExp>),
}

#[derive(Debug)]
pub enum PrimaryExp {
    InnerExp(Box<Exp>),
    Number(i32),
}

#[derive(Debug)]
pub enum UnaryOp {
    POSITIVE,
    NEGATIVE,
    NOT,
}

#[derive(Debug)]
pub enum AddExp {
    InnerMulExp(MulExp),
    InnerAddExp(Box<AddExp>, AddOp, MulExp),
}

#[derive(Debug)]
pub enum MulExp {
    InnerUnaryExp(UnaryExp),
    InnerMulExp(Box<MulExp>, MulOp, UnaryExp),
}

#[derive(Debug)]
pub enum AddOp {
    ADD,
    SUBTRACT,
}

#[derive(Debug)]
pub enum MulOp {
    MULTIPLY,
    DIVIDE,
    MOD,
}

#[derive(Debug)]
pub enum LOrExp {
    InnerLAndExp(LAndExp),
    InnerLOrExp(Box<LOrExp>, LAndExp),
}

#[derive(Debug)]
pub enum LAndExp {
    InnerEqExp(EqExp),
    InnerLAndExp(Box<LAndExp>, EqExp),
}

#[derive(Debug)]
pub enum EqExp {
    InnerRelExp(RelExp),
    InnerEqExp(Box<EqExp>, EqOp, RelExp),
}

#[derive(Debug)]
pub enum RelExp {
    InnerAddExp(AddExp),
    InnerRelExp(Box<RelExp>, RelOp, AddExp),
}

#[derive(Debug)]
pub enum EqOp {
    EQUAL,
    NOTEQUAL,
}

#[derive(Debug)]
pub enum RelOp {
    LESSTHAN,
    GREATERTHAN,
    LESSTHANOREQUAL,
    GREATERTHANOREQUAL,
}