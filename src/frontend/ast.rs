#[derive(Debug)]
pub struct CompUnit {
    pub global_items: Vec<GlobalItem>,
}

#[derive(Debug)]
pub enum GlobalItem {
    Decl(Decl),
    FuncDef(FuncDef),
}

#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub func_fparams: Vec<String>,
    pub block: Block,
}

#[derive(Debug)]
pub enum FuncType {
    INT,
    VOID,
}


#[derive(Debug)]
pub struct Block {
    pub block_items: Vec<BlockItem>,
}

#[derive(Debug)]
pub struct If {
    pub exp: Exp,
    pub then: Stmt,
    pub else_then: Option<Stmt>,
}

#[derive(Debug)]
pub struct While {
    pub exp: Exp,
    pub stmt: Stmt,
}

#[derive(Debug)]
pub enum ReturnStmt {
    Int(Exp),
    VOID,
}

#[derive(Debug)]
pub enum Stmt {
    ReturnStmt(ReturnStmt), 
    AssignStmt(String, Exp),
    ExpStmt(Option<Exp>),
    Block(Block),
    IfStmt(Box<If>),
    WhileStmt(Box<While>),
    BREAK,
    CONTINUE,
}

#[derive(Debug)]
pub enum Exp {
    InnerLOrExp(LOrExp),
}

#[derive(Debug)]
pub enum UnaryExp {
    InnerPrimaryExp(Box<PrimaryExp>),
    InnerUnaryExp(UnaryOp, Box<UnaryExp>),
    InnerCall(String, Vec<Exp>),
}

#[derive(Debug)]
pub enum PrimaryExp {
    InnerExp(Box<Exp>),
    InnerLVal(String),
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

#[derive(Debug)]
pub enum Decl {
    InnerConstDecl(ConstDecl),
    InnerVarDecl(VarDecl),
}

#[derive(Debug)]
pub struct ConstDecl {
    pub const_defs: Vec<ConstDef>,
}

#[derive(Debug)]
pub struct ConstDef {
    pub ident: String,
    pub const_initval: ConstInitVal,
}

#[derive(Debug)]
pub struct ConstInitVal {
    pub const_exp: ConstExp,
}

#[derive(Debug)]
pub enum BlockItem {
    InnerDecl(Decl),
    InnerStmt(Stmt),
}

#[derive(Debug)]
pub struct ConstExp {
    pub exp: Exp,
}

#[derive(Debug)]
pub struct VarDecl {
    pub var_defs: Vec<VarDef>,
}

#[derive(Debug)]
pub enum VarDef {
    InnerNoInit(String),
    InnerInit(String, InitVal),
}

#[derive(Debug)]
pub struct InitVal {
    pub exp: Exp,
}
