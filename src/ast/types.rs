use crate::common::position::{Location, Span};

#[derive(Debug, Clone)]
pub enum Nodetype {
    Variable,
    Function,
    Import,
    Class,
    Interface,
    For,
    While,
    Condition,
}

#[derive(Debug, Clone)]
pub enum ASTValue {
    Identifier(Identifier),
    CallFunctionExpr(Box<CallFunctionExpr>),
    ArithmeticOp(Box<ArithmeticOp>),
    LogicalOp(Box<LogicalOp>),
    BinaryOp(Box<BinaryOp>),
    UnaryOp(Box<UnaryOp>),
    AccessDotExpr(Box<AccessDotExpr>),
    AccessIndexExpr(Box<AccessIndexExpr>),
    TypeCastExpr(Box<TypeCastExpr>),
    IntParsed(IntParsed),
    FloatParsed(FloatParsed),
    BoolParsed(BoolParsed),
    ArrayParsed(ArrayParsed),
    NullParsed(NullParsed),
    DictParsed(DictParsed),
    StringParsed(StringParsed),
    AssignVariableStmt(Box<AssignVariableStmt>),
    FunctionStmt(Box<FunctionStmt>),
    ConditionStmt(Box<ConditionStmt>),
    InitVariableStmt(Box<InitVariableStmt>),
}

impl ASTValue {
    pub fn get_span(val: &ASTValue) -> Span {
        match val {
            ASTValue::Identifier(e) => e.span,
            ASTValue::CallFunctionExpr(e) => e.span,
            ASTValue::ArithmeticOp(e) => e.span,
            ASTValue::LogicalOp(e) => e.span,
            ASTValue::BinaryOp(e) => e.span,
            ASTValue::UnaryOp(e) => e.span,
            ASTValue::AccessDotExpr(e) => e.span,
            ASTValue::AccessIndexExpr(e) => e.span,
            ASTValue::TypeCastExpr(e) => e.span,
            ASTValue::IntParsed(e) => e.span,
            ASTValue::FloatParsed(e) => e.span,
            ASTValue::BoolParsed(e) => e.span,
            ASTValue::ArrayParsed(e) => e.span,
            ASTValue::NullParsed(e) => e.span,
            ASTValue::DictParsed(e) => e.span,
            ASTValue::AssignVariableStmt(e) => e.span,
            ASTValue::StringParsed(e) => e.span,
            ASTValue::FunctionStmt(e) => e.span,
            ASTValue::ConditionStmt(e) => e.span,
            ASTValue::InitVariableStmt(e) => e.span,
        }
    }

    pub fn get_type(&self) -> String {
        match self {
            ASTValue::Identifier(e) => e.ntype.clone(),
            ASTValue::CallFunctionExpr(e) => e.ntype.clone(),
            ASTValue::ArithmeticOp(e) => e.ntype.clone(),
            ASTValue::LogicalOp(e) => e.ntype.clone(),
            ASTValue::BinaryOp(e) => e.ntype.clone(),
            ASTValue::UnaryOp(e) => e.ntype.clone(),
            ASTValue::AccessDotExpr(e) => e.ntype.clone(),
            ASTValue::AccessIndexExpr(e) => e.ntype.clone(),
            ASTValue::TypeCastExpr(e) => e.ntype.clone(),
            ASTValue::IntParsed(e) => e.ntype.clone(),
            ASTValue::FloatParsed(e) => e.ntype.clone(),
            ASTValue::BoolParsed(e) => e.ntype.clone(),
            ASTValue::ArrayParsed(e) => e.ntype.clone(),
            ASTValue::NullParsed(e) => e.ntype.clone(),
            ASTValue::DictParsed(e) => e.ntype.clone(),
            ASTValue::ConditionStmt(e) => e.ntype.clone(),
            ASTValue::InitVariableStmt(e) => e.ntype.clone(),
            ASTValue::FunctionStmt(e) => e.ntype.clone(),
            ASTValue::StringParsed(e) => e.ntype.clone(),
            ASTValue::AssignVariableStmt(e) => e.ntype.clone(),
        }
    }

    pub fn get_span2(&self) -> Span {
        Self::get_span(self)
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub span: Span,
    pub ntype: String,
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct CallFunctionExpr {
    pub span: Span,
    pub ntype: String,
    pub what: ASTValue,
    pub params: Vec<ASTValue>,
}

#[derive(Debug, Clone)]
pub struct ArithmeticOp {
    pub span: Span,
    pub ntype: String,
    pub left: ASTValue,
    pub op: String,
    pub right: ASTValue,
}

#[derive(Debug, Clone)]
pub struct UnaryOp {
    pub span: Span,
    pub ntype: String,
    pub value: ASTValue,
    pub op: String,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct LogicalOp {
    pub span: Span,
    pub ntype: String,
    pub left: ASTValue,
    pub op: String,
    pub right: ASTValue,
}

#[derive(Debug, Clone)]
pub struct BinaryOp {
    pub span: Span,
    pub ntype: String,
    pub left: ASTValue,
    pub op: String,
    pub right: ASTValue,
}

#[derive(Debug, Clone)]
pub enum DotAccessable {
    Identifier(Identifier),
    CallFunctionExpr(CallFunctionExpr),
    AccessDotExpr(Box<AccessDotExpr>),
    AccessIndexExpr(Box<AccessIndexExpr>),
}

impl DotAccessable {
    pub fn get_span(val: &DotAccessable) -> Span {
        match val {
            DotAccessable::AccessDotExpr(e) => e.span,
            DotAccessable::AccessIndexExpr(e) => e.span,
            DotAccessable::CallFunctionExpr(e) => e.span,
            DotAccessable::Identifier(e) => e.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct AccessDotExpr {
    pub span: Span,
    pub ntype: String,
    pub left: DotAccessable,
    pub right: DotAccessable,
    pub return_null: bool,
}

#[derive(Debug, Clone)]
pub struct AccessIndexExpr {
    pub span: Span,
    pub ntype: String,
    pub left: DotAccessable,
    pub right: ASTValue,
}

#[derive(Debug, Clone)]
pub enum TypeValue {
    Static(String),
    Dynamic(ASTValue),
    Expr(TypeExpr),
}

#[derive(Debug, Clone)]
pub struct TypeExpr {
    pub span: Span,
    pub ntype: String,
    pub value: Vec<TypeValue>,
    pub arr_len: Option<usize>,
}

#[derive(Debug, Clone)]
pub struct TypeCastExpr {
    pub span: Span,
    pub ntype: String,
    pub value: ASTValue,
    pub target_type: TypeExpr,
    pub return_null: bool,
}

#[derive(Debug, Clone)]
pub struct StringParsed {
    pub span: Span,
    pub ntype: String,
    pub value: String,
}

#[derive(Debug, Clone)]
pub enum IntValue {
    U8(u8),
    I8(i8),
    U16(u16),
    I16(i16),
    U32(u32),
    I32(i32),
    I64(i64),
    U64(u64),
}

#[derive(Debug, Clone)]
pub enum NumType {
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    I64,
    U64,
}

#[derive(Debug, Clone)]
pub struct IntParsed {
    pub span: Span,
    pub ntype: String,
    pub value: String,
}

#[derive(Debug, Clone)]
pub enum FloatValue {
    F32(f32),
    F64(f64),
}

#[derive(Debug, Clone)]
pub enum FloatType {
    F32,
    F64,
}

#[derive(Debug, Clone)]
pub struct FloatParsed {
    pub span: Span,
    pub ntype: String,
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct BoolParsed {
    pub span: Span,
    pub ntype: String,
    pub value: bool,
}

#[derive(Debug, Clone)]
pub struct NullParsed {
    pub span: Span,
    pub ntype: String,
}

#[derive(Debug, Clone)]
pub struct ArrayParsed {
    pub span: Span,
    pub ntype: String,
    pub elements: Vec<ASTValue>,
}

#[derive(Debug, Clone)]
pub struct DictElement {
    pub span: Span,
    pub key: Identifier,
    pub value: ASTValue,
}

#[derive(Debug, Clone)]
pub struct DictParsed {
    pub span: Span,
    pub ntype: String,
    pub entries: Vec<DictElement>,
}

#[derive(Debug, Clone)]
pub enum VarType {
    Infer(()),
    Static(TypeExpr),
}

#[derive(Debug, Clone)]
pub struct InitVariableStmt {
    pub span: Span,
    pub ntype: String,
    pub constant: bool,
    pub name: Identifier,
    pub vtype: VarType,
    pub value: Option<ASTValue>,
}

#[derive(Debug, Clone)]
pub enum Assignable {
    AccessDotExpr(AccessDotExpr),
    Identifier(Identifier),
    AccessIndexExpr(AccessIndexExpr),
}

impl Assignable {
    pub fn get_span(&self) -> Span {
        match self {
            Self::AccessDotExpr(e) => e.span,
            Self::AccessIndexExpr(e) => e.span,
            Self::Identifier(e) => e.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct AssignVariableStmt {
    pub span: Span,
    pub ntype: String,
    pub target: Assignable,
    pub op: String,
    pub value: ASTValue,
}

#[derive(Debug, Clone)]
pub struct FunctionParam {
    pub span: Span,
    pub name: Identifier,
    pub ptype: VarType,
    pub default: Option<ASTValue>,
}

#[derive(Debug, Clone)]
pub struct BlockStmt {
    pub span: Span,
    pub ntype: String,
    pub body: Vec<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct FunctionStmt {
    pub span: Span,
    pub ntype: String,
    pub name: Option<Identifier>,
    pub params: Vec<FunctionParam>,
    pub rtype: VarType,
    pub block: BlockStmt,
}

#[derive(Debug, Clone)]
pub struct InterfaceMember {
    pub span: Span,
    pub name: Identifier,
    pub etype: TypeExpr,
}

#[derive(Debug, Clone)]
pub struct InterfaceStmt {
    pub span: Span,
    pub ntype: String,
    pub name: Identifier,
    pub members: Vec<InterfaceMember>,
}

#[derive(Debug, Clone)]
pub struct ImportStmt {
    pub span: Span,
    pub ntype: String,
    pub spec: Vec<Identifier>,
}

#[derive(Debug, Clone)]
pub enum Exportable {
    Function(FunctionStmt),
    Class(ClassStmt),
    Interface(InterfaceStmt),
    Variable(InitVariableStmt),
}

#[derive(Debug, Clone)]
pub struct ExportStmt {
    pub ntype: String,
    pub item: Exportable,
}

#[derive(Debug, Clone)]
pub enum Extendable {
    Ident(Identifier),
    DotExpr(AccessDotExpr),
}

#[derive(Debug, Clone)]
pub struct ClassStmt {
    pub span: Span,
    pub ntype: String,
    pub name: Identifier,
    pub extends: Option<Extendable>,
    pub implements: Vec<Extendable>,
    pub properties: Vec<InitVariableStmt>,
    pub methods: Vec<FunctionStmt>,
    pub initializer: Option<FunctionStmt>,
}

#[derive(Debug, Clone)]
pub struct ForStmt {
    pub span: Span,
    pub ntype: String,
    pub iter_val: Option<Identifier>,
    pub idx_val: Option<Identifier>,
    pub iterable: ASTValue,
    pub block: BlockStmt,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub span: Span,
    pub ntype: String,
    pub condition: ASTValue,
    pub block: BlockStmt,
}

#[derive(Debug, Clone)]
pub struct ConditionStmt {
    pub span: Span,
    pub ntype: String,
    pub condition: Option<ASTValue>, // not optional for if, but for else
    pub else_stmt: Option<Box<ConditionStmt>>,
    pub block: BlockStmt,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub span: Span,
    pub ntype: String,
    pub value: Option<ASTValue>,
}

#[derive(Debug, Clone)]
pub enum ASTNode {
    Value(ASTValue),
    InitVariableStmt(InitVariableStmt),
    FunctionStmt(FunctionStmt),
    ClassStmt(ClassStmt),
    ForStmt(ForStmt),
    WhileStmt(WhileStmt),
    ConditionStmt(ConditionStmt),
    ReturnStmt(ReturnStmt),
    InterfaceStmt(InterfaceStmt),
}

impl ASTNode {
    pub fn get_span(&self) -> Span {
        match self {
            ASTNode::Value(e) => e.get_span2(),
            ASTNode::InitVariableStmt(e) => e.span,
            ASTNode::FunctionStmt(e) => e.span,
            ASTNode::ClassStmt(e) => e.span,
            ASTNode::ForStmt(e) => e.span,
            ASTNode::WhileStmt(e) => e.span,
            ASTNode::ConditionStmt(e) => e.span,
            ASTNode::ReturnStmt(e) => e.span,
            ASTNode::InterfaceStmt(e) => e.span,
        }
    }
}

#[derive(Debug, Clone)]
pub enum GlobalNode {
    Node(ASTNode),
    ImportStmt(ImportStmt),
    ExportStmt(ExportStmt),
}

#[derive(Debug, Clone)]
pub struct GlobalBlockStmt {
    pub span: Span,
    pub ntype: String,
    pub body: Vec<GlobalNode>,
}
