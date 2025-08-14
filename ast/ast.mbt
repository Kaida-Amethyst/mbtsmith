typealias @Either.Either


struct Env[K, V] {
  map: Map[K, V]
  parent: Env[K, V]?
}

fn[K, V] Env::new() -> Env[K, V] {
  Env::{ map: Map::new(), parent: None }
}

fn[K, V] Env::sub_env(self: Self[K, V]) -> Env[K, V] {
  Env::{ map: Map::new(), parent: Some(self) }
}

fn[K:Eq+Hash, V] Env::get(self: Self[K, V], key: K) -> V? {
  match self.map.get(key) {
    Some(value) => Some(value);
    None => match self.parent {
      Some(parent) => parent.get(key);
      None => None;
    }
  }
}

pub struct RandAstGenerator {
  rand: @random.Rand
  upper_max_len: Int
  ident_max_len: Int
  ty_env: Env[Upper, Type]
  id_env: Env[Ident, Type]
}

pub fn RandAstGenerator::new(upper_max_len~: Int = 10, ident_max_len~:Int=10) -> RandAstGenerator {
  RandAstGenerator::{
    rand: @random.Rand::new(),
    upper_max_len,
    ident_max_len,
    ty_env: Env::new(),
    id_env: Env::new(),
  }
}

///|
pub fn RandAstGenerator::int(self: Self, limit: Int) -> Int {
  self.rand.int(limit~)
}

///|
///
/// Always return positive integers.
pub fn RandAstGenerator::pos_int(self: Self, limit: Int) -> Int {
  let limit = limit.abs()
  let limit = limit.reinterpret_as_uint()
  self.rand.uint(limit~).reinterpret_as_int()
}

pub fn[T] RandAstGenerator::choice(self: Self, choices: Array[T]) -> T {
  let length = choices.length();
  if length == 0 {
    println("Fetal Error: Cannot choose from an empty array");
    panic()
  }
  let index = self.pos_int(length);
  choices[index]
}

const UPPER_CHARS: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
const LOWER_CHARS: String = "abcdefghijklmnopqrstuvwxyz";
const DIGITS: String = "0123456789";
const IDENT_CHARS: String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_";
const NON_UPPER_CHARS: String = "abcdefghijklmnopqrstuvwxyz0123456789_";

///|
pub fn RandAstGenerator::gen_upper(self: Self) -> Upper {
  fn helper() {
    let str_builder = StringBuilder::new();
    let first_char = self.choice(UPPER_CHARS.to_array());
    str_builder.write_char(first_char);
    for i in 0..<self.pos_int(self.upper_max_len) {
      let c = self.choice(IDENT_CHARS.to_array());
      str_builder.write_char(c);
    }
    Upper(str_builder.to_string())
  }
  let mut upper = helper()
  if self.ty_env.get(upper) is Some(_) {
    upper = helper()
  }
  upper
}

///|
pub fn RandAstGenerator::gen_ident(self: Self) -> Ident {
  fn helper() -> Ident {
    let str_builder = StringBuilder::new();
    let first_char = self.choice(NON_UPPER_CHARS.to_array());
    str_builder.write_char(first_char);
    for i in 0..<self.pos_int(self.ident_max_len) {
      let c = self.choice(IDENT_CHARS.to_array());
      str_builder.write_char(c);
    }
    Ident(str_builder.to_string())
  }
  let mut ident = helper();
  while self.id_env.get(ident) is Some(_) {
    ident = helper()
  }
  ident
}

pub fn RandAstGenerator::gen_double(self: Self) -> Double {
  self.rand.double()
}

pub fn RandAstGenerator::gen_bool(self: Self) -> Bool {
  self.rand.int() % 2 == 0
}


///|
pub(all) type Upper String derive(Hash, Eq)

///|
pub impl Show for Upper with output(self, logger) {
  logger.write_string(self.inner());
}

///|
pub(all) type Ident String derive(Hash, Eq)

///|
pub impl Show for Ident with output(self, logger) {
  logger.write_string(self.inner());
}

///| Show for Type
///
/// ```moonbit
/// inspect(Type::Unit, content="Unit")
/// inspect(Type::Bool, content="Bool")
/// inspect(Type::Int, content="Int")
/// inspect(Type::Double, content="Double")
/// inspect(Type::Array(Int), content="Array[Int]")
/// inspect(Type::Tuple([Int, Bool]), content="(Int, Bool)")
/// inspect(Type::Func([Int, Bool], Int), content="(Int, Bool) -> Int")
/// inspect(Type::GenericDef("T"), content="T")
/// inspect(Type::Struct("Point", []), content="Point")
/// inspect(Type::GenericSub("Complex", Int), content="Complex[Int]")
/// ```
pub(all) enum Type {
  Unit
  Bool
  Int
  Double
  Array(Type)
  Tuple(Array[Type])
  Func(Array[Type], Type) // (Int, Bool) -> Int

  // User-defined type, e.g. Point
  Struct(Upper, Array[(Ident, Type)])
  GenericDef(Upper)  // T
  GenericSub(Upper, Type) // Complex[Int]
}

pub impl Show for Type with output(self, logger) {
  let s = match self {
    Unit => "Unit";
    Bool => "Bool";
    Int => "Int";
    Double => "Double";
    Array(inner) => "Array[\{inner}]";
    Tuple(inner) => {
      let inner_str = inner.map(t => t.to_string()).join(", ");
      "(\{inner_str})"
    }
    Func(args, ret) => {
      let args_str = args.map(t => t.to_string()).join(", ");
      "(\{args_str}) -> \{ret}"
    }
    Struct(name, _) => "\{name}";
    GenericDef(name) => "\{name}";
    GenericSub(name, inner) => "\{name}[\{inner}]";
  }
  logger.write_string(s);
}

///| ValueExpr is primary expressions that can be evaluated to a value.
///
/// ```mbt
/// inspect(ValueExpr::UnitExpr, content="()")
/// inspect(ValueExpr::BoolExpr(true), content="true")
/// inspect(ValueExpr::BoolExpr(false), content="false")
/// inspect(ValueExpr::IntExpr(1), content="1")
/// inspect(ValueExpr::IntExpr(65536), content="65536")
/// inspect(ValueExpr::FloatExpr(1.0), content="1.0")
/// inspect(ValueExpr::FloatExpr(2.0), content="2.0")
/// inspect(ValueExpr::IdentExpr(Ident("x")), content="x")
/// ```
pub(all) enum ValueExpr {
  ArrayMake(Expr, Expr)
  StructConstruct(Upper, Array[(Ident, Expr)]) // Point::{ x: 1, y: 2 }
  EnumConstruct(Upper, Array[Expr]) // Point(1, 2)
  UnitExpr // ()
  GroupExpr(Expr) // (x)
  TupleExpr(Array[Expr]) // (x, y)
  ArrayExpr(Array[Expr]) // [x, y, z]
  BoolExpr(Bool) // true | false
  IdentExpr(Ident) // x
  BlockExpr(Array[Stmt], Expr?) // { blah; blah;  blah}
  NegExpr(Expr) // -x
  FloatExpr(Double) // 1.0 | 1.
  IntExpr(Int) // 1
  NotExpr(Expr) // !x
}

pub impl Show for ValueExpr with output(self, logger) {
  let s = match self {
    ArrayMake(size, init) => "Array::make(\{size}, \{init})";
    StructConstruct(name, fields) => {
      let fields_str = fields.map(f => "\{f.0}: \{f.1}").join(", ");
      "\{name}::{\{fields_str}}"
    }
    EnumConstruct(name, fields) => {
      let fields_str = fields.map(f => f.to_string()).join(", ");
      "\{name}(\{fields_str})"
    }
    UnitExpr => "()";
    GroupExpr(expr) => "(\{expr})";
    TupleExpr(exprs) => {
      let exprs_str = exprs.map(e => e.to_string()).join(", ");
      "(\{exprs_str})"
    }
    ArrayExpr(exprs) => {
      let exprs_str = exprs.map(e => e.to_string()).join(", ");
      "[\{exprs_str}]"
    }
    BoolExpr(value) => if value { "true" } else { "false" };
    IdentExpr(ident) => ident.to_string();
    BlockExpr(stmts, expr_opt) => {
      let stmts_str = stmts.map(s => s.to_string()).join("; ");
      match expr_opt {
        Some(expr) => "{ \{stmts_str}; \{expr} }";
        None => "{ \{stmts_str} }";
      }
    }
    NegExpr(expr) => "-\{expr}";
    FloatExpr(value) => { 
      let s = value.to_string();
      if s.contains_char('.') {
        s; // e.g. "1.0"
      } else {
        s + ".0"; // e.g. "1" -> "1.0"
      }
    }
    IntExpr(value) => value.to_string();
    NotExpr(expr) => "!\{expr}";
  }
  logger.write_string(s);
}

///| ApplyExpr is used to represent function application, array access, and dot access.
///
/// ```mbt
/// let complex = ValueExpr::IdentExpr(Ident("complex"));
/// let complex = ApplyExpr::ValueExpr(complex);
/// inspect(complex, content="complex")
///
/// let arr = ValueExpr::IdentExpr(Ident("arr"));
/// let arr = ApplyExpr::ValueExpr(arr);
/// inspect(arr, content="arr")
///
/// let add = ValueExpr::IdentExpr(Ident("add"));
/// let add = ApplyExpr::ValueExpr(add);
/// inspect(add, content="add")
///
/// let i1 = ValueExpr::IntExpr(1);
/// let i1 = ApplyExpr::ValueExpr(i1);
/// inspect(i1, content="1");
///
/// let i1 = IfLevelExpr::ApplyExpr(i1);
/// let i1 = Expr::IfLevelExpr(i1);
///
/// let i42 = ValueExpr::IntExpr(42);
/// let i42 = ApplyExpr::ValueExpr(i42);
/// inspect(i42, content="42");
///
/// let i42 = IfLevelExpr::ApplyExpr(i42);
/// let i42 = Expr::IfLevelExpr(i42);
///
/// inspect(ApplyExpr::ArrAcc(arr, i1), content="arr[1]")
/// inspect(ApplyExpr::DotAcc(complex, Ident("y")), content="complex.y")
/// inspect(ApplyExpr::Call(add, [i1, i42]), content="add(1, 42)")
/// ```
pub(all) enum ApplyExpr {
  ValueExpr(ValueExpr)
  ArrAcc(ApplyExpr, Expr)
  DotAcc(ApplyExpr, Ident) // x.y
  Call(ApplyExpr, Array[Expr]) // x(y1, y2, ...)
}

pub impl Show for ApplyExpr with output(self, logger) {
  match self {
    ValueExpr(value_expr) => logger.write_string(value_expr.to_string());
    ArrAcc(left, index) => {
      logger.write_string("\{left}[\{index}]");
    }
    DotAcc(left, ident) => {
      logger.write_string("\{left}.\{ident}");
    }
    Call(func, args) => {
      let args_str = args.map(arg => arg.to_string()).join(", ");
      logger.write_string("\{func}(\{args_str})");
    }
  }
}

pub(all) enum CmpOp {
  Eq
  Ne
  Lt
  Le
  Gt
  Ge
}

pub impl Show for CmpOp with output(self, logger) {
  let s = match self {
    Eq => "==";
    Ne => "!=";
    Lt => "<";
    Le => "<=";
    Gt => ">";
    Ge => ">=";
  }
  logger.write_string(s);
}

///| Expr is the main expression type that can be used in the AST.
///
/// ```mbt
/// let x = ValueExpr::IdentExpr(Ident("x"));
/// let y = ValueExpr::IdentExpr(Ident("y"));
/// let i42 = ValueExpr::IntExpr(42);
/// let i73 = ValueExpr::IntExpr(73);
/// let f22 = ValueExpr::FloatExpr(22.0);
/// let f89 = ValueExpr::FloatExpr(89.0);
/// let bool_true = ValueExpr::BoolExpr(true);
/// let bool_false = ValueExpr::BoolExpr(false);
///
/// let x = ApplyExpr::ValueExpr(x);
/// let x = IfLevelExpr::ApplyExpr(x);
/// let x = Expr::IfLevelExpr(x);
///
/// let y = ApplyExpr::ValueExpr(y);
/// let y = IfLevelExpr::ApplyExpr(y);
/// let y = Expr::IfLevelExpr(y);
///
/// let i42 = ApplyExpr::ValueExpr(i42);
/// let i42 = IfLevelExpr::ApplyExpr(i42);
/// let i42 = Expr::IfLevelExpr(i42);
///
/// let i73 = ApplyExpr::ValueExpr(i73);
/// let i73 = IfLevelExpr::ApplyExpr(i73);
/// let i73 = Expr::IfLevelExpr(i73);
///
/// let f22 = ApplyExpr::ValueExpr(f22);
/// let f22 = IfLevelExpr::ApplyExpr(f22);
/// let f22 = Expr::IfLevelExpr(f22);
///
/// let f89 = ApplyExpr::ValueExpr(f89);
/// let f89 = IfLevelExpr::ApplyExpr(f89);
/// let f89 = Expr::IfLevelExpr(f89);
///
/// let bool_true = ApplyExpr::ValueExpr(bool_true);
/// let bool_true = IfLevelExpr::ApplyExpr(bool_true);
/// let bool_true = Expr::IfLevelExpr(bool_true);
///
/// let bool_false = ApplyExpr::ValueExpr(bool_false);
/// let bool_false = IfLevelExpr::ApplyExpr(bool_false);
/// let bool_false = Expr::IfLevelExpr(bool_false);
///
/// inspect(Expr::AndExpr(bool_false, y), content="false && y")
/// inspect(Expr::OrExpr(x, bool_true), content="x || true")
/// inspect(Expr::CmpExpr(Eq, x, y), content="x == y")
/// inspect(Expr::CmpExpr(Ne, x, y), content="x != y")
/// inspect(Expr::CmpExpr(Lt, i42, i73), content="42 < 73")
/// inspect(Expr::CmpExpr(Le, i42, i73), content="42 <= 73")
/// inspect(Expr::CmpExpr(Gt, i73, i42), content="73 > 42")
/// inspect(Expr::CmpExpr(Ge, i73, i42), content="73 >= 42")
/// inspect(Expr::AddExpr(i42, i73), content="42 + 73")
/// inspect(Expr::SubExpr(i73, i42), content="73 - 42")
/// inspect(Expr::MulExpr(i42, i73), content="42 * 73")
/// inspect(Expr::DivExpr(i73, i42), content="73 / 42")
/// inspect(Expr::ModExpr(i73, i42), content="73 % 42")
/// ```
pub(all) enum Expr {
  AndExpr(Expr, Expr) // x && y
  OrExpr(Expr, Expr) // x || y
  CmpExpr(CmpOp, Expr, Expr) // x == y | x != y | x < y | x <= y | x > y | x >= y
  AddExpr(Expr, Expr) // x + y
  SubExpr(Expr, Expr) // x - y
  MulExpr(Expr, Expr) // x * y
  DivExpr(Expr, Expr) // x / y
  ModExpr(Expr, Expr) // x % y
  IfLevelExpr(IfLevelExpr)
}

pub impl Show for Expr with output(self, logger) {
  let s = match self {
    AndExpr(left, right) => "\{left} && \{right}";
    OrExpr(left, right) => "\{left} || \{right}";
    CmpExpr(op, left, right) => "\{left} \{op} \{right}";
    AddExpr(left, right) => "\{left} + \{right}";
    SubExpr(left, right) => "\{left} - \{right}";
    MulExpr(left, right) => "\{left} * \{right}";
    DivExpr(left, right) => "\{left} / \{right}";
    ModExpr(left, right) => "\{left} % \{right}";
    IfLevelExpr(if_level_expr) => if_level_expr.to_string();
  }
  logger.write_string(s);
}

pub(all) enum IfLevelExpr {
  ApplyExpr(ApplyExpr)
  IfExpr(IfExpr)
  MatchExpr(MatchExpr)
}

pub impl Show for IfLevelExpr with output(self, logger) {
  match self {
    ApplyExpr(apply_expr) => logger.write_string(apply_expr.to_string());
    IfExpr(if_expr) => logger.write_string(if_expr.to_string());
    MatchExpr(match_expr) => logger.write_string(match_expr.to_string());
  }
}

pub(all) struct IfExpr {
  cond: Expr
  then_: BlockExpr
  else_: Either[IfExpr, BlockExpr]
}

pub impl Show for IfExpr with output(self, logger) {
  logger.write_string("if \{self.cond} ");
  logger.write_string(self.then_.to_string());
  match self.else_ {
    Left(if_expr) => {
      logger.write_string(" else ");
      logger.write_string(if_expr.to_string());
    }
    Right(block) => {
      logger.write_string("else ");
      logger.write_string(block.to_string());
    }
  }
}

pub struct BlockExpr {
  nested_level: Int // for print indentation
  stmts: Array[Stmt]
  last_expr: Expr?
}

pub impl Show for BlockExpr with output(self, logger) {
  // no ident before the first LBrace
  logger.write_string("{\n");
  let indent = "  ".repeat(self.nested_level + 1);
  for stmt in self.stmts {
    logger.write_string(indent);
    logger.write_string(stmt.to_string());
    logger.write_string(";\n");
  }
  if self.last_expr is Some(expr) {
    logger.write_string(indent);
    logger.write_string(expr.to_string());
    logger.write_string("\n");
  }
  let last_indent = "  ".repeat(self.nested_level);
  logger.write_string(last_indent);
  logger.write_string("}");
}

pub struct MatchExpr {
  nested_level: Int // for print indentation
  expr: Expr
  arms: Array[(Pattern, Expr)]
}

pub impl Show for MatchExpr with output(self, logger) {
  // no ident before the first LBrace
  logger.write_string("match \{self.expr} {\n");
  let indent = "  ".repeat(self.nested_level + 1);
  for arm in self.arms {
    let (pattern, expr) = arm;
    logger.write_string(indent);
    logger.write_string("\{pattern} => \{expr},\n");
  }
  let last_indent = "  ".repeat(self.nested_level);
  logger.write_string(last_indent);
  logger.write_string("}");
}

///| Pattern is used in the match arm.
///
/// ```mbt
/// inspect(Pattern::Number(1), content="1")
/// inspect(Pattern::Bool(true), content="true")
/// inspect(Pattern::WildCard, content="_")
/// inspect(Pattern::Ident("x"), content="x")
/// inspect(Pattern::Tuple([Ident("x"), Ident("y")]), content="(x, y)")
/// inspect(Pattern::EnumPattern(None, "Point", [Ident("x"), Ident("y")]), content="Point(x, y)")
/// inspect(Pattern::EnumPattern(None, "Red", []), content="Red")
/// inspect(Pattern::EnumPattern(Some("Color"), "Red", []), content="Color::Red")
/// ```
pub(all) enum Pattern {
  Number(Int) // 1, 2, ...
  Bool(Bool) // true, false
  WildCard // _
  Ident(Ident) // x, y, ...
  Tuple(Array[Pattern]) // (x, y, z)
  EnumPattern(Upper?, Upper, Array[Pattern])
}

pub impl Show for Pattern with output(self, logger) {
  match self {
    Number(value) => logger.write_string(value.to_string());
    Bool(value) => logger.write_string(if value { "true" } else { "false" });
    WildCard => logger.write_string("_");
    Ident(ident) => logger.write_string(ident.to_string());
    Tuple(patterns) => {
      let patterns_str = patterns.map(p => p.to_string()).join(", ");
      logger.write_string("(\{patterns_str})");
    }
    EnumPattern(prefix_opt, name, patterns) => {
      let prefix_str = match prefix_opt {
        Some(prefix) => "\{prefix}::";
        None => "";
      }
      let patterns_str = if patterns.is_empty() {
        "";
      } else {
        let s = patterns.map(p => p.to_string()).join(", ");
        "(\{s})"
      }
      logger.write_string("\{prefix_str}\{name}\{patterns_str}");
    }
  }
}

pub(all) enum Stmt {
  LetTupe(Array[Binding], Type?, Expr)
  LetMut(Ident, Type?, Expr)
  Let(Ident, Type?, Expr)
  LocalFuncDef(Array[(Ident, Type?)], Type?, BlockExpr)
  Assign(LeftValue, Expr)
  While(Expr, BlockExpr)
  Return(Expr)
  ExprStmt(Expr)
};

pub impl Show for Stmt with output(self, logger) {
  match self {
    LetTupe(bindings, ty_opt, expr) => {
      let bindings_str = bindings.map(b => b.to_string()).join(", ");
      let ty_str = match ty_opt {
        Some(ty) => ": \{ty}";
        None => "";
      }
      logger.write_string("let (\{bindings_str})\{ty_str} = \{expr}");
    }
    LetMut(ident, ty_opt, expr) => {
      let ty_str = match ty_opt {
        Some(ty) => ": \{ty}";
        None => "";
      }
      logger.write_string("let mut \{ident}\{ty_str} = \{expr}");
    }
    Let(ident, ty_opt, expr) => {
      let ty_str = match ty_opt {
        Some(ty) => ": \{ty}";
        None => "";
      }
      logger.write_string("let \{ident}\{ty_str} = \{expr}");
    }
    LocalFuncDef(params, ret_ty_opt, body) => {
      let params_str = params.map(p => "\{p.0}: \{p.1}").join(", ");
      let ret_ty_str = match ret_ty_opt {
        Some(ret_ty) => " -> \{ret_ty}";
        None => "";
      }
      logger.write_string("fn (\{params_str})\{ret_ty_str} ");
      logger.write_string(body.to_string());
    }
    Assign(left_value, expr) => {
      logger.write_string("\{left_value} = \{expr}");
    }
    While(cond, body) => {
      logger.write_string("while \{cond} ");
      logger.write_string(body.to_string());
    }
    Return(expr) => {
      logger.write_string("return \{expr}");
    }
    ExprStmt(expr) => {
      logger.write_string("\{expr}");
    }
  }
}

///| Binding is used in the left-hand side of a `let` statement.
///
/// ```mbt
/// inspect(Binding::Ident("x"), content="x")
/// inspect(Binding::WhileCard, content="_")
/// ```
pub(all) enum Binding {
  Ident(Ident)
  WhileCard
}

pub impl Show for Binding with output(self, logger) {
  let s = match self {
    Ident(ident) => ident.to_string();
    WhileCard => "_".to_string();
  }
  logger.write_string(s);
}


///| LeftValue is the left-hand side of an assignment.
///
/// ```mbt
/// inspect(LeftValue::Ident("x"), content="x")
/// inspect(LeftValue::DotAcc(Ident("x"), "y"), content="x.y")
///
/// let int_expr = ValueExpr::IntExpr(1);
/// let int_expr = ApplyExpr::ValueExpr(int_expr);
/// let int_expr = IfLevelExpr::ApplyExpr(int_expr);
/// let int_expr = Expr::IfLevelExpr(int_expr);
/// inspect(LeftValue::ArrAcc(Ident("arr"), int_expr), content="arr[1]")
/// ```
pub(all) enum LeftValue {
  Ident(Ident)
  DotAcc(LeftValue, Ident) // x.y
  ArrAcc(LeftValue, Expr) // x[y]
}

pub impl Show for LeftValue with output(self, logger) {
  let s = match self {
    Ident(ident) => ident.to_string();
    DotAcc(left, ident) => "\{left}.\{ident}";
    ArrAcc(left, index) => "\{left}[\{index}]";
  }
  logger.write_string(s);
}
