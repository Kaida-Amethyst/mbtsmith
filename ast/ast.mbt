typealias @Either.Either

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
  Enum(Upper, Array[(Upper, Array[Type])]) // e.g. Color::Red(Int, Int, Int)
  GenericDef(Upper)  // T
  GenericSub(Upper, Type) // Complex[Int]
} derive(Eq)

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
    Enum(name, _) => "\{name}";
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
  EnumConstruct(Upper?, Upper, Array[Expr]) // Point(1, 2)
  UnitExpr // ()
  GroupExpr(Expr) // (x)
  TupleExpr(Array[Expr]) // (x, y)
  ArrayExpr(Array[Expr]) // [x, y, z]
  BoolExpr(Bool) // true | false
  IdentExpr(Ident) // x
  BlockExpr(BlockExpr) // { blah; blah;  blah}
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
    EnumConstruct(enum_name, tag, fields) => {
      let fields_str = fields.map(f => f.to_string()).join(", ");
      let prefix = match enum_name {
        Some(prefix) => "\{prefix}::";
        None => "";
      }
      "\{prefix}\{tag}(\{fields_str})"
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
    BlockExpr(b) => b.to_string();
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

fn Expr::from_value_expr(value_expr: ValueExpr) -> Expr {
  IfLevelExpr(ApplyExpr(ValueExpr(value_expr)))
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

///| IfLevelExpr represents expressions that have the same precedence as if expressions.
///
/// ```mbt
/// let x = ValueExpr::IdentExpr(Ident("x"));
/// let x = ApplyExpr::ValueExpr(x);
/// inspect(IfLevelExpr::ApplyExpr(x), content="x")
///
/// let cond = ValueExpr::BoolExpr(true);
/// let cond = ApplyExpr::ValueExpr(cond);
/// let cond = IfLevelExpr::ApplyExpr(cond);
/// let cond = Expr::IfLevelExpr(cond);
/// let then_block = BlockExpr::new()
/// let else_block = BlockExpr::new()
/// let if_expr = IfExpr::{ cond, then_: then_block, else_: Right(else_block) };
/// inspect(IfLevelExpr::IfExpr(if_expr), content="if true {\n} else {\n}")
///
/// let match_expr = MatchExpr::{ nested_level: 0, expr: cond, arms: [] };
/// inspect(IfLevelExpr::MatchExpr(match_expr), content="match true {\n}")
/// ```
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

///| IfExpr represents conditional expressions with if-then-else structure.
///
/// ```mbt
/// let cond = ValueExpr::BoolExpr(true);
/// let cond = ApplyExpr::ValueExpr(cond);
/// let cond = IfLevelExpr::ApplyExpr(cond);
/// let cond = Expr::IfLevelExpr(cond);
///
/// let x_val = ValueExpr::IntExpr(42);
/// let x_val = ApplyExpr::ValueExpr(x_val);
/// let x_val = IfLevelExpr::ApplyExpr(x_val);
/// let x_val = Expr::IfLevelExpr(x_val);
///
/// let y_val = ValueExpr::IntExpr(24);
/// let y_val = ApplyExpr::ValueExpr(y_val);
/// let y_val = IfLevelExpr::ApplyExpr(y_val);
/// let y_val = Expr::IfLevelExpr(y_val);
///
/// let then_block = BlockExpr::new(last_expr=Some(x_val));
/// let else_block = BlockExpr::new(last_expr=Some(y_val));
/// let if_expr = IfExpr::{ cond, then_: then_block, else_: Right(else_block) };
///
/// let expect =
///   #|if true {
///   #|  42
///   #|} else {
///   #|  24
///   #|}
///
/// inspect(if_expr, content=expect)
/// ```
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
      logger.write_string(" else ");
      logger.write_string(block.to_string());
    }
  }
}

///| BlockExpr represents a block of code wrapped in `{}` containing statements and an optional final expression.
/// A block can contain multiple statements followed by an optional expression that serves as the block's value.
/// The nested_level field controls indentation for pretty printing.
///
/// ```mbt
/// // Simple block with just statements
/// let i42 = ValueExpr::IntExpr(42);
/// let i42 = ApplyExpr::ValueExpr(i42);
/// let i42 = IfLevelExpr::ApplyExpr(i42);
/// let i42 = Expr::IfLevelExpr(i42);
///
/// let stmt = Stmt::Let(Ident("x"), None, i42);
/// let block1 = BlockExpr::new()
/// block1.push(stmt);
///
/// let expect1 = 
///   #|{
///   #|  let x = 42;
///   #|}
///
/// inspect(block1, content=expect1)
///
/// // Block with statements and final expression
/// let y_val = ValueExpr::IntExpr(100);
/// let y_val = ApplyExpr::ValueExpr(y_val);
/// let y_val = IfLevelExpr::ApplyExpr(y_val);
/// let y_val = Expr::IfLevelExpr(y_val);
///
/// let stmt2 = Stmt::Let(Ident("y"), None, y_val);
/// let final_expr = ValueExpr::IdentExpr(Ident("y"));
/// let final_expr = ApplyExpr::ValueExpr(final_expr);
/// let final_expr = IfLevelExpr::ApplyExpr(final_expr);
/// let final_expr = Expr::IfLevelExpr(final_expr);
///
/// let block2 = BlockExpr::new(last_expr = Some(final_expr));
/// block2.push(stmt2);
///
///
/// let expect2 = 
///   #|{
///   #|  let y = 100;
///   #|  y
///   #|}
///
/// inspect(block2, content=expect2)
///
/// // Empty block
/// let empty_block = BlockExpr::new()
/// inspect(empty_block, content="{\n}")
/// ```
pub struct BlockExpr {
  nested_level: Int // for print indentation
  stmts: Array[Stmt]
  mut last_expr: Expr?
  parent: BlockExpr?
}

pub fn BlockExpr::new(nested_level~:Int = 0, parent~: BlockExpr?=None, last_expr~: Expr? = None) -> BlockExpr {
  BlockExpr::{
    nested_level,
    stmts: [],
    last_expr,
    parent
  }
}

pub fn BlockExpr::set_last_expr(self: Self, expr: Expr) -> Unit {
  self.last_expr = Some(expr);
}

pub fn BlockExpr::push(self: Self, stmt: Stmt) -> Unit {
  self.stmts.push(stmt);
}

pub impl Show for BlockExpr with output(self, logger) {
  // no ident before the first LBrace
  logger.write_string("{\n");
  let indent = "  ".repeat(self.nested_level + 1);
  for stmt in self.stmts {
    logger.write_string(indent);
    logger.write_string(stmt.to_string());
    logger.write_string("\n");
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

///| MatchExpr represents pattern matching expressions with multiple arms.
///
/// ```mbt
/// let x = ValueExpr::IdentExpr(Ident("x"));
/// let x = ApplyExpr::ValueExpr(x);
/// let x = IfLevelExpr::ApplyExpr(x);
/// let x = Expr::IfLevelExpr(x);
///
/// let val1 = ValueExpr::IntExpr(1);
/// let val1 = ApplyExpr::ValueExpr(val1);
/// let val1 = IfLevelExpr::ApplyExpr(val1);
/// let val1 = Expr::IfLevelExpr(val1);
///
/// let val2 = ValueExpr::IntExpr(2);
/// let val2 = ApplyExpr::ValueExpr(val2);
/// let val2 = IfLevelExpr::ApplyExpr(val2);
/// let val2 = Expr::IfLevelExpr(val2);
///
/// let arms = [
///   (Pattern::Number(1), val1),
///   (Pattern::WildCard, val2)
/// ];
/// let match_expr = MatchExpr::{ nested_level: 0, expr: x, arms };
///
/// let expect =
///   #|match x {
///   #|  1 => 1,
///   #|  _ => 2,
///   #|}
///
/// inspect(match_expr, content=expect)
/// ```
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

///| Stmt represents different types of statements in the language.
/// Statements are instructions that perform actions but do not return values.
///
/// ```mbt
/// // Let statement with type annotation
/// let val42 = ValueExpr::IntExpr(42);
/// let val42 = ApplyExpr::ValueExpr(val42);
/// let val42 = IfLevelExpr::ApplyExpr(val42);
/// let val42 = Expr::IfLevelExpr(val42);
/// let let_stmt = Stmt::Let(Ident("x"), Some(Type::Int), val42);
/// inspect(let_stmt, content="let x: Int = 42;")
///
/// // Mutable let statement
/// let mut_stmt = Stmt::LetMut(Ident("counter"), None, val42);
/// inspect(mut_stmt, content="let mut counter = 42;")
///
/// // Tuple destructuring let
/// let bindings = [Binding::Ident(Ident("a")), Binding::Ident(Ident("b"))];
/// let tuple_val = ValueExpr::TupleExpr([val42, val42]);
/// let tuple_val = ApplyExpr::ValueExpr(tuple_val);
/// let tuple_val = IfLevelExpr::ApplyExpr(tuple_val);
/// let tuple_val = Expr::IfLevelExpr(tuple_val);
/// let tuple_stmt = Stmt::LetTuple(bindings, None, tuple_val);
/// inspect(tuple_stmt, content="let (a, b) = (42, 42);")
///
/// // Assignment statement
/// let left_val = LeftValue::Ident(Ident("x"));
/// let assign_stmt = Stmt::Assign(left_val, val42);
/// inspect(assign_stmt, content="x = 42;")
///
/// // While loop
/// let cond = ValueExpr::BoolExpr(true);
/// let cond = ApplyExpr::ValueExpr(cond);
/// let cond = IfLevelExpr::ApplyExpr(cond);
/// let cond = Expr::IfLevelExpr(cond);
/// let body = BlockExpr::new()
/// let while_stmt = Stmt::While(cond, body);
/// inspect(while_stmt, content="while true {\n}")
///
/// // Return statement
/// let return_stmt = Stmt::Return(val42);
/// inspect(return_stmt, content="return 42;")
///
/// // Expression statement
/// let expr_stmt = Stmt::ExprStmt(val42);
/// inspect(expr_stmt, content="42;")
/// ```
pub(all) enum Stmt {
  LetTuple(Array[Binding], Type?, Expr)
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
    LetTuple(bindings, ty_opt, expr) => {
      let bindings_str = bindings.map(b => b.to_string()).join(", ");
      let ty_str = match ty_opt {
        Some(ty) => ": \{ty}";
        None => "";
      }
      logger.write_string("let (\{bindings_str})\{ty_str} = \{expr};");
    }
    LetMut(ident, ty_opt, expr) => {
      let ty_str = match ty_opt {
        Some(ty) => ": \{ty}";
        None => "";
      }
      logger.write_string("let mut \{ident}\{ty_str} = \{expr};");
    }
    Let(ident, ty_opt, expr) => {
      let ty_str = match ty_opt {
        Some(ty) => ": \{ty}";
        None => "";
      }
      logger.write_string("let \{ident}\{ty_str} = \{expr};");
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
      logger.write_string("\{left_value} = \{expr};");
    }
    While(cond, body) => {
      logger.write_string("while \{cond} ");
      logger.write_string(body.to_string());
    }
    Return(expr) => {
      logger.write_string("return \{expr};");
    }
    ExprStmt(expr) => {
      logger.write_string("\{expr};");
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

///| TopLet represents top-level let bindings with optional type annotations.
/// These are global variable declarations that can be used throughout the program.
///
/// ```mbt
/// let val42 = ValueExpr::IntExpr(42);
/// let val42 = ApplyExpr::ValueExpr(val42);
/// let val42 = IfLevelExpr::ApplyExpr(val42);
/// let val42 = Expr::IfLevelExpr(val42);
///
/// // Top-level let without type annotation
/// let top_let1 = TopLet::{ name: "global_var", ty: None, value: val42 };
/// inspect(top_let1, content="let global_var = 42;")
///
/// // Top-level let with type annotation
/// let top_let2 = TopLet::{ name: "typed_var", ty: Some(Type::Int), value: val42 };
/// inspect(top_let2, content="let typed_var: Int = 42;")
///
/// // Top-level let with complex expression
/// let array_val = ValueExpr::ArrayExpr([val42, val42]);
/// let array_val = ApplyExpr::ValueExpr(array_val);
/// let array_val = IfLevelExpr::ApplyExpr(array_val);
/// let array_val = Expr::IfLevelExpr(array_val);
/// let top_let3 = TopLet::{ name:"array_var", ty: Some(Type::Array(Type::Int)), value: array_val };
/// inspect(top_let3, content="let array_var: Array[Int] = [42, 42];")
/// ```
pub(all) struct TopLet {
  name: Ident
  ty: Type?
  value: Expr
}

pub impl Show for TopLet with output(self, logger) {
  let ty_str = match self.ty {
    Some(ty) => ": \{ty}";
    None => "";
  }
  logger.write_string("let \{self.name}\{ty_str} = \{self.value};");
}

///| TopFuncDef represents top-level function definitions with optional generic parameters.
/// These are function declarations that can be called from anywhere in the program.
///
/// ```mbt
/// let val42 = ValueExpr::IntExpr(42);
/// let val42 = ApplyExpr::ValueExpr(val42);
/// let val42 = IfLevelExpr::ApplyExpr(val42);
/// let val42 = Expr::IfLevelExpr(val42);
/// let body = BlockExpr::new(last_expr=Some(val42));
///
/// // Simple function without generic parameters
/// let params1 : Array[(Ident, Type)] = [("x", Int), ("y", Int)];
/// let func1 = TopFuncDef::{ 
///   generic_param: None, 
///   name: "add", 
///   params: params1, 
///   ret_ty: Type::Int, 
///   body 
/// };
/// inspect(func1, content="fn add(x: Int, y: Int) -> Int {\n  42\n}")
///
/// // Generic function with type parameter
/// let params2: Array[(Ident, Type)] = [("x", GenericDef("T"))];
/// let func2 = TopFuncDef::{ 
///   generic_param: Some(Upper("T")), 
///   name: "identity", 
///   params: params2, 
///   ret_ty: Type::GenericDef(Upper("T")), 
///   body 
/// };
/// inspect(func2, content="fn[T] identity(x: T) -> T {\n  42\n}")
///
/// // Function with no parameters
/// let func3 = TopFuncDef::{ 
///   generic_param: None, 
///   name: "get_answer", 
///   params: [], 
///   ret_ty: Type::Int, 
///   body 
/// };
/// inspect(func3, content="fn get_answer() -> Int {\n  42\n}")
/// ```
pub(all) struct TopFuncDef {
  generic_param: Upper?
  name: Ident
  params: Array[(Ident, Type)]
  ret_ty: Type
  body: BlockExpr
}

pub impl Show for TopFuncDef with output(self, logger) {
  let generic_str = match self.generic_param {
    Some(param) => "[\{param}]";
    None => "";
  }
  let params_str = self.params.map(p => "\{p.0}: \{p.1}").join(", ");
  logger.write_string("fn\{generic_str} \{self.name}(\{params_str}) -> \{self.ret_ty} ");
  logger.write_string(self.body.to_string());
}

///| StructDef represents struct type definitions with optional generic parameters.
/// Structs define custom data types with named fields.
///
/// ```mbt
/// // Simple struct without generic parameters
/// let fields1 : Array[(Ident, Type)] = [("x", Int), ("y", Int)];
/// let struct1 = StructDef::{ 
///   generic_param: None, 
///   name: Upper("Point"), 
///   fields: fields1 
/// };
/// let expect1 = 
///   #|struct Point {
///   #|  x: Int
///   #|  y: Int
///   #|}
/// inspect(struct1, content=expect1)
///
/// // Generic struct with type parameter
/// let fields2: Array[(Ident, Type)] = [("value", GenericDef("T"))];
/// let struct2 = StructDef::{ 
///   generic_param: Some(Upper("T")), 
///   name: Upper("Container"), 
///   fields: fields2 
/// };
/// let expect2 = 
///   #|struct Container[T] {
///   #|  value: T
///   #|}
/// inspect(struct2, content=expect2)
///
/// // Empty struct
/// let struct3 = StructDef::{ 
///   generic_param: None, 
///   name: Upper("Empty"), 
///   fields: [] 
/// };
/// inspect(struct3, content="struct Empty {}\n")
/// ```
pub(all) struct StructDef {
  generic_param: Upper?
  name: Upper
  fields: Array[(Ident, Type)]
}

pub fn StructDef::type_of(self: Self) -> Type {
  Type::Struct(self.name, self.fields)
}

pub impl Show for StructDef with output(self, logger) {
  let generic_str = match self.generic_param {
    Some(param) => "[\{param}]";
    None => "";
  }
  logger.write_string("struct \{self.name}\{generic_str} {");
  if self.fields.is_empty() {
    logger.write_string("}\n");
    return;
  }
  logger.write_string("\n");
  let indent = "  ";
  for field in self.fields {
    let (name, ty) = field;
    logger.write_string(indent);
    logger.write_string("\{name}: \{ty}\n");
  }
  logger.write_string("}");
}

///| EnumDef represents enum type definitions with optional generic parameters.
/// Enums define algebraic data types with multiple variants that can have associated data.
///
/// ```mbt
/// // Simple enum without generic parameters and with various variant types
/// let variants1 : Array[(Upper, Array[Type])] = [
///   ("Red", []),
///   ("Green", []),
///   ("Blue", []),
///   ("RGB", [Int, Int, Int])
/// ];
/// let enum1 = EnumDef::{ 
///   generic_param: None, 
///   name: Upper("Color"), 
///   variants: variants1 
/// };
/// let expect1 = 
///   #|enum Color {
///   #|  Red
///   #|  Green
///   #|  Blue
///   #|  RGB(Int, Int, Int)
///   #|}
/// inspect(enum1, content=expect1)
///
/// // Generic enum with type parameter
/// let variants2 : Array[(Upper, Array[Type])] = [
///   ("None", []),
///   ("Some", [GenericDef("T")])
/// ];
/// let enum2 = EnumDef::{ 
///   generic_param: Some(Upper("T")), 
///   name: Upper("Option"), 
///   variants: variants2 
/// };
/// let expect2 = 
///   #|enum Option[T] {
///   #|  None
///   #|  Some(T)
///   #|}
/// inspect(enum2, content=expect2)
///
/// // Empty enum
/// let enum3 = EnumDef::{ 
///   generic_param: None, 
///   name: Upper("Never"), 
///   variants: [] 
/// };
/// inspect(enum3, content="enum Never {}\n")
/// ```
pub(all) struct EnumDef {
  generic_param: Upper?
  name: Upper
  variants: Array[(Upper, Array[Type])] // e.g. Point(x: Int, y: Int)
}

pub fn EnumDef::type_of(self: Self) -> Type {
  Type::Enum(self.name, self.variants)
}

pub impl Show for EnumDef with output(self, logger) {
  let generic_str = match self.generic_param {
    Some(param) => "[\{param}]";
    None => "";
  }
  logger.write_string("enum \{self.name}\{generic_str} {");
  if self.variants.is_empty() {
    logger.write_string("}\n");
    return;
  }
  logger.write_string("\n");
  let indent = "  ";
  for variant in self.variants {
    let (name, fields) = variant;
    logger.write_string(indent);
    logger.write_string("\{name}");
    if fields.is_empty() {
      logger.write_string("\n");
      continue;
    }
    let fields_str = fields.map(f => f.to_string()).join(", ");
    logger.write_string("(\{fields_str})");
    logger.write_string("\n");
  }
  logger.write_string("}");
}

pub enum TopDecl {
  TopLet(TopLet)
  TopFuncDef(TopFuncDef)
  StructDef(StructDef)
  EnumDef(EnumDef)
}

pub impl Show for TopDecl with output(self, logger) {
  match self {
    TopLet(top_let) => logger.write_string(top_let.to_string());
    TopFuncDef(top_func_def) => logger.write_string(top_func_def.to_string());
    StructDef(struct_def) => logger.write_string(struct_def.to_string());
    EnumDef(enum_def) => logger.write_string(enum_def.to_string());
  }
}

pub struct Program {
  top_decls: Array[TopDecl]
}

pub impl Show for Program with output(self, logger) {
  for decl in self.top_decls {
    logger.write_string(decl.to_string());
    logger.write_string("\n\n");
  }
}
