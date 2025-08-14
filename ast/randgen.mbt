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

fn[K:Eq+Hash, V] Env::set(self: Self[K, V], key: K, value: V) -> Unit {
  self.map.set(key, value);
}

fn[K, V: Eq] Env::collect_keys_by_cond(self: Self[K, V], cond: (V) -> Bool) -> Array[K] {
  let keys = Array::new();
  for pair in self.map {
    let (key, v) = pair;
    if cond(v) {
      keys.push(key);
    }
  }
  if self.parent is Some(parent) {
    keys.append(parent.collect_keys_by_cond(cond));
  }
  keys
}

fn[K, V: Eq] Env::collect_keys_by_value(self: Self[K, V], value: V) -> Array[K] {
  self.collect_keys_by_cond((v) => v == value);
}

pub struct RandAstGenerator {
  rand: @random.Rand
  upper_max_len: Int
  ident_max_len: Int
  tuple_type_max_len: Int
  direct_array_max_len: Int
  param_types_max_len: Int
  expr_max_depth: Int
  stmt_max_num: Int
  ty_env: Env[Upper, Type]
  id_env: Env[Ident, Type]
  generic: Upper?

  // structs
  structs: Map[Upper, StructDef]
  enums: Map[Upper, EnumDef]
  toplets: Map[Upper, Type]
  topfuncs: Map[Ident, TopFuncDef]
}

pub fn RandAstGenerator::new(
  upper_max_len~: Int = 10,
  ident_max_len~:Int=10,
  tuple_type_max_len~: Int = 4,
  direct_array_max_len~: Int = 10,
  param_types_max_len~: Int = 15,
  expr_max_depth~: Int = 5,
  stmt_max_num~: Int = 30,
  generic~: Upper? = None) -> RandAstGenerator {
  RandAstGenerator::{
    rand: @random.Rand::new(),
    upper_max_len,
    ident_max_len,
    tuple_type_max_len,
    direct_array_max_len,
    param_types_max_len,
    expr_max_depth,
    stmt_max_num,
    ty_env: Env::new(),
    id_env: Env::new(),
    generic,
    structs: Map::new(),
    enums: Map::new(),
    toplets: Map::new(),
    topfuncs: Map::new(),
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
  let n = self.rand.int();
  let n = if n < 0 { -n } else { n };
  n % limit
}

pub fn[T] RandAstGenerator::choice(self: Self, choices: Array[T]) -> T? {
  let length = choices.length();
  if length == 0 {
    return None;
  }
  let index = self.pos_int(length);
  Some(choices[index])
}

const UPPER_CHARS: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
const LOWER_CHARS: String = "abcdefghijklmnopqrstuvwxyz";
const DIGITS: String = "0123456789";
const IDENT_CHARS: String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_";
const NON_UPPER_CHARS: String = "abcdefghijklmnopqrstuvwxyz0123456789_";

///|
pub fn RandAstGenerator::new_upper(self: Self) -> Upper {
  fn helper() {
    let str_builder = StringBuilder::new();
    let first_char = self.choice(UPPER_CHARS.to_array()).unwrap();
    str_builder.write_char(first_char);
    for i in 0..<self.pos_int(self.upper_max_len) {
      let c = self.choice(IDENT_CHARS.to_array()).unwrap();
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
pub fn RandAstGenerator::new_ident(self: Self) -> Ident {
  fn helper() -> Ident {
    let str_builder = StringBuilder::new();
    let first_char = self.choice(NON_UPPER_CHARS.to_array()).unwrap();
    str_builder.write_char(first_char);
    for i in 0..<self.pos_int(self.ident_max_len) {
      let c = self.choice(IDENT_CHARS.to_array()).unwrap();
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

pub fn RandAstGenerator::gen_type(self: Self) -> Type {
  match self.pos_int(22) {
    0..<2 => Type::Unit;
    2..<4 => Bool;
    4..<7 => Int;
    7..<10 => Double;
    11..<13 => Array(self.gen_type());
    13..<15 => self.gen_tuple_type();
    15..<16 => self.gen_func_type();
    16..<18 => self.gen_struct_type();
    18..<20 => self.gen_enum_type();
    20..<22 => {
      if self.generic is Some(generic) {
        GenericDef(generic)
      } else {
        self.gen_type()
      }
    }
    _ => self.gen_generic_sub_type(); // e.g. Option[T]
  }
}

fn RandAstGenerator::gen_tuple_type(self: Self) -> Type {
  let types = Array::new();
  for i in 0..<self.pos_int(self.tuple_type_max_len) {
    types.push(self.gen_type());
  }
  while types.length() < 2 {
    types.push(Unit);
  }
  Tuple(types)
}

// Cannot randomly generate a struct or enum type or generic sub type
// must choose from existing ones
fn RandAstGenerator::gen_struct_type(self: Self) -> Type {
  let structs = self.structs.keys().collect();
  let name = match self.choice(structs) {
    Some(name) => name;
    None => return self.gen_type();
  }
  let s = self.structs.get(name).unwrap()
  s.type_of()
}

fn RandAstGenerator::gen_enum_type(self: Self) -> Type {
  let enums = self.enums.keys().collect();
  let name = match self.choice(enums) {
    Some(name) => name;
    None => return self.gen_type();
  }
  let e = self.enums.get(name).unwrap()
  e.type_of()
}

fn RandAstGenerator::gen_generic_sub_type(self: Self) -> Type {
  fn gen_generic_sub_type_from_struct_defs() -> Upper? {
    let generic_structs : Array[Upper] = Array::new();
    for struct_def in self.structs.values() {
      if struct_def.generic_param is Some(u) {
        generic_structs.push(u);
      }
    }
    self.choice(generic_structs)
  }
  fn gen_generic_sub_type_from_enum_defs() -> Upper? {
    let generic_enums : Array[Upper] = Array::new();
    for enum_def in self.enums.values() {
      if enum_def.generic_param is Some(u) {
        generic_enums.push(u);
      }
    }
    self.choice(generic_enums)
  }
  let master = match self.pos_int(2) {
    0 => gen_generic_sub_type_from_struct_defs();
    _ => gen_generic_sub_type_from_enum_defs();
  }

  match master {
    Some(master) => {
      let sub_type = self.gen_type();
      GenericSub(master, sub_type)
    }
    None => return self.gen_type();
  }
}

fn RandAstGenerator::gen_func_type(self: Self) -> Type {
  let param_types = Array::new();
  for i in 0..<self.pos_int(self.param_types_max_len) {
    param_types.push(self.gen_type());
  }
  if param_types.length() == 0 {
    param_types.push(Unit);
  }
  let ret_type = self.gen_type();
  Func(param_types, ret_type)
}

fn RandAstGenerator::find_ident_by_type(self: Self, ty: Type) -> Ident? {
  let keys = self.id_env.collect_keys_by_value(ty);
  if keys.length() == 0 {
    return None;
  }
  let index = self.pos_int(keys.length());
  Some(keys[index])
}

//pub(all) enum Type {
//  Unit
//  Bool
//  Int
//  Double
//  Array(Type)
//  Tuple(Array[Type])
//  Func(Array[Type], Type) // (Int, Bool) -> Int
//
//  // User-defined type, e.g. Point
//  Struct(Upper, Array[(Ident, Type)])
//  Enum(Upper, Array[(Upper, Array[Type])]) // e.g. Color::Red(Int, Int, Int)
//  GenericDef(Upper)  // T
//  GenericSub(Upper, Type) // Complex[Int]
//}
// generate a value expression by a given type
fn RandAstGenerator::gen_value_expr(self: Self, ty: Type, deepth: Int) -> ValueExpr {
  fn get_value_expr_helper(ty: Type, default_expr: () -> ValueExpr) -> ValueExpr {
    let n = if deepth < self.expr_max_depth {
      self.pos_int(10)
    } else {
      self.pos_int(6)
    }
    match n {
      0..<3 => default_expr(); // construct it directly
      3..<6 => {
        let ident = self.find_ident_by_type(ty);
        match ident {
          Some(ident) => ValueExpr::IdentExpr(ident);
          None => default_expr();
        }
      }
      6..<8 => {
        let expr = self.gen_expr(ty, deepth + 1);
        ValueExpr::GroupExpr(expr);
      }
      _ => {
        let block_expr = self.gen_block_expr(ty, deepth + 1);
        ValueExpr::BlockExpr(block_expr);
      }
    }
  }
  match ty {
    Unit => get_value_expr_helper(Unit, () => ValueExpr::UnitExpr);
    Bool => get_value_expr_helper(Bool, () => ValueExpr::BoolExpr(self.gen_bool()));
    Int => get_value_expr_helper(Int, () => ValueExpr::IntExpr(self.int(1000)));
    Double => get_value_expr_helper(Double, () => ValueExpr::FloatExpr(self.gen_double()));
    Array(elem_ty) => {
      let default_expr = () => {
        match self.pos_int(2) {
          0 => ValueExpr::ArrayMake(self.gen_expr(Int, deepth + 1), self.gen_expr(elem_ty, deepth + 1));
          _ => {
            let exprs = Array::new();
            for i in 0..<self.pos_int(self.direct_array_max_len) {
              exprs.push(self.gen_expr(elem_ty, deepth + 1));
            }
            ValueExpr::ArrayExpr(exprs);
          }
        }
      }
      get_value_expr_helper(Array(elem_ty), default_expr)
    }
    Tuple(elem_tys) => {
      let default_expr = () => {
        let exprs = Array::new();
        for elem_ty in elem_tys {
          exprs.push(self.gen_expr(elem_ty, deepth + 1));
        }
        ValueExpr::TupleExpr(exprs);
      }
      get_value_expr_helper(Tuple(elem_tys), default_expr)
    }
    //Func()
    // GenericDef(Upper)
    // GenericSub(Upper, Type)
    Struct(name, fields) => {
      let default_expr = () => {
        let field_exprs = Array::new();
        for field in fields {
          let (field_name, field_ty) = field
          let expr = self.gen_expr(field_ty, deepth + 1);
          field_exprs.push((field_name, expr));
        }
        ValueExpr::StructConstruct(name, field_exprs);
      }
      get_value_expr_helper(Struct(name, fields), default_expr)
    }
    Enum(name, variants) => {
      let default_expr = () => {
        let variant = self.choice(variants);
        match variant {
          Some((variant_name, variant_tys)) => {
            let exprs = Array::new();
            for ty in variant_tys {
              exprs.push(self.gen_expr(ty, deepth + 1));
            }
            let name = if self.pos_int(2) > 0 { Some(name) } else { None };
            ValueExpr::EnumConstruct(name, variant_name, exprs);
          }
          None => ValueExpr::UnitExpr;
        }
      }
      get_value_expr_helper(Enum(name, variants), default_expr)
    }
    _ => {
      println("Unimplemented")
      panic()
    }
  }
}


fn RandAstGenerator::gen_apply_expr(self: Self, ty: Type, deepth: Int) -> ApplyExpr {
  let n = if deepth < self.expr_max_depth {
    self.pos_int(6)
  } else {
    self.pos_int(3)
  }
  match n {
    0..<2 => {
      let value_expr = self.gen_value_expr(ty, deepth);
      ApplyExpr::ValueExpr(value_expr);
    }
    2..<3 => {
      let aexpr = self.gen_apply_expr(ty, deepth + 1);
      let index_expr = self.gen_expr(Int, deepth + 1);
      ApplyExpr::ArrAcc(aexpr, index_expr);
    }
    3..<4 => {
      let aexpr = self.gen_apply_expr(ty, deepth + 1);
      let ident = self.new_ident();
      ApplyExpr::DotAcc(aexpr, ident);
    }
    _ => {
      let aexpr = self.gen_apply_expr(ty, deepth + 1);
      let args = Array::new();
      for i in 0..<self.pos_int(3) {
        args.push(self.gen_expr(ty, deepth + 1));
      }
      ApplyExpr::Call(aexpr, args);
    }
  }
}

fn RandAstGenerator::gen_expr(self: Self, ty: Type, deepth: Int) -> Expr {
  let n = if deepth < self.expr_max_depth {
    self.pos_int(18)
  } else {
    self.pos_int(2)
  }
  match n {
    2..<4 => {
      let left = self.gen_expr(ty, deepth + 1);
      let right = self.gen_expr(ty, deepth + 1);
      Expr::AndExpr(left, right);
    }
    4..<6 => {
      let left = self.gen_expr(ty, deepth + 1);
      let right = self.gen_expr(ty, deepth + 1);
      Expr::OrExpr(left, right);
    }
    6..<8 => {
      let left = self.gen_expr(ty, deepth + 1);
      let right = self.gen_expr(ty, deepth + 1);
      let cmp_op = match self.pos_int(6) {
        0 => Eq;
        1 => Ne;
        2 => Lt;
        3 => Le;
        4 => Gt;
        _ => Ge;
      }
      Expr::CmpExpr(cmp_op, left, right);
    }
    8..<10 => {
      let left = self.gen_expr(ty, deepth + 1);
      let right = self.gen_expr(ty, deepth + 1);
      Expr::AddExpr(left, right);
    }
    10..<12 => {
      let left = self.gen_expr(ty, deepth + 1);
      let right = self.gen_expr(ty, deepth + 1);
      Expr::SubExpr(left, right);
    }
    12..<14 => {
      let left = self.gen_expr(ty, deepth + 1);
      let right = self.gen_expr(ty, deepth + 1);
      Expr::MulExpr(left, right);
    }
    14..<16 => {
      let left = self.gen_expr(ty, deepth + 1);
      let right = self.gen_expr(ty, deepth + 1);
      Expr::DivExpr(left, right);
    }
    16..<18 => {
      let left = self.gen_expr(ty, deepth + 1);
      let right = self.gen_expr(ty, deepth + 1);
      Expr::ModExpr(left, right);
    }
    _ => IfLevelExpr(self.gen_if_level_expr(ty, deepth));
  }
}
//pub(all) enum IfLevelExpr {
//  ApplyExpr(ApplyExpr)
//  IfExpr(IfExpr)
//  MatchExpr(MatchExpr)
//}
fn RandAstGenerator::gen_if_level_expr(self: Self, ty: Type, deepth: Int) -> IfLevelExpr {
  let n = if deepth < self.expr_max_depth {
    self.pos_int(4)
  } else {
    self.pos_int(2)
  }
  match n {
    2..<3 => {
      let if_expr = self.gen_if_expr(ty, deepth + 1);
      IfLevelExpr::IfExpr(if_expr);
    }
    3..<4 => {
      let match_expr = self.gen_match_expr(ty, deepth + 1);
      IfLevelExpr::MatchExpr(match_expr);
    }
    _ => {
      let aexpr = self.gen_apply_expr(ty, deepth);
      IfLevelExpr::ApplyExpr(aexpr);
    }
  }
}

fn RandAstGenerator::gen_block_expr(self: Self, ty: Type, deepth: Int) -> BlockExpr {
  ...
}

fn RandAstGenerator::gen_if_expr(self: Self, ty: Type, deepth: Int) -> IfExpr {
  ...
}

fn RandAstGenerator::gen_match_expr(self: Self, ty: Type, deepth: Int) -> MatchExpr {
  ...
}

fn RandAstGenerator::gen_stmt(self: Self) -> Stmt {
  ...
}
