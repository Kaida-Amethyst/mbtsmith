//struct Env[K, V] {
//  map: Map[K, V]
//  parent: Env[K, V]?
//}
//
//fn[K, V] Env::new() -> Env[K, V] {
//  Env::{ map: Map::new(), parent: None }
//}
//
//fn[K, V] Env::sub_env(self: Self[K, V]) -> Env[K, V] {
//  Env::{ map: Map::new(), parent: Some(self) }
//}
//
//fn[K:Eq+Hash, V] Env::get(self: Self[K, V], key: K) -> V? {
//  match self.map.get(key) {
//    Some(value) => Some(value);
//    None => match self.parent {
//      Some(parent) => parent.get(key);
//      None => None;
//    }
//  }
//}
//
//fn[K:Eq+Hash, V] Env::set(self: Self[K, V], key: K, value: V) -> Unit {
//  self.map.set(key, value);
//}
//
//fn[K, V: Eq] Env::collect_keys_by_cond(self: Self[K, V], cond: (V) -> Bool) -> Array[K] {
//  let keys = Array::new();
//  for pair in self.map {
//    let (key, v) = pair;
//    if cond(v) {
//      keys.push(key);
//    }
//  }
//  if self.parent is Some(parent) {
//    keys.append(parent.collect_keys_by_cond(cond));
//  }
//  keys
//}
//
//fn[K, V: Eq] Env::collect_keys_by_value(self: Self[K, V], value: V) -> Array[K] {
//  self.collect_keys_by_cond((v) => v == value);
//}
//
//pub(all) struct RandAstGenerator {
//  rand: @random.Rand
//  upper_max_len: Int
//  ident_max_len: Int
//  tuple_type_max_len: Int
//  direct_array_max_len: Int
//  param_types_max_len: Int
//  expr_max_depth: Int
//  stmt_max_num: Int
//  let_tuple_max_bindings: Int
//  ty_env: Env[Upper, Type]
//  id_env: Env[Ident, Type]
//  mutable_id_env: Env[Ident, Type]
//  mut generic: Upper?
//
//  mut curr_block : BlockExpr?
//  ret_ty: Type
//
//  // structs
//  structs: Map[Upper, StructDef]
//  enums: Map[Upper, EnumDef]
//  toplets: Map[Upper, Type]
//  topfuncs: Map[Ident, TopFuncDef]
//}
//
//pub fn RandAstGenerator::new(
//  upper_max_len~: Int = 10,
//  ident_max_len~:Int=10,
//  tuple_type_max_len~: Int = 4,
//  direct_array_max_len~: Int = 10,
//  param_types_max_len~: Int = 7,
//  expr_max_depth~: Int = 5,
//  stmt_max_num~: Int = 30,
//  let_tuple_max_bindings~: Int = 4,
//  generic~: Upper? = None,
//  block~: BlockExpr? =None,
//  ret_ty~: Type = Unit
//) -> RandAstGenerator {
//  RandAstGenerator::{
//    rand: @random.Rand::new(),
//    upper_max_len,
//    ident_max_len,
//    tuple_type_max_len,
//    direct_array_max_len,
//    param_types_max_len,
//    expr_max_depth,
//    stmt_max_num,
//    let_tuple_max_bindings,
//    ty_env: Env::new(),
//    id_env: Env::new(),
//    mutable_id_env: Env::new(),
//    generic,
//    curr_block: block,
//    ret_ty,
//    structs: Map::new(),
//    enums: Map::new(),
//    toplets: Map::new(),
//    topfuncs: Map::new(),
//  }
//}
//
/////|
//pub fn RandAstGenerator::int(self: Self, limit: Int) -> Int {
//  self.rand.int(limit~)
//}
//
/////|
/////
///// Always return positive integers.
//pub fn RandAstGenerator::pos_int(self: Self, limit: Int) -> Int {
//  let limit = limit.abs()
//  let n = self.rand.int();
//  let n = if n < 0 { -n } else { n };
//  n % limit
//}
//
//pub fn[T] RandAstGenerator::choice(self: Self, choices: Array[T]) -> T? {
//  let length = choices.length();
//  if length == 0 {
//    return None;
//  }
//  let index = self.pos_int(length);
//  Some(choices[index])
//}
//
//const UPPER_CHARS: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
//const LOWER_CHARS: String = "abcdefghijklmnopqrstuvwxyz";
//const DIGITS: String = "0123456789";
//const IDENT_CHARS: String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_";
//const NON_UPPER_CHARS: String = "abcdefghijklmnopqrstuvwxyz0123456789_";
//
/////|
//pub fn RandAstGenerator::new_upper(self: Self) -> Upper {
//  fn helper() {
//    let str_builder = StringBuilder::new();
//    let first_char = self.choice(UPPER_CHARS.to_array()).unwrap();
//    str_builder.write_char(first_char);
//    for i in 0..<self.pos_int(self.upper_max_len) {
//      let c = self.choice(IDENT_CHARS.to_array()).unwrap();
//      str_builder.write_char(c);
//    }
//    Upper(str_builder.to_string())
//  }
//  let mut upper = helper()
//  if self.ty_env.get(upper) is Some(_) {
//    upper = helper()
//  }
//  upper
//}
//
/////|
//pub fn RandAstGenerator::new_ident(self: Self) -> Ident {
//  fn helper() -> Ident {
//    let str_builder = StringBuilder::new();
//    let first_char = self.choice(LOWER_CHARS.to_array()).unwrap();
//    str_builder.write_char(first_char);
//    for i in 0..<self.pos_int(self.ident_max_len) {
//      let c = self.choice(IDENT_CHARS.to_array()).unwrap();
//      str_builder.write_char(c);
//    }
//    Ident(str_builder.to_string())
//  }
//  let mut ident = helper();
//  while self.id_env.get(ident) is Some(_) {
//    ident = helper()
//  }
//  ident
//}
//
//pub fn RandAstGenerator::new_binding(self: Self) -> Binding {
//  if self.pos_int(6) > 0 {
//    Ident(self.new_ident())
//  } else {
//    WhileCard
//  }
//}
//
//pub fn RandAstGenerator::gen_double(self: Self) -> Double {
//  self.rand.double()
//}
//
//pub fn RandAstGenerator::gen_bool(self: Self) -> Bool {
//  self.rand.int() % 2 == 0
//}
//
//pub fn RandAstGenerator::gen_type(self: Self) -> Type {
//  match self.pos_int(22) {
//    0..<2 => Type::Unit;
//    2..<4 => Bool;
//    4..<7 => Int;
//    7..<10 => Double;
//    11..<13 => Array(self.gen_type());
//    13..<15 => self.gen_tuple_type();
//    15..<16 => self.gen_func_type();
//    16..<18 => self.gen_struct_type();
//    18..<20 => self.gen_enum_type();
//    20..<22 => {
//      if self.generic is Some(generic) {
//        GenericDef(generic)
//      } else {
//        // Return a basic type instead of recursive call
//        match self.pos_int(4) {
//          0 => Type::Unit;
//          1 => Bool;
//          2 => Int;
//          _ => Double;
//        }
//      }
//    }
//    _ => self.gen_generic_sub_type(); // e.g. Option[T]
//  }
//}
//
//fn RandAstGenerator::gen_tuple_type(self: Self, size~: Int? = None) -> Type {
//  let types = Array::new();
//  if size is Some(size) {
//    for i in 0 ..< size {
//      types.push(self.gen_type());
//    }
//    return Tuple(types);
//  }
//  for i in 0..<self.pos_int(self.tuple_type_max_len) {
//    types.push(self.gen_type());
//  }
//  while types.length() < 2 {
//    types.push(Unit);
//  }
//  Tuple(types)
//}
//
//// Cannot randomly generate a struct or enum type or generic sub type
//// must choose from existing ones
//fn RandAstGenerator::gen_struct_type(self: Self) -> Type {
//  let structs = self.structs.keys().collect();
//  let name = match self.choice(structs) {
//    Some(name) => name;
//    None => return Int; // Return basic type instead of recursive call
//  }
//  // if struct is generic, return basic type
//  if self.structs.get(name).unwrap().generic_param is Some(_) {
//    return Int // Return basic type instead of recursive call
//  }
//  let s = self.structs.get(name).unwrap()
//  s.type_of()
//}
//
//fn RandAstGenerator::gen_enum_type(self: Self) -> Type {
//  let enums = self.enums.keys().collect();
//  let name = match self.choice(enums) {
//    Some(name) => name;
//    None => return Bool; // Return basic type instead of recursive call
//  }
//  if self.enums.get(name).unwrap().generic_param is Some(_) {
//    return Bool; // Return basic type instead of recursive call
//  }
//  let e = self.enums.get(name).unwrap()
//  e.type_of()
//}
//
//fn RandAstGenerator::gen_generic_sub_type(self: Self) -> Type {
//  // Collect all generic struct names
//  let generic_structs : Array[Upper] = Array::new();
//  for struct_def in self.structs.values() {
//    if struct_def.generic_param is Some(_) {
//      generic_structs.push(struct_def.name);
//    }
//  }
//
//  // Collect all generic enum names  
//  let generic_enums : Array[Upper] = Array::new();
//  for enum_def in self.enums.values() {
//    if enum_def.generic_param is Some(_) {
//      generic_enums.push(enum_def.name);
//    }
//  }
//
//  // Combine all generic type names
//  let all_generic_types : Array[Upper] = Array::new();
//  all_generic_types.append(generic_structs);
//  all_generic_types.append(generic_enums);
//
//  // If no generic types found, return a basic type
//  if all_generic_types.is_empty() {
//    return Double; // Return basic type instead of recursive call
//  }
//
//  // Pick a random generic type and substitute with a concrete type
//  let generic_name = self.choice(all_generic_types).unwrap();
//  let sub_type = self.gen_type();
//  GenericSub(generic_name, sub_type)
//}
//
//fn RandAstGenerator::gen_func_type(self: Self) -> Type {
//  let param_types = Array::new();
//  for i in 0..<self.pos_int(self.param_types_max_len) {
//    param_types.push(self.gen_type());
//  }
//  if param_types.length() == 0 {
//    param_types.push(Unit);
//  }
//  let ret_type = self.gen_type();
//  Func(param_types, ret_type)
//}
//
//fn RandAstGenerator::find_ident_by_type(self: Self, ty: Type) -> Ident? {
//  let keys = self.id_env.collect_keys_by_value(ty);
//  if keys.length() == 0 {
//    return None;
//  }
//  let index = self.pos_int(keys.length());
//  Some(keys[index])
//}
//
////pub(all) enum Type {
////  Unit
////  Bool
////  Int
////  Double
////  Array(Type)
////  Tuple(Array[Type])
////  Func(Array[Type], Type) // (Int, Bool) -> Int
////
////  // User-defined type, e.g. Point
////  Struct(Upper, Array[(Ident, Type)])
////  Enum(Upper, Array[(Upper, Array[Type])]) // e.g. Color::Red(Int, Int, Int)
////  GenericDef(Upper)  // T
////  GenericSub(Upper, Type) // Complex[Int]
////}
//// generate a value expression by a given type
//fn RandAstGenerator::gen_value_expr(self: Self, ty: Type, deepth: Int) -> ValueExpr {
//  fn get_value_expr_helper(ty: Type, default_expr: () -> ValueExpr) -> ValueExpr {
//    let n = if deepth < self.expr_max_depth {
//      self.pos_int(10)
//    } else {
//      self.pos_int(6) // When at max depth, only choose 0-5 to avoid recursive cases
//    }
//    match n {
//      0..<3 => default_expr(); // construct it directly
//      3..<6 => {
//        let ident = self.find_ident_by_type(ty);
//        match ident {
//          Some(ident) => ValueExpr::IdentExpr(ident);
//          None => default_expr();
//        }
//      }
//      6..<8 => {
//        if deepth < self.expr_max_depth {
//          let expr = self.gen_expr(ty, deepth + 1);
//          ValueExpr::GroupExpr(expr)
//        } else {
//          default_expr() // Avoid recursion at max depth
//        }
//      }
//      _ => {
//        if deepth < self.expr_max_depth {
//          let block_expr = self.gen_block_expr(ty);
//          ValueExpr::BlockExpr(block_expr)
//        } else {
//          default_expr() // Avoid recursion at max depth
//        }
//      }
//    }
//  }
//  match ty {
//    Unit => get_value_expr_helper(Unit, () => ValueExpr::UnitExpr);
//    Bool => get_value_expr_helper(Bool, () => ValueExpr::BoolExpr(self.gen_bool()));
//    Int => get_value_expr_helper(Int, () => ValueExpr::IntExpr(self.int(1000)));
//    Double => get_value_expr_helper(Double, () => ValueExpr::FloatExpr(self.gen_double()));
//    Array(elem_ty) => {
//      let default_expr = () => {
//        match self.pos_int(2) {
//          0 => ValueExpr::ArrayMake(self.gen_expr(Int, deepth + 1), self.gen_expr(elem_ty, deepth + 1));
//          _ => {
//            let exprs = Array::new();
//            for i in 0..<self.pos_int(self.direct_array_max_len) {
//              exprs.push(self.gen_expr(elem_ty, deepth + 1));
//            }
//            ValueExpr::ArrayExpr(exprs)
//          }
//        }
//      }
//      get_value_expr_helper(Array(elem_ty), default_expr)
//    }
//    Tuple(elem_tys) => {
//      let default_expr = () => {
//        let exprs = Array::new();
//        for elem_ty in elem_tys {
//          exprs.push(self.gen_expr(elem_ty, deepth + 1));
//        }
//        ValueExpr::TupleExpr(exprs)
//      }
//      get_value_expr_helper(Tuple(elem_tys), default_expr)
//    }
//    Struct(name, fields) => {
//      let default_expr = () => {
//        let field_exprs = Array::new();
//        for field in fields {
//          let (field_name, field_ty) = field
//          let expr = self.gen_expr(field_ty, deepth + 1);
//          field_exprs.push((field_name, expr));
//        }
//        ValueExpr::StructConstruct(name, field_exprs)
//      }
//      get_value_expr_helper(Struct(name, fields), default_expr)
//    }
//    Enum(name, variants) => {
//      let default_expr = () => {
//        let variant = self.choice(variants);
//        match variant {
//          Some((variant_name, variant_tys)) => {
//            let exprs = Array::new();
//            for ty in variant_tys {
//              exprs.push(self.gen_expr(ty, deepth + 1));
//            }
//            let name = if self.pos_int(2) > 0 { Some(name) } else { None };
//            ValueExpr::EnumConstruct(name, variant_name, exprs)
//          }
//          None => ValueExpr::UnitExpr
//        }
//      }
//      get_value_expr_helper(Enum(name, variants), default_expr)
//    }
//    //Func()
//    // GenericDef(Upper)
//    // GenericSub(Upper, Type)
//    _ => ValueExpr::UnitExpr
//  }
//}
//
//
//fn RandAstGenerator::gen_apply_expr(self: Self, ty: Type, deepth: Int) -> ApplyExpr {
//  let n = if deepth < self.expr_max_depth {
//    self.pos_int(6)
//  } else {
//    self.pos_int(3)
//  }
//  match n {
//    0..<2 => {
//      let value_expr = self.gen_value_expr(ty, deepth);
//      ApplyExpr::ValueExpr(value_expr)
//    }
//    2..<3 => {
//      let aexpr = self.gen_apply_expr(ty, deepth + 1);
//      let index_expr = self.gen_expr(Int, deepth + 1);
//      ApplyExpr::ArrAcc(aexpr, index_expr)
//    }
//    3..<4 => {
//      let aexpr = self.gen_apply_expr(ty, deepth + 1);
//      let ident = self.new_ident();
//      ApplyExpr::DotAcc(aexpr, ident)
//    }
//    _ => {
//      let aexpr = self.gen_apply_expr(ty, deepth + 1);
//      let args = Array::new();
//      for i in 0..<self.pos_int(3) {
//        args.push(self.gen_expr(ty, deepth + 1));
//      }
//      ApplyExpr::Call(aexpr, args)
//    }
//  }
//}
//
//fn RandAstGenerator::gen_expr(self: Self, ty: Type, deepth: Int) -> Expr {
//  let n = if deepth < self.expr_max_depth {
//    self.pos_int(18)
//  } else {
//    self.pos_int(2) // This will return 0 or 1, which should match simple cases
//  }
//  match n {
//    0..<2 => {
//      // When at max depth, return simple IfLevelExpr with non-recursive apply
//      IfLevelExpr(self.gen_if_level_expr(ty, deepth))
//    }
//    2..<4 => {
//      let left = self.gen_expr(ty, deepth + 1);
//      let right = self.gen_expr(ty, deepth + 1);
//      Expr::AndExpr(left, right)
//    }
//    4..<6 => {
//      let left = self.gen_expr(ty, deepth + 1);
//      let right = self.gen_expr(ty, deepth + 1);
//      Expr::OrExpr(left, right)
//    }
//    6..<8 => {
//      let left = self.gen_expr(ty, deepth + 1);
//      let right = self.gen_expr(ty, deepth + 1);
//      let cmp_op = match self.pos_int(6) {
//        0 => Eq;
//        1 => Ne;
//        2 => Lt;
//        3 => Le;
//        4 => Gt;
//        _ => Ge;
//      }
//      Expr::CmpExpr(cmp_op, left, right)
//    }
//    8..<10 => {
//      let left = self.gen_expr(ty, deepth + 1);
//      let right = self.gen_expr(ty, deepth + 1);
//      Expr::AddExpr(left, right)
//    }
//    10..<12 => {
//      let left = self.gen_expr(ty, deepth + 1);
//      let right = self.gen_expr(ty, deepth + 1);
//      Expr::SubExpr(left, right)
//    }
//    12..<14 => {
//      let left = self.gen_expr(ty, deepth + 1);
//      let right = self.gen_expr(ty, deepth + 1);
//      Expr::MulExpr(left, right)
//    }
//    14..<16 => {
//      let left = self.gen_expr(ty, deepth + 1);
//      let right = self.gen_expr(ty, deepth + 1);
//      Expr::DivExpr(left, right)
//    }
//    16..<18 => {
//      let left = self.gen_expr(ty, deepth + 1);
//      let right = self.gen_expr(ty, deepth + 1);
//      Expr::ModExpr(left, right)
//    }
//    _ => IfLevelExpr(self.gen_if_level_expr(ty, deepth))
//  }
//}
//
//fn RandAstGenerator::gen_if_level_expr(self: Self, ty: Type, deepth: Int) -> IfLevelExpr {
//  let n = if deepth < self.expr_max_depth {
//    self.pos_int(4)
//  } else {
//    self.pos_int(2) // This will return 0 or 1
//  }
//  match n {
//    0..<2 => {
//      // When at max depth, return simple ApplyExpr
//      let aexpr = self.gen_apply_expr(ty, deepth);
//      IfLevelExpr::ApplyExpr(aexpr)
//    }
//    2..<3 => {
//      let if_expr = self.gen_if_expr(ty);
//      IfLevelExpr::IfExpr(if_expr)
//    }
//    3..<4 => {
//      let match_expr = self.gen_match_expr(ty);
//      IfLevelExpr::MatchExpr(match_expr)
//    }
//    _ => {
//      let aexpr = self.gen_apply_expr(ty, deepth);
//      IfLevelExpr::ApplyExpr(aexpr)
//    }
//  }
//}
//
//fn RandAstGenerator::gen_block_expr(self: Self, ty: Type) -> BlockExpr {
//  let block = BlockExpr::new()
//  self.curr_block = Some(block);
//
//  let gen_stmt_num = self.pos_int(self.stmt_max_num);
//  let mut stmt_num = 0;
//  while stmt_num < gen_stmt_num {
//    let stmt = self.gen_stmt();
//    //println(stmt)
//    block.stmts.push(stmt);
//    stmt_num += 1;
//  }
//
//  let last_expr = if ty != Unit {
//    Some(self.gen_expr(ty, 0));
//  } else {
//    None
//  }
//  block.last_expr = last_expr;
//  block
//}
//
////pub(all) struct IfExpr {
////  cond: Expr
////  then_: BlockExpr
////  else_: Either[IfExpr, BlockExpr]
////}
//fn RandAstGenerator::gen_if_expr(self: Self, ty: Type) -> IfExpr {
//  let cond_expr = self.gen_expr(Bool, 0);
//  let then_block = self.gen_block_expr(ty);
//
//  let else_expr = if self.pos_int(2) > 0 {
//    let if_expr = self.gen_if_expr(ty);
//    Either::Left(if_expr)
//  } else {
//    Either::Right(self.gen_block_expr(ty))
//  }
//
//  IfExpr::{
//    cond: cond_expr,
//    then_: then_block,
//    else_: else_expr,
//  }
//}
//
//fn RandAstGenerator::gen_pattern(self: Self, ty: Type) -> Pattern {
//  match ty {
//    Int => {
//      match self.pos_int(3) {
//        0 => Pattern::Number(self.int(100));
//        1 => Pattern::Ident(self.new_ident());
//        _ => Pattern::WildCard;
//      }
//    }
//    Bool => {
//      match self.pos_int(3) {
//        0 => Pattern::Bool(self.gen_bool());
//        1 => Pattern::Ident(self.new_ident());
//        _ => Pattern::WildCard;
//      }
//    }
//    Tuple(elem_tys) => {
//      match self.pos_int(3) {
//        0 => {
//          let patterns = Array::new();
//          for elem_ty in elem_tys {
//            patterns.push(self.gen_pattern(elem_ty));
//          }
//          Pattern::Tuple(patterns)
//        }
//        1 => Pattern::Ident(self.new_ident());
//        _ => Pattern::WildCard;
//      }
//    }
//    Enum(name, variants) => {
//      match self.pos_int(4) {
//        0 => {
//          let variant = self.choice(variants);
//          match variant {
//            Some((variant_name, variant_tys)) => {
//              let patterns = Array::new();
//              for variant_ty in variant_tys {
//                patterns.push(self.gen_pattern(variant_ty));
//              }
//              let prefix = if self.pos_int(2) > 0 { Some(name) } else { None };
//              Pattern::EnumPattern(prefix, variant_name, patterns)
//            }
//            None => Pattern::WildCard;
//          }
//        }
//        1 => Pattern::Ident(self.new_ident());
//        _ => Pattern::WildCard;
//      }
//    }
//    _ => {
//      match self.pos_int(2) {
//        0 => Pattern::Ident(self.new_ident());
//        _ => Pattern::WildCard;
//      }
//    }
//  }
//}
//
//fn RandAstGenerator::gen_match_expr(self: Self, ty: Type) -> MatchExpr {
//  let expr = self.gen_expr(self.gen_type(), 0);
//  let arms = Array::new();
//
//  let arm_count = self.pos_int(5) + 1; // at least 1 arm
//  for i in 0..<arm_count {
//    let pattern = if i == arm_count - 1 {
//      // Last arm should be a wildcard as catch-all
//      Pattern::WildCard
//    } else {
//      self.gen_pattern(ty)
//    };
//    let arm_expr = self.gen_expr(ty, 0);
//    arms.push((pattern, arm_expr));
//  }
//
//  MatchExpr::{
//    nested_level: 0,
//    expr,
//    arms,
//  }
//}
//
////pub(all) enum Stmt {
////  LetTupe(Array[Binding], Type?, Expr)
////  LetMut(Ident, Type?, Expr)
////  Let(Ident, Type?, Expr)
////  LocalFuncDef(Array[(Ident, Type?)], Type?, BlockExpr)
////  Assign(LeftValue, Expr)
////  While(Expr, BlockExpr)
////  Return(Expr)
////  ExprStmt(Expr)
////};
//fn RandAstGenerator::gen_stmt(self: Self) -> Stmt {
//  let n = self.pos_int(10)
//  match n {
//    0..<2 => {
//      let bindings = Array::new();
//      let mut bindings_len = 0 ;
//      while bindings_len <= 0 {
//        bindings_len = self.pos_int(self.let_tuple_max_bindings);
//      }
//      for i in 0..<bindings_len {
//        let bind = self.new_binding();
//        bindings.push(bind);
//      }
//      let ty = self.gen_tuple_type(size=Some(bindings_len));
//      let expr = self.gen_expr(ty, 0);
//      let ty = if self.pos_int(2) > 0 {
//        Some(ty)
//      } else {
//        None
//      }
//      Stmt::LetTuple(bindings, ty, expr)
//    }
//    2..<4 => {
//      let ident = self.new_ident();
//      let ty = self.gen_type();
//      let expr = self.gen_expr(ty, 0);
//      let ty = if self.pos_int(2) > 0 {
//        Some(ty)
//      } else {
//        None
//      }
//      Stmt::LetMut(ident, ty, expr)
//    }
//    4..<6 => {
//      let ident = self.new_ident();
//      let ty = self.gen_type();
//      let expr = self.gen_expr(ty, 0);
//      let ty = if self.pos_int(2) > 0 {
//        Some(ty)
//      } else {
//        None
//      }
//      Stmt::Let(ident, ty, expr)
//    }
//    //7..<8 => {
//    //  let left_value = self.gen_left_value();
//    //  let expr = self.gen_expr(left_value.ty(), 0);
//    //  Stmt::Assign(left_value, expr);
//    //}
//    _ => {
//      let cond_expr = self.gen_expr(Bool, 0);
//      let block_expr = self.gen_block_expr(Unit);
//      Stmt::While(cond_expr, block_expr)
//    }
//  }
//}
//
//fn RandAstGenerator::gen_local_func_def(self: Self) -> Stmt {
//  let params = Array::new();
//  let param_count = self.pos_int(5); // 0-4 parameters
//
//  for i in 0..<param_count {
//    let param_name = self.new_ident();
//    let param_ty = if self.pos_int(2) > 0 {
//      Some(self.gen_type())
//    } else {
//      None
//    };
//    params.push((param_name, param_ty));
//  }
//
//  let ret_ty = if self.pos_int(2) > 0 {
//    Some(self.gen_type())
//  } else {
//    None
//  };
//
//  let body = self.gen_block_expr(match ret_ty {
//    Some(ty) => ty;
//    None => Unit;
//  });
//
//  Stmt::LocalFuncDef(params, ret_ty, body)
//}
//
//pub fn RandAstGenerator::gen_top_let(self: Self) -> TopLet {
//  let name = self.new_ident();
//  let (ty, value) = match self.pos_int(9) {
//    0..<2 => (Int, Expr::from_int(self.int(300000)));
//    2..<4 => (Bool, Expr::from_bool(self.gen_bool()));
//    4..<8 => (Double, Expr::from_double(self.gen_double()));
//    _ => (Unit, Expr::from_value_expr(ValueExpr::UnitExpr));
//  }
//  let ty = if self.pos_int(2) > 0 {
//    Some(ty)
//  } else {
//    None
//  }
//  TopLet::{
//    name,
//    ty,
//    value,
//  }
//}
//
//pub fn RandAstGenerator::gen_top_func_def(self: Self) -> TopFuncDef {
//  let generic_param = if self.pos_int(3) == 0 {
//    Some(self.new_upper())
//  } else {
//    None
//  };
//  self.generic = generic_param; // Set the current generic parameter for later use
//
//  let name = self.new_ident();
//
//  let params = Array::new();
//  let param_count = self.pos_int(6); // 0-5 parameters
//
//  for i in 0..<param_count {
//    let param_name = self.new_ident();
//    let param_ty = self.gen_type();
//    params.push((param_name, param_ty));
//  }
//
//  let ret_ty = self.gen_type();
//  let fty = Type::Func(params.map((p) => p.1), ret_ty);
//  self.id_env.set(name, fty);
//  let body = self.gen_block_expr(ret_ty);
//  self.generic = None; // Clear the generic parameter after use
//
//  TopFuncDef::{
//    generic_param,
//    name,
//    params,
//    ret_ty,
//    body,
//  }
//}
//
//pub fn RandAstGenerator::gen_struct_def(self: Self) -> StructDef {
//  let generic_param = if self.pos_int(3) == 0 {
//    Some(self.new_upper())
//  } else {
//    None
//  };
//  self.generic = generic_param; // Set the current generic parameter for later use
//
//  let name = self.new_upper();
//
//  let fields = Array::new();
//  let field_count = self.pos_int(6); // 0-5 fields
//
//  for i in 0..<field_count {
//    let field_name = self.new_ident();
//    let field_ty = self.gen_type();
//    fields.push((field_name, field_ty));
//  }
//
//  let struct_def = StructDef::{
//    generic_param,
//    name,
//    fields,
//  };
//
//  // Register the struct for later use
//  self.structs.set(name, struct_def);
//  self.generic = None; // Clear the generic parameter after use
//
//  struct_def
//}
//
//pub fn RandAstGenerator::gen_enum_def(self: Self) -> EnumDef {
//  let generic_param = if self.pos_int(3) == 0 {
//    Some(self.new_upper())
//  } else {
//    None
//  };
//  self.generic = generic_param; // Set the current generic parameter for later use
//
//  let name = self.new_upper();
//
//  let variants = Array::new();
//  let variant_count = self.pos_int(5) + 1; // at least 1 variant
//
//  for i in 0..<variant_count {
//    let variant_name = self.new_upper();
//    let variant_fields = Array::new();
//    let field_count = self.pos_int(4); // 0-3 fields
//
//    for j in 0..<field_count {
//      variant_fields.push(self.gen_type());
//    }
//
//    variants.push((variant_name, variant_fields));
//  }
//
//  let enum_def = EnumDef::{
//    generic_param,
//    name,
//    variants,
//  };
//
//  // Register the enum for later use
//  self.enums.set(name, enum_def);
//  self.generic = None; // Clear the generic parameter after use
//
//  enum_def
//}
//
//pub fn RandAstGenerator::gen_top_decl(self: Self) -> TopDecl {
//  match self.pos_int(4) {
//    0 => TopDecl::TopLet(self.gen_top_let());
//    1 => TopDecl::TopFuncDef(self.gen_top_func_def());
//    2 => TopDecl::StructDef(self.gen_struct_def());
//    _ => TopDecl::EnumDef(self.gen_enum_def());
//  }
//}
//
//pub fn RandAstGenerator::gen_program(self: Self) -> Program {
//  let top_decls = Array::new();
//  let decl_count = self.pos_int(10) + 5; // at least 5 declarations
//
//  for i in 0..<decl_count {
//    let decl = self.gen_top_decl();
//    top_decls.push(decl);
//  }
//
//  Program::{
//    top_decls,
//  }
//}
