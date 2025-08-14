/// 新的随机程序生成器 - 基于csmith思路，专注于生成类型安全的MiniMoonbit程序

/// 环境管理 - 用于跟踪变量和类型绑定
struct Env[K, V] {
  map: Map[K, V]
  parent: Env[K, V]?
}

fn Env::new[K, V]() -> Env[K, V] {
  { map: Map::new(), parent: None }
}

fn Env::sub_env[K, V](self: Self[K, V]) -> Env[K, V] {
  { map: Map::new(), parent: Some(self) }
}

fn Env::get[K : Eq + Hash, V](self: Self[K, V], key: K) -> V? {
  match self.map.get(key) {
    Some(value) => Some(value)
    None => match self.parent {
      Some(parent) => parent.get(key)
      None => None
    }
  }
}

fn Env::set[K : Eq + Hash, V](self: Self[K, V], key: K, value: V) -> Unit {
  self.map.set(key, value)
}

fn Env::collect_vars_of_type[K, V : Eq](self: Self[K, V], target_type: V) -> Array[K] {
  let result = Array::new()
  self.map.each(fn(key, value) {
    if value == target_type {
      result.push(key)
    }
  })
  if self.parent is Some(parent) {
    result.append(parent.collect_vars_of_type(target_type))
  }
  result
}

/// 生成器状态和配置
pub struct RandomGenerator {
  rand: @random.Rand
  
  // 配置参数
  max_depth: Int
  max_stmt_count: Int
  max_array_size: Int
  max_tuple_size: Int
  max_func_params: Int
  
  // 环境
  mut var_env: Env[Ident, Type]     // 变量类型环境
  mut mutable_vars: Env[Ident, Type] // 可变变量环境
  type_env: Env[Upper, Type]    // 自定义类型环境
  
  // 已定义的类型和函数
  struct_defs: Map[Upper, StructDef]
  enum_defs: Map[Upper, EnumDef]
  func_defs: Map[Ident, TopFuncDef]
  
  // 名称计数器，用于确保唯一性
  mut name_counter: Int
  
  // 当前上下文
  mut current_return_type: Type
  mut current_depth: Int
}

pub fn RandomGenerator::new() -> RandomGenerator {
  {
    rand: @random.Rand::new(),
    max_depth: 5,
    max_stmt_count: 15,
    max_array_size: 5,
    max_tuple_size: 4,
    max_func_params: 4,
    var_env: Env::new(),
    mutable_vars: Env::new(),
    type_env: Env::new(),
    struct_defs: Map::new(),
    enum_defs: Map::new(),
    func_defs: Map::new(),
    name_counter: 0,

    current_return_type: Unit,
    current_depth: 0,
  }
}

/// 辅助函数
fn RandomGenerator::rand_int(self: Self, max: Int) -> Int {
  if max <= 0 { return 0 }
  self.rand.int(limit=max)
}

fn RandomGenerator::rand_bool(self: Self) -> Bool {
  self.rand_int(2) == 0
}

fn RandomGenerator::rand_double(self: Self) -> Double {
  self.rand.double()
}

fn RandomGenerator::choice[T](self: Self, choices: Array[T]) -> T? {
  if choices.is_empty() {
    None
  } else {
    Some(choices[self.rand_int(choices.length())])
  }
}

/// 生成标识符
fn RandomGenerator::gen_ident(self: Self) -> Ident {
  let names = ["x", "y", "z", "a", "b", "c", "value", "temp", "result", "data", "item", "elem", "i", "j", "k"]
  let base_name = self.choice(names).unwrap()
  self.name_counter = self.name_counter + 1
  Ident(base_name + self.name_counter.to_string())
}

fn RandomGenerator::gen_upper(self: Self) -> Upper {
  let names = ["Point", "Node", "Item", "Data", "Result", "Option", "List", "Tree", "Value", "Container"]
  let base_name = self.choice(names).unwrap()
  self.name_counter = self.name_counter + 1
  Upper(base_name + self.name_counter.to_string())
}

/// 生成基础类型
fn RandomGenerator::gen_basic_type(self: Self) -> Type {
  match self.rand_int(4) {
    0 => Unit
    1 => Bool
    2 => Int
    _ => Double
  }
}

/// 生成复合类型
fn RandomGenerator::gen_type(self: Self, allow_complex: Bool) -> Type {
  if self.current_depth >= self.max_depth || not(allow_complex) {
    return self.gen_basic_type()
  }
  
  match self.rand_int(4) {
    0 | 1 | 2 => self.gen_basic_type()
    _ => self.gen_tuple_type()  // 只生成元组类型，避免Array导致的导入问题
  }
}

fn RandomGenerator::gen_tuple_type(self: Self) -> Type {
  let size = self.rand_int(self.max_tuple_size - 1) + 2  // 至少2个元素
  let types = Array::new()
  for i = 0; i < size; i = i + 1 {
    types.push(self.gen_type(false))
  }
  Tuple(types)
}

/// 查找指定类型的变量
fn RandomGenerator::find_var_of_type(self: Self, target_type: Type) -> Ident? {
  let vars = self.var_env.collect_vars_of_type(target_type)
  self.choice(vars)
}

/// 生成简单的字面量表达式
fn RandomGenerator::gen_literal_expr(self: Self, ty: Type) -> Expr {
  match ty {
    Unit => IfLevelExpr(ApplyExpr(ValueExpr(UnitExpr)))
    Bool => IfLevelExpr(ApplyExpr(ValueExpr(BoolExpr(self.rand_bool()))))
    Int => IfLevelExpr(ApplyExpr(ValueExpr(IntExpr(self.rand_int(1000)))))
    Double => IfLevelExpr(ApplyExpr(ValueExpr(FloatExpr(self.rand_double()))))
    Tuple(elem_types) => {
      let elements = Array::new()
      for elem_type in elem_types {
        elements.push(self.gen_literal_expr(elem_type))
      }
      IfLevelExpr(ApplyExpr(ValueExpr(TupleExpr(elements))))
    }
    _ => IfLevelExpr(ApplyExpr(ValueExpr(UnitExpr)))  // 对于其他复杂类型，返回Unit
  }
}

/// 生成变量引用表达式
fn RandomGenerator::gen_var_expr(self: Self, ty: Type) -> Expr? {
  match self.find_var_of_type(ty) {
    Some(var) => Some(IfLevelExpr(ApplyExpr(ValueExpr(IdentExpr(var)))))
    None => None
  }
}

/// 生成数组表达式
fn RandomGenerator::gen_array_expr(self: Self, elem_type: Type) -> Expr {
  if self.rand_bool() {
    // Array::make(size, init_value)
    let size_expr = self.gen_literal_expr(Int)
    let init_expr = self.gen_expr(elem_type, true)
    IfLevelExpr(ApplyExpr(ValueExpr(ArrayMake(size_expr, init_expr))))
  } else {
    // [elem1, elem2, ...]
    let size = self.rand_int(self.max_array_size) + 1
    let elements = Array::new()
    for i = 0; i < size; i = i + 1 {
      elements.push(self.gen_expr(elem_type, true))
    }
    IfLevelExpr(ApplyExpr(ValueExpr(ArrayExpr(elements))))
  }
}

/// 生成元组表达式
fn RandomGenerator::gen_tuple_expr(self: Self, elem_types: Array[Type]) -> Expr {
  let elements = Array::new()
  for elem_type in elem_types {
    elements.push(self.gen_expr(elem_type, true))
  }
  IfLevelExpr(ApplyExpr(ValueExpr(TupleExpr(elements))))
}

/// 生成表达式
fn RandomGenerator::gen_expr(self: Self, ty: Type, simple: Bool) -> Expr {
  if self.current_depth >= self.max_depth || simple {
    // 在最大深度或要求简单表达式时，优先生成字面量或变量引用
    match self.rand_int(3) {
      0 => {
        match self.gen_var_expr(ty) {
          Some(expr) => expr
          None => self.gen_literal_expr(ty)
        }
      }
      _ => self.gen_literal_expr(ty)
    }
  } else {
    self.current_depth = self.current_depth + 1
    let result = match ty {
      Unit | Bool | Int | Double => {
        match self.rand_int(4) {
          0 => self.gen_literal_expr(ty)
          1 => {
            match self.gen_var_expr(ty) {
              Some(expr) => expr
              None => self.gen_literal_expr(ty)
            }
          }
          2 => {
            if ty == Int || ty == Double {
              self.gen_arithmetic_expr(ty)
            } else {
              self.gen_literal_expr(ty)
            }
          }
          _ => {
            if ty == Bool {
              self.gen_comparison_expr()
            } else {
              self.gen_literal_expr(ty)
            }
          }
        }
      }
      Array(elem_type) => {
        match self.rand_int(3) {
          0 => {
            match self.gen_var_expr(ty) {
              Some(expr) => expr
              None => self.gen_array_expr(elem_type)
            }
          }
          _ => self.gen_array_expr(elem_type)
        }
      }
      Tuple(elem_types) => {
        match self.rand_int(3) {
          0 => {
            match self.gen_var_expr(ty) {
              Some(expr) => expr
              None => self.gen_tuple_expr(elem_types)
            }
          }
          _ => self.gen_tuple_expr(elem_types)
        }
      }
      _ => self.gen_literal_expr(Unit)
    }
    self.current_depth = self.current_depth - 1
    result
  }
}

/// 生成算术表达式 (仅适用于Int和Double)
fn RandomGenerator::gen_arithmetic_expr(self: Self, ty: Type) -> Expr {
  let left = self.gen_expr(ty, true)
  let right = self.gen_expr(ty, true)
  
  match self.rand_int(5) {
    0 => AddExpr(left, right)
    1 => SubExpr(left, right)
    2 => MulExpr(left, right)
    3 => DivExpr(left, right)
    _ => ModExpr(left, right)
  }
}

/// 生成比较表达式 (返回Bool)
fn RandomGenerator::gen_comparison_expr(self: Self) -> Expr {
  let ty = if self.rand_bool() { Int } else { Double }
  let left = self.gen_expr(ty, true)
  let right = self.gen_expr(ty, true)
  
  let op = match self.rand_int(6) {
    0 => Eq
    1 => Ne
    2 => Lt
    3 => Le
    4 => Gt
    _ => Ge
  }
  
  CmpExpr(op, left, right)
}

/// 生成语句
fn RandomGenerator::gen_stmt(self: Self) -> Stmt {
  // 在深度较大时，避免生成复杂的语句（如while循环）
  if self.current_depth >= self.max_depth - 1 {
    match self.rand_int(3) {
      0 => self.gen_let_stmt()
      1 => self.gen_expr_stmt()
      _ => self.gen_let_stmt()
    }
  } else {
    match self.rand_int(5) {
      0 => self.gen_let_stmt()
      1 => self.gen_let_mut_stmt()
      2 => self.gen_assign_stmt()
      3 => self.gen_expr_stmt()
      _ => self.gen_let_stmt()
    }
  }
}

fn RandomGenerator::gen_let_stmt(self: Self) -> Stmt {
  let var_name = self.gen_ident()
  let var_type = self.gen_type(true)
  let init_expr = self.gen_expr(var_type, false)
  
  // 将变量添加到环境中
  self.var_env.set(var_name, var_type)
  
  let type_annotation = if self.rand_bool() { Some(var_type) } else { None }
  Let(var_name, type_annotation, init_expr)
}

fn RandomGenerator::gen_let_mut_stmt(self: Self) -> Stmt {
  let var_name = self.gen_ident()
  let var_type = self.gen_type(true)
  let init_expr = self.gen_expr(var_type, false)
  
  // 将变量添加到环境中（包括可变变量环境）
  self.var_env.set(var_name, var_type)
  self.mutable_vars.set(var_name, var_type)
  
  let type_annotation = if self.rand_bool() { Some(var_type) } else { None }
  LetMut(var_name, type_annotation, init_expr)
}

fn RandomGenerator::gen_assign_stmt(self: Self) -> Stmt {
  // 获取所有可变变量
  let mut_vars = Array::new()
  self.mutable_vars.map.each(fn(var_name, var_type) {
    mut_vars.push((var_name, var_type))
  })
  
  if mut_vars.is_empty() {
    // 如果没有可变变量，生成一个let语句
    return self.gen_let_stmt()
  }
  
  let (var_name, var_type) = self.choice(mut_vars).unwrap()
  let new_value = self.gen_expr(var_type, false)
  
  Assign(Ident(var_name), new_value)
}

fn RandomGenerator::gen_while_stmt(self: Self) -> Stmt {
  let cond = self.gen_expr(Bool, false)
  let body = self.gen_block_expr(Unit)
  While(cond, body)
}

fn RandomGenerator::gen_expr_stmt(self: Self) -> Stmt {
  let expr = self.gen_expr(Unit, false)
  ExprStmt(expr)
}

/// 生成代码块
fn RandomGenerator::gen_block_expr(self: Self, return_type: Type) -> BlockExpr {
  let block = BlockExpr::new()
  
  // 检查递归深度，避免无限递归
  if self.current_depth >= self.max_depth {
    // 在最大深度时，只生成简单的返回表达式
    if return_type != Unit {
      let last_expr = self.gen_literal_expr(return_type)
      block.set_last_expr(last_expr)
    }
    return block
  }
  
  // 创建子环境
  let old_var_env = self.var_env
  let old_mut_env = self.mutable_vars
  let old_depth = self.current_depth
  self.var_env = old_var_env.sub_env()
  self.mutable_vars = old_mut_env.sub_env()
  self.current_depth = self.current_depth + 1
  
  // 生成较少的语句
  let stmt_count = self.rand_int(5) + 1  // 减少语句数量，避免过深的嵌套
  for i = 0; i < stmt_count; i = i + 1 {
    let stmt = self.gen_stmt()
    block.push(stmt)
  }
  
  // 生成返回表达式（如果需要）
  if return_type != Unit {
    let last_expr = self.gen_literal_expr(return_type)  // 强制生成匹配类型的字面量表达式
    block.set_last_expr(last_expr)
  }
  
  // 恢复环境
  self.var_env = old_var_env
  self.mutable_vars = old_mut_env
  self.current_depth = old_depth
  
  block
}

/// 生成顶层定义
pub fn RandomGenerator::gen_top_let(self: Self) -> TopLet {
  let name = self.gen_ident()
  let ty = self.gen_type(true)
  let value = self.gen_expr(ty, false)
  
  // 总是生成类型注解以确保类型安全
  let type_annotation = Some(ty)
  { name, ty: type_annotation, value }
}

pub fn RandomGenerator::gen_struct_def(self: Self) -> StructDef {
  let name = self.gen_upper()
  let field_count = self.rand_int(5) + 1  // 1-5个字段
  let fields = Array::new()
  
  for i = 0; i < field_count; i = i + 1 {
    let field_name = self.gen_ident()
    let field_type = self.gen_type(false)  // 字段类型保持简单
    fields.push((field_name, field_type))
  }
  
  let struct_def = { generic_param: None, name, fields }
  
  // 注册结构体定义
  self.struct_defs.set(name, struct_def)
  
  struct_def
}

pub fn RandomGenerator::gen_enum_def(self: Self) -> EnumDef {
  let name = self.gen_upper()
  let variant_count = self.rand_int(4) + 1  // 1-4个变体
  let variants = Array::new()
  
  for i = 0; i < variant_count; i = i + 1 {
    let variant_name = self.gen_upper()
    let field_count = self.rand_int(3)  // 0-2个字段
    let variant_fields = Array::new()
    
    for j = 0; j < field_count; j = j + 1 {
      variant_fields.push(self.gen_type(false))
    }
    
    variants.push((variant_name, variant_fields))
  }
  
  let enum_def = { generic_param: None, name, variants }
  
  // 注册枚举定义
  self.enum_defs.set(name, enum_def)
  
  enum_def
}

pub fn RandomGenerator::gen_top_func_def(self: Self) -> TopFuncDef {
  let name = self.gen_ident()
  let param_count = self.rand_int(self.max_func_params)
  let params = Array::new()
  
  // 创建新的环境作用域
  let old_var_env = self.var_env
  self.var_env = old_var_env.sub_env()
  
  // 生成参数
  for i = 0; i < param_count; i = i + 1 {
    let param_name = self.gen_ident()
    let param_type = self.gen_type(false)
    params.push((param_name, param_type))
    
    // 将参数添加到环境中
    self.var_env.set(param_name, param_type)
  }
  
  // 生成返回类型和函数体
  let ret_ty = self.gen_type(true)
  let old_return_type = self.current_return_type
  self.current_return_type = ret_ty
  
  let body = self.gen_block_expr(ret_ty)
  
  // 恢复环境和返回类型
  self.current_return_type = old_return_type
  self.var_env = old_var_env
  
  let func_def = { generic_param: None, name, params, ret_ty, body }
  
  // 注册函数定义
  self.func_defs.set(name, func_def)
  
  func_def
}

pub fn RandomGenerator::gen_top_decl(self: Self) -> TopDecl {
  match self.rand_int(4) {
    0 => TopDecl::TopLet(self.gen_top_let())
    1 => TopDecl::StructDef(self.gen_struct_def())
    2 => TopDecl::EnumDef(self.gen_enum_def())
    _ => TopDecl::TopFuncDef(self.gen_top_func_def())
  }
}

pub fn RandomGenerator::gen_program(self: Self) -> Program {
  let top_decls = Array::new()
  
  // 首先生成一些基础的类型定义
  for i = 0; i < 2; i = i + 1 {
    top_decls.push(TopDecl::StructDef(self.gen_struct_def()))
  }
  
  for i = 0; i < 2; i = i + 1 {
    top_decls.push(TopDecl::EnumDef(self.gen_enum_def()))
  }
  
  // 然后生成函数和顶层变量
  let decl_count = self.rand_int(6) + 4  // 4-9个额外声明
  for i = 0; i < decl_count; i = i + 1 {
    match self.rand_int(3) {
      0 => top_decls.push(TopDecl::TopLet(self.gen_top_let()))
      1 => top_decls.push(TopDecl::TopFuncDef(self.gen_top_func_def()))
      _ => top_decls.push(TopDecl::StructDef(self.gen_struct_def()))
    }
  }
  
  { top_decls, }
}
