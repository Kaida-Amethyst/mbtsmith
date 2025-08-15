/// 增强的随机程序生成器 - 基于csmith思路，专注于生成复杂的类型安全的MiniMoonbit程序

///| 环境管理 - 用于跟踪变量和类型绑定
struct Env[K, V] {
  map : Map[K, V]
  parent : Env[K, V]?
}

///|
fn[K, V] Env::new() -> Env[K, V] {
  { map: Map::new(), parent: None }
}

///|
fn[K, V] Env::sub_env(self : Self[K, V]) -> Env[K, V] {
  { map: Map::new(), parent: Some(self) }
}

///|
fn[K : Eq + Hash, V] Env::get(self : Self[K, V], key : K) -> V? {
  match self.map.get(key) {
    Some(value) => Some(value)
    None =>
      match self.parent {
        Some(parent) => parent.get(key)
        None => None
      }
  }
}

///|
fn[K : Eq + Hash, V] Env::set(self : Self[K, V], key : K, value : V) -> Unit {
  self.map.set(key, value)
}

///|
fn[K, V : Eq] Env::collect_vars_of_type(
  self : Self[K, V],
  target_type : V,
) -> Array[K] {
  let result = Array::new()
  self.map.each(fn(key, value) { if value == target_type { result.push(key) } })
  if self.parent is Some(parent) {
    result.append(parent.collect_vars_of_type(target_type))
  }
  result
}

///| 生成器状态和配置
pub struct RandomGenerator {
  rand : @random.Rand

  // 增强的配置参数，支持生成更复杂的程序
  max_depth : Int
  max_stmt_count : Int
  max_array_size : Int
  max_tuple_size : Int
  max_func_params : Int
  max_nested_level : Int
  total_lines_target : Int

  // 环境
  mut var_env : Env[Ident, Type] // 变量类型环境
  mut mutable_vars : Env[Ident, Type] // 可变变量环境
  mut unused_mutable_vars : Map[Ident, Type] // 未使用的可变变量（需要强制赋值）
  type_env : Env[Upper, Type] // 自定义类型环境

  // 跟踪所有定义的变量和它们的使用情况
  mut all_defined_vars : Map[Ident, Type] // 所有定义的变量
  mut used_vars : Map[Ident, Bool] // 已使用的变量

  // 已定义的类型和函数
  struct_defs : Map[Upper, StructDef]
  enum_defs : Map[Upper, EnumDef]
  func_defs : Map[Ident, TopFuncDef]

  // 名称计数器，用于确保唯一性
  mut name_counter : Int

  // 当前上下文
  mut current_return_type : Type
  mut current_depth : Int
  mut current_nested_level : Int

  // 行数统计
  mut current_lines : Int
}

///|
pub fn RandomGenerator::new(seed~ : Int = 0) -> RandomGenerator {
  let g = RandomGenerator::{
    rand: @random.Rand::new(),
    max_depth: 4, // 减少最大深度避免栈溢出
    max_stmt_count: 15, // 适中的语句数量
    max_array_size: 5, // 适中的数组大小
    max_tuple_size: 4, // 适中的元组大小
    max_func_params: 4, // 适中的函数参数数量
    max_nested_level: 3, // 减少嵌套层级限制
    total_lines_target: 2000, // 目标行数
    var_env: Env::new(),
    mutable_vars: Env::new(),
    unused_mutable_vars: Map::new(),
    type_env: Env::new(),
    all_defined_vars: Map::new(),
    used_vars: Map::new(),
    struct_defs: Map::new(),
    enum_defs: Map::new(),
    func_defs: Map::new(),
    name_counter: 0,
    current_return_type: Unit,
    current_depth: 0,
    current_nested_level: 0,
    current_lines: 0,
  }
  for i in 0..<seed {
    let _ = g.rand_int(100)

  }
  g
}

///| 辅助函数
fn RandomGenerator::rand_int(self : Self, max : Int) -> Int {
  if max <= 0 {
    return 0
  }
  self.rand.int(limit=max)
}

///|
fn RandomGenerator::rand_bool(self : Self) -> Bool {
  self.rand_int(2) == 0
}

///|
fn RandomGenerator::rand_double(self : Self) -> Double {
  self.rand.double() * 1000.0 // 扩大范围，生成更多样的浮点数
}

///|
fn[T] RandomGenerator::choice(self : Self, choices : Array[T]) -> T? {
  if choices.is_empty() {
    None
  } else {
    Some(choices[self.rand_int(choices.length())])
  }
}

///|
fn[T] RandomGenerator::weighted_choice(
  self : Self,
  choices : Array[(T, Int)],
) -> T? {
  if choices.is_empty() {
    return None
  }
  let total_weight = choices.fold(init=0, fn(acc, choice) { acc + choice.1 })
  if total_weight <= 0 {
    return self.choice(choices.map(fn(choice) { choice.0 }))
  }
  let mut current_weight = 0
  let target_weight = self.rand_int(total_weight)
  for choice in choices {
    current_weight = current_weight + choice.1
    if current_weight > target_weight {
      return Some(choice.0)
    }
  }

  // 备用选择
  self.choice(choices.map(fn(choice) { choice.0 }))
}

///| 生成标识符
fn RandomGenerator::gen_ident(self : Self) -> Ident {
  let names = [
    "x", "y", "z", "a", "b", "c", "value", "temp", "result", "data", "item", "elem",
    "i", "j", "k", "count", "sum", "index", "flag", "status", "info", "node", "next",
    "prev", "head", "tail", "left", "right", "top", "bottom", "min", "max", "first",
    "last", "current", "target",
  ]
  let base_name = self.choice(names).unwrap()
  self.name_counter = self.name_counter + 1
  Ident(base_name + self.name_counter.to_string())
}

///|
fn RandomGenerator::gen_upper(self : Self) -> Upper {
  let names = [
    "Point", "Node", "Item", "Data", "Result", "Option", "List", "Tree", "Value",
    "Container", "Record", "Entry", "Element", "Structure", "Entity", "Object", "Instance",
    "State", "Config", "Context", "Buffer", "Cache", "Queue", "Stack", "Matrix",
    "Vector", "Complex", "Wrapper",
  ]
  let base_name = self.choice(names).unwrap()
  self.name_counter = self.name_counter + 1
  Upper(base_name + self.name_counter.to_string())
}

///| 生成基础类型
fn RandomGenerator::gen_basic_type(self : Self) -> Type {
  match self.rand_int(4) {
    0 => Unit
    1 => Bool
    2 => Int
    _ => Double
  }
}

///| 增强的类型生成器，支持更复杂的嵌套类型
fn RandomGenerator::gen_type(self : Self, allow_complex : Bool) -> Type {
  if self.current_nested_level >= self.max_nested_level ||
    !allow_complex ||
    self.current_depth >= self.max_depth - 1 {
    return self.gen_basic_type()
  }

  // 使用加权选择，基础类型权重更高，保证程序可编译
  let choices = [
    (0, 80), // 基础类型 - 大幅提高权重
    (1, 10), // 数组类型 - 降低权重  
    (2, 8), // 元组类型 - 降低权重
    (3, 1), // 自定义结构体 - 极大降低权重
    (4, 1),
  ] // 自定义枚举 - 极大降低权重
  match self.weighted_choice(choices) {
    Some(0) => self.gen_basic_type()
    Some(1) => self.gen_array_type()
    Some(2) => self.gen_tuple_type()
    Some(3) => self.gen_custom_struct_type()
    Some(4) => self.gen_custom_enum_type()
    _ => self.gen_basic_type()
  }
}

///|
fn RandomGenerator::gen_array_type(self : Self) -> Type {
  self.current_nested_level = self.current_nested_level + 1
  let element_type = self.gen_type(true)
  self.current_nested_level = self.current_nested_level - 1
  Array(element_type)
}

///|
fn RandomGenerator::gen_tuple_type(self : Self) -> Type {
  let size = self.rand_int(self.max_tuple_size - 1) + 2 // 至少2个元素
  let types = Array::new()
  self.current_nested_level = self.current_nested_level + 1
  for i = 0; i < size; i = i + 1 {
    types.push(
      self.gen_type(self.current_nested_level < self.max_nested_level - 1),
    )
  }
  self.current_nested_level = self.current_nested_level - 1
  Tuple(types)
}

///|
fn RandomGenerator::gen_custom_struct_type(self : Self) -> Type {
  // 从已定义的结构体中随机选择一个
  let available_structs = Array::new()
  self.struct_defs.each(fn(name, _) { available_structs.push(name) })
  match self.choice(available_structs) {
    Some(struct_name) =>
      match self.struct_defs.get(struct_name) {
        Some(struct_def) => struct_def.type_of()
        None => self.gen_basic_type() // 备用方案
      }
    None => self.gen_basic_type() // 如果没有可用的结构体，返回基础类型
  }
}

///|
fn RandomGenerator::gen_custom_enum_type(self : Self) -> Type {
  // 从已定义的枚举中随机选择一个
  let available_enums = Array::new()
  self.enum_defs.each(fn(name, _) { available_enums.push(name) })
  match self.choice(available_enums) {
    Some(enum_name) =>
      match self.enum_defs.get(enum_name) {
        Some(enum_def) => enum_def.type_of()
        None => self.gen_basic_type() // 备用方案
      }
    None => self.gen_basic_type() // 如果没有可用的枚举，返回基础类型
  }
}

///| 查找指定类型的所有变量
fn RandomGenerator::find_all_vars_of_type(
  self : Self,
  target_type : Type,
) -> Array[Ident] {
  self.var_env.collect_vars_of_type(target_type)
}

///| 标记可变变量已被使用（从未使用列表中移除）
fn RandomGenerator::mark_mutable_var_used(
  self : Self,
  var_name : Ident,
) -> Unit {
  // 创建新的Map来替代移除操作
  let new_map = Map::new()
  self.unused_mutable_vars.each(fn(name, ty) {
    if name != var_name {
      new_map.set(name, ty)
    }
  })
  self.unused_mutable_vars = new_map
}

///| 记录变量定义
fn RandomGenerator::record_var_definition(
  self : Self,
  var_name : Ident,
  var_type : Type,
) -> Unit {
  self.all_defined_vars = {
    let new_map = Map::new()
    self.all_defined_vars.each(fn(k, v) { new_map.set(k, v) })
    new_map.set(var_name, var_type)
    new_map
  }
  self.used_vars = {
    let new_map = Map::new()
    self.used_vars.each(fn(k, v) { new_map.set(k, v) })
    new_map.set(var_name, false) // 初始标记为未使用
    new_map
  }
}

///| 标记变量已使用
fn RandomGenerator::mark_var_used(self : Self, var_name : Ident) -> Unit {
  self.used_vars = {
    let new_map = Map::new()
    self.used_vars.each(fn(k, v) { new_map.set(k, v) })
    new_map.set(var_name, true)
    new_map
  }
}

///| 安全地生成对未使用可变变量的强制赋值语句
fn RandomGenerator::gen_safe_force_assign_unused_mutables(
  self : Self,
) -> Array[Stmt] {
  let assignments = Array::new()
  self.unused_mutable_vars.each(fn(var_name, var_type) {
    // 只有当变量在当前环境中确实可见时才生成赋值
    match self.var_env.get(var_name) {
      Some(_) => {
        let new_value = self.gen_expr(var_type, true)
        let assign_stmt = Assign(LeftValue::Ident(var_name), new_value)
        assignments.push(assign_stmt)
      }
      None => () // 变量不可见，跳过
    }
  })

  // 清空未使用列表
  self.unused_mutable_vars = Map::new()
  assignments
}

///| 生成复杂的字面量表达式
fn RandomGenerator::gen_literal_expr(self : Self, ty : Type) -> Expr {
  match ty {
    Unit => IfLevelExpr(ApplyExpr(ValueExpr(UnitExpr)))
    Bool => IfLevelExpr(ApplyExpr(ValueExpr(BoolExpr(self.rand_bool()))))
    Int => {
      // 生成更多样的整数
      let value = match self.rand_int(4) {
        0 => self.rand_int(10) // 小整数 0-9
        1 => self.rand_int(1000) // 中等整数 0-999
        2 => self.rand_int(100000) // 大整数 0-99999
        _ => self.rand_int(2) * 2 - 1 // -1 或 1
      }
      IfLevelExpr(ApplyExpr(ValueExpr(IntExpr(value))))
    }
    Double => {
      let value = self.rand_double()
      IfLevelExpr(ApplyExpr(ValueExpr(FloatExpr(value))))
    }
    Array(elem_type) => self.gen_array_expr(elem_type)
    Tuple(elem_types) => self.gen_tuple_expr(elem_types)
    Struct(name, fields) => self.gen_struct_construct_expr(name, fields)
    Enum(name, variants) => self.gen_enum_construct_expr(name, variants)
    _ => IfLevelExpr(ApplyExpr(ValueExpr(UnitExpr))) // 对于其他类型，返回Unit
  }
}

///| 生成结构体构造表达式
fn RandomGenerator::gen_struct_construct_expr(
  self : Self,
  name : Upper,
  fields : Array[(Ident, Type)],
) -> Expr {
  let field_assignments = Array::new()
  for field in fields {
    let (field_name, field_type) = field
    let field_value = self.gen_expr(field_type, true) // 字段值使用简单表达式
    field_assignments.push((field_name, field_value))
  }
  IfLevelExpr(ApplyExpr(ValueExpr(StructConstruct(name, field_assignments))))
}

///| 生成枚举构造表达式
fn RandomGenerator::gen_enum_construct_expr(
  self : Self,
  name : Upper,
  variants : Array[(Upper, Array[Type])],
) -> Expr {
  match self.choice(variants) {
    Some((variant_name, variant_types)) => {
      let field_values = Array::new()
      for variant_type in variant_types {
        field_values.push(self.gen_expr(variant_type, true))
      }
      IfLevelExpr(
        ApplyExpr(
          ValueExpr(EnumConstruct(Some(name), variant_name, field_values)),
        ),
      )
    }
    None => IfLevelExpr(ApplyExpr(ValueExpr(UnitExpr))) // 如果没有变体，返回Unit
  }
}

///| 生成变量引用表达式 - 确保引用的变量在当前环境中确实存在
fn RandomGenerator::gen_var_expr(self : Self, ty : Type) -> Expr? {
  // 首先查找当前环境中指定类型的变量
  let available_vars = self.var_env.collect_vars_of_type(ty)
  if available_vars.is_empty() {
    return None
  }

  // 从可用变量中随机选择一个
  match self.choice(available_vars) {
    Some(v) =>
      // 再次验证变量确实存在
      match self.var_env.get(v) {
        Some(_) => {
          // 标记变量已使用
          self.mark_var_used(v)

          // 如果是可变变量，标记为已使用
          match self.unused_mutable_vars.get(v) {
            Some(_) => self.mark_mutable_var_used(v)
            None => ()
          }
          Some(IfLevelExpr(ApplyExpr(ValueExpr(IdentExpr(v)))))
        }
        None => None
      }
    None => None
  }
}

///| 生成复杂的数组表达式
fn RandomGenerator::gen_array_expr(self : Self, elem_type : Type) -> Expr {
  match self.rand_int(2) {
    0 => {
      // Array::make(size, init_value)
      let size_val = self.rand_int(self.max_array_size) + 1
      let size_expr = IfLevelExpr(ApplyExpr(ValueExpr(IntExpr(size_val))))
      let init_expr = self.gen_expr(elem_type, true)
      IfLevelExpr(ApplyExpr(ValueExpr(ArrayMake(size_expr, init_expr))))
    }
    _ => {
      // [elem1, elem2, ...] - 确保至少有一个元素
      let size = self.rand_int(self.max_array_size) + 1 // 至少1个元素
      let elements = Array::new()
      for i = 0; i < size; i = i + 1 {
        elements.push(self.gen_expr(elem_type, true))
      }
      IfLevelExpr(ApplyExpr(ValueExpr(ArrayExpr(elements))))
    }
  }
}

///| 生成复杂的元组表达式
fn RandomGenerator::gen_tuple_expr(
  self : Self,
  elem_types : Array[Type],
) -> Expr {
  let elements = Array::new()
  for elem_type in elem_types {
    elements.push(self.gen_expr(elem_type, true))
  }
  IfLevelExpr(ApplyExpr(ValueExpr(TupleExpr(elements))))
}

///| 生成函数调用表达式 - 确保只调用确实存在且可访问的函数
fn RandomGenerator::gen_function_call_expr(
  self : Self,
  expected_return_type : Type,
) -> Expr? {
  // 查找返回类型匹配的函数（包括顶层函数和当前作用域中的局部函数）
  let available_funcs = Array::new()

  // 查找当前环境中的函数类型变量（局部函数） - 只检查当前可访问的
  self.var_env.map.each(fn(var_name, var_type) {
    match var_type {
      Func(param_types, return_type) =>
        if return_type == expected_return_type {
          // 再次验证函数确实存在于当前环境中
          match self.var_env.get(var_name) {
            Some(_) => available_funcs.push((var_name, param_types))
            None => ()
          }
        }
      _ => ()
    }
  })

  // 只在有很少的可用函数时才考虑顶层函数，避免复杂性
  if available_funcs.length() < 2 {
    self.func_defs.each(fn(func_name, func_def) {
      if func_def.ret_ty == expected_return_type &&
        func_def.params.length() <= 2 {
        available_funcs.push((func_name, func_def.params.map(fn(p) { p.1 })))
      }
    })
  }
  match self.choice(available_funcs) {
    Some((func_name, param_types)) => {
      let args = Array::new()
      for param_type in param_types {
        args.push(self.gen_expr(param_type, true)) // 使用简单表达式
      }
      let func_apply = ApplyExpr::ValueExpr(ValueExpr::IdentExpr(func_name))
      Some(IfLevelExpr(ApplyExpr(Call(func_apply, args))))
    }
    None => None
  }
}

///| 生成数组访问表达式
fn RandomGenerator::gen_array_access_expr(
  self : Self,
  elem_type : Type,
) -> Expr? {
  // 查找数组类型的变量
  let array_vars = self.find_all_vars_of_type(Array(elem_type))
  match self.choice(array_vars) {
    Some(array_var) => {
      let index_expr = self.gen_expr(Int, true)
      let array_apply = ApplyExpr::ValueExpr(ValueExpr::IdentExpr(array_var))
      Some(IfLevelExpr(ApplyExpr(ArrAcc(array_apply, index_expr))))
    }
    None => None
  }
}

///| 生成点访问表达式（结构体字段访问）
fn RandomGenerator::gen_dot_access_expr(
  self : Self,
  expected_type : Type,
) -> Expr? {
  // 查找具有匹配字段类型的结构体变量
  let candidates = Array::new()
  self.var_env.map.each(fn(var_name, var_type) {
    match var_type {
      Struct(_, fields) =>
        for field in fields {
          let (field_name, field_type) = field
          if field_type == expected_type {
            candidates.push((var_name, field_name))
          }
        }
      _ => ()
    }
  })
  match self.choice(candidates) {
    Some((var_name, field_name)) => {
      let struct_apply = ApplyExpr::ValueExpr(ValueExpr::IdentExpr(var_name))
      Some(IfLevelExpr(ApplyExpr(DotAcc(struct_apply, field_name))))
    }
    None => None
  }
}

///| 增强的表达式生成器，支持更复杂的表达式结构
pub fn RandomGenerator::gen_expr(self : Self, ty : Type, simple : Bool) -> Expr {
  if self.current_depth >= self.max_depth || simple {
    // 在最大深度或要求简单表达式时，优先生成字面量或变量引用
    let choices = [
      (0, 60), // 字面量 - 大幅提高权重
      (1, 35), // 变量引用 - 提高权重
      (2, 3), // 函数调用 - 大幅降低权重
      (3, 2),
    ] // 字段访问 - 大幅降低权重
    match self.weighted_choice(choices) {
      Some(0) => self.gen_literal_expr(ty)
      Some(1) =>
        match self.gen_var_expr(ty) {
          Some(expr) => expr
          None => self.gen_literal_expr(ty)
        }
      Some(2) =>
        match self.gen_function_call_expr(ty) {
          Some(expr) => expr
          None => self.gen_literal_expr(ty)
        }
      Some(3) =>
        match self.gen_dot_access_expr(ty) {
          Some(expr) => expr
          None => self.gen_literal_expr(ty)
        }
      _ => self.gen_literal_expr(ty)
    }
  } else {
    self.current_depth = self.current_depth + 1
    let result = match ty {
      Unit => self.gen_complex_unit_expr()
      Bool => self.gen_complex_bool_expr()
      Int => self.gen_complex_int_expr()
      Double => self.gen_complex_double_expr()
      Array(elem_type) => self.gen_complex_array_expr(elem_type)
      Tuple(elem_types) => self.gen_complex_tuple_expr(elem_types)
      Struct(name, fields) => self.gen_complex_struct_expr(name, fields)
      Enum(name, variants) => self.gen_complex_enum_expr(name, variants)
      _ => self.gen_literal_expr(ty)
    }
    self.current_depth = self.current_depth - 1
    result
  }
}

///| 生成复杂的Unit表达式
fn RandomGenerator::gen_complex_unit_expr(self : Self) -> Expr {
  let choices = [
    (0, 40), // 字面量Unit
    (1, 20), // 变量引用
    (2, 15), // 函数调用
    (3, 15), // if表达式
    (4, 10),
  ] // 表达式语句
  match self.weighted_choice(choices) {
    Some(0) => self.gen_literal_expr(Unit)
    Some(1) =>
      match self.gen_var_expr(Unit) {
        Some(expr) => expr
        None => self.gen_literal_expr(Unit)
      }
    Some(2) =>
      match self.gen_function_call_expr(Unit) {
        Some(expr) => expr
        None => self.gen_literal_expr(Unit)
      }
    Some(3) =>
      match self.gen_if_expr(Unit) {
        Some(expr) => expr
        None => self.gen_literal_expr(Unit)
      }
    Some(4) => {
      // 生成一个包含Unit表达式的组合表达式
      let inner_expr = self.gen_expr(Unit, true)
      IfLevelExpr(ApplyExpr(ValueExpr(GroupExpr(inner_expr))))
    }
    _ => self.gen_literal_expr(Unit)
  }
}

///| 生成复杂的Bool表达式
fn RandomGenerator::gen_complex_bool_expr(self : Self) -> Expr {
  let choices = [
    (0, 20), // 字面量Bool
    (1, 18), // 变量引用
    (2, 18), // 比较表达式
    (3, 15), // 逻辑表达式
    (4, 12), // if表达式
    (5, 10), // 函数调用
    (6, 7),
  ] // 取反表达式
  match self.weighted_choice(choices) {
    Some(0) => self.gen_literal_expr(Bool)
    Some(1) =>
      match self.gen_var_expr(Bool) {
        Some(expr) => expr
        None => self.gen_literal_expr(Bool)
      }
    Some(2) => self.gen_comparison_expr()
    Some(3) => self.gen_logical_expr()
    Some(4) =>
      match self.gen_if_expr(Bool) {
        Some(expr) => expr
        None => self.gen_literal_expr(Bool)
      }
    Some(5) =>
      match self.gen_function_call_expr(Bool) {
        Some(expr) => expr
        None => self.gen_literal_expr(Bool)
      }
    Some(6) => {
      let inner_expr = self.gen_expr(Bool, true)
      IfLevelExpr(ApplyExpr(ValueExpr(NotExpr(inner_expr))))
    }
    _ => self.gen_literal_expr(Bool)
  }
}

///| 生成复杂的Int表达式
fn RandomGenerator::gen_complex_int_expr(self : Self) -> Expr {
  let choices = [
    (0, 35), // 字面量Int
    (1, 30), // 变量引用
    (2, 15), // 算术表达式
    (3, 8), // if表达式
    (4, 5), // 函数调用
    (5, 4), // match表达式
    (6, 2), // 数组访问
    (7, 1), // 字段访问
  ]
  match self.weighted_choice(choices) {
    Some(0) => self.gen_literal_expr(Int)
    Some(1) =>
      match self.gen_var_expr(Int) {
        Some(expr) => expr
        None => self.gen_literal_expr(Int)
      }
    Some(2) => self.gen_arithmetic_expr(Int)
    Some(3) =>
      match self.gen_if_expr(Int) {
        Some(expr) => expr
        None => self.gen_literal_expr(Int)
      }
    Some(4) =>
      match self.gen_function_call_expr(Int) {
        Some(expr) => expr
        None => self.gen_literal_expr(Int)
      }
    Some(5) =>
      match self.gen_match_expr(Int) {
        Some(expr) => expr
        None => self.gen_literal_expr(Int)
      }
    Some(6) =>
      match self.gen_array_access_expr(Int) {
        Some(expr) => expr
        None => self.gen_literal_expr(Int)
      }
    Some(7) =>
      match self.gen_dot_access_expr(Int) {
        Some(expr) => expr
        None => self.gen_literal_expr(Int)
      }
    _ => self.gen_literal_expr(Int)
  }
}

///| 生成复杂的Double表达式
fn RandomGenerator::gen_complex_double_expr(self : Self) -> Expr {
  let choices = [
    (0, 25), // 字面量Double
    (1, 20), // 变量引用
    (2, 20), // 算术表达式
    (3, 12), // if表达式
    (4, 10), // 函数调用
    (5, 8), // match表达式
    (6, 3), // 数组访问
    (7, 2), // 字段访问
  ]
  match self.weighted_choice(choices) {
    Some(0) => self.gen_literal_expr(Double)
    Some(1) =>
      match self.gen_var_expr(Double) {
        Some(expr) => expr
        None => self.gen_literal_expr(Double)
      }
    Some(2) => self.gen_arithmetic_expr(Double)
    Some(3) =>
      match self.gen_if_expr(Double) {
        Some(expr) => expr
        None => self.gen_literal_expr(Double)
      }
    Some(4) =>
      match self.gen_function_call_expr(Double) {
        Some(expr) => expr
        None => self.gen_literal_expr(Double)
      }
    Some(5) =>
      match self.gen_match_expr(Double) {
        Some(expr) => expr
        None => self.gen_literal_expr(Double)
      }
    Some(6) =>
      match self.gen_array_access_expr(Double) {
        Some(expr) => expr
        None => self.gen_literal_expr(Double)
      }
    Some(7) =>
      match self.gen_dot_access_expr(Double) {
        Some(expr) => expr
        None => self.gen_literal_expr(Double)
      }
    _ => self.gen_literal_expr(Double)
  }
}

///| 生成复杂的Array表达式
fn RandomGenerator::gen_complex_array_expr(
  self : Self,
  elem_type : Type,
) -> Expr {
  let choices = [
    (0, 30), // 数组字面量
    (1, 25), // 变量引用
    (2, 20), // Array::make
    (3, 15), // 函数调用
    (4, 10),
  ] // 字段访问
  match self.weighted_choice(choices) {
    Some(0) => self.gen_array_expr(elem_type)
    Some(1) =>
      match self.gen_var_expr(Array(elem_type)) {
        Some(expr) => expr
        None => self.gen_array_expr(elem_type)
      }
    Some(2) => {
      // 确保Array::make的第一个参数至少为1
      let size_val = self.rand_int(self.max_array_size) + 1
      let size_expr = IfLevelExpr(ApplyExpr(ValueExpr(IntExpr(size_val))))
      let init_expr = self.gen_expr(elem_type, false)
      IfLevelExpr(ApplyExpr(ValueExpr(ArrayMake(size_expr, init_expr))))
    }
    Some(3) =>
      match self.gen_function_call_expr(Array(elem_type)) {
        Some(expr) => expr
        None => self.gen_array_expr(elem_type)
      }
    Some(4) =>
      match self.gen_dot_access_expr(Array(elem_type)) {
        Some(expr) => expr
        None => self.gen_array_expr(elem_type)
      }
    _ => self.gen_array_expr(elem_type)
  }
}

///| 生成复杂的Tuple表达式
fn RandomGenerator::gen_complex_tuple_expr(
  self : Self,
  elem_types : Array[Type],
) -> Expr {
  let choices = [
    (0, 40), // 元组字面量
    (1, 25), // 变量引用
    (2, 20), // 函数调用
    (3, 15),
  ] // 字段访问
  match self.weighted_choice(choices) {
    Some(0) => self.gen_tuple_expr(elem_types)
    Some(1) =>
      match self.gen_var_expr(Tuple(elem_types)) {
        Some(expr) => expr
        None => self.gen_tuple_expr(elem_types)
      }
    Some(2) =>
      match self.gen_function_call_expr(Tuple(elem_types)) {
        Some(expr) => expr
        None => self.gen_tuple_expr(elem_types)
      }
    Some(3) =>
      match self.gen_dot_access_expr(Tuple(elem_types)) {
        Some(expr) => expr
        None => self.gen_tuple_expr(elem_types)
      }
    _ => self.gen_tuple_expr(elem_types)
  }
}

///| 生成复杂的Struct表达式
fn RandomGenerator::gen_complex_struct_expr(
  self : Self,
  name : Upper,
  fields : Array[(Ident, Type)],
) -> Expr {
  let choices = [
    (0, 40), // 结构体构造
    (1, 30), // 变量引用
    (2, 20), // 函数调用
    (3, 10),
  ] // 字段访问
  match self.weighted_choice(choices) {
    Some(0) => self.gen_struct_construct_expr(name, fields)
    Some(1) =>
      match self.gen_var_expr(Struct(name, fields)) {
        Some(expr) => expr
        None => self.gen_struct_construct_expr(name, fields)
      }
    Some(2) =>
      match self.gen_function_call_expr(Struct(name, fields)) {
        Some(expr) => expr
        None => self.gen_struct_construct_expr(name, fields)
      }
    Some(3) =>
      match self.gen_dot_access_expr(Struct(name, fields)) {
        Some(expr) => expr
        None => self.gen_struct_construct_expr(name, fields)
      }
    _ => self.gen_struct_construct_expr(name, fields)
  }
}

///| 生成复杂的Enum表达式
fn RandomGenerator::gen_complex_enum_expr(
  self : Self,
  name : Upper,
  variants : Array[(Upper, Array[Type])],
) -> Expr {
  let choices = [
    (0, 40), // 枚举构造
    (1, 30), // 变量引用
    (2, 20), // 函数调用
    (3, 10),
  ] // 字段访问
  match self.weighted_choice(choices) {
    Some(0) => self.gen_enum_construct_expr(name, variants)
    Some(1) =>
      match self.gen_var_expr(Enum(name, variants)) {
        Some(expr) => expr
        None => self.gen_enum_construct_expr(name, variants)
      }
    Some(2) =>
      match self.gen_function_call_expr(Enum(name, variants)) {
        Some(expr) => expr
        None => self.gen_enum_construct_expr(name, variants)
      }
    Some(3) =>
      match self.gen_dot_access_expr(Enum(name, variants)) {
        Some(expr) => expr
        None => self.gen_enum_construct_expr(name, variants)
      }
    _ => self.gen_enum_construct_expr(name, variants)
  }
}

///| 生成算术表达式 (仅适用于Int和Double)
fn RandomGenerator::gen_arithmetic_expr(self : Self, ty : Type) -> Expr {
  // 始终生成简单的双操作数表达式，避免无限递归
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

///| 生成比较表达式 (返回Bool)
fn RandomGenerator::gen_comparison_expr(self : Self) -> Expr {
  let ty = if self.rand_bool() { Type::Int } else { Type::Double }

  // 生成简单比较，避免递归复杂度
  let left = self.gen_expr(ty, true)
  let right = self.gen_expr(ty, true)
  self.make_comparison(left, right)
}

///|
fn RandomGenerator::make_comparison(
  self : Self,
  left : Expr,
  right : Expr,
) -> Expr {
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

///| 生成逻辑表达式 (返回Bool)
fn RandomGenerator::gen_logical_expr(self : Self) -> Expr {
  match self.rand_int(2) {
    0 => {
      // AND 表达式
      let left = self.gen_expr(Bool, true) // 使用简单表达式
      let right = self.gen_expr(Bool, true)
      AndExpr(left, right)
    }
    _ => {
      // OR 表达式
      let left = self.gen_expr(Bool, true) // 使用简单表达式
      let right = self.gen_expr(Bool, true)
      OrExpr(left, right)
    }
  }
}

///| 生成if表达式
pub fn RandomGenerator::gen_if_expr(self : Self, expected_type : Type) -> Expr? {
  // 防止过深的嵌套
  if self.current_depth >= self.max_depth - 1 {
    return None
  }

  // 生成条件表达式（使用当前环境，不要引入新变量）
  let cond = self.gen_expr(Bool, true)

  // 生成then块，重要：不要创建新的子环境，因为if表达式不应该引入新的作用域
  let old_depth = self.current_depth
  self.current_depth = self.current_depth + 1

  // 生成then块 - 只使用表达式，不添加新变量声明
  let then_block = if expected_type == Unit {
    let block = BlockExpr::new(nested_level=self.current_depth - 1)
    let unit_expr = self.gen_expr(Unit, true)
    block.set_last_expr(unit_expr)
    block
  } else {
    let block = BlockExpr::new(nested_level=self.current_depth - 1)
    // 直接生成返回表达式，不生成语句避免作用域问题
    let return_expr = self.gen_expr(expected_type, true) // 使用简单表达式
    block.set_last_expr(return_expr)
    block
  }

  // 生成else块 - 同样不创建新的作用域
  let else_part = if self.rand_bool() == true &&
    self.current_depth < self.max_depth - 2 {
    // 生成else if，但要小心递归深度
    match self.gen_if_expr(expected_type) {
      Some(nested_if) =>
        match nested_if {
          IfLevelExpr(IfExpr(if_expr)) => Either::Left(if_expr)
          _ => {
            // 如果不是if表达式，创建一个简单的else块
            let else_block = BlockExpr::new(nested_level=self.current_depth - 1)
            let else_expr = self.gen_expr(expected_type, true)
            else_block.set_last_expr(else_expr)
            Either::Right(else_block)
          }
        }
      None => {
        // 生成简单的else块
        let else_block = BlockExpr::new(nested_level=self.current_depth - 1)
        let else_expr = self.gen_expr(expected_type, true)
        else_block.set_last_expr(else_expr)
        Either::Right(else_block)
      }
    }
  } else {
    // 生成简单的else块
    let else_block = BlockExpr::new(nested_level=self.current_depth - 1)
    let else_expr = self.gen_expr(expected_type, true)
    else_block.set_last_expr(else_expr)
    Either::Right(else_block)
  }

  // 恢复深度
  self.current_depth = old_depth
  let if_expr = IfExpr::{ cond, then_: then_block, else_: else_part }
  Some(IfLevelExpr(IfExpr(if_expr)))
}

///| 生成match表达式
pub fn RandomGenerator::gen_match_expr(
  self : Self,
  expected_type : Type,
) -> Expr? {
  // 防止过深的嵌套
  if self.current_depth >= self.max_depth - 1 {
    return None
  }

  // 选择一个合适的类型用于匹配
  let match_type = match self.rand_int(4) {
    0 => Type::Bool
    1 => Type::Int
    2 => {
      // 尝试使用已定义的枚举类型
      let available_enums = Array::new()
      self.enum_defs.each(fn(_, enum_def) {
        available_enums.push(enum_def.type_of())
      })
      match self.choice(available_enums) {
        Some(enum_type) => enum_type
        None => Type::Bool // 备用方案
      }
    }
    _ =>
      // 简单的元组类型
      Type::Tuple([Type::Int, Type::Bool])
  }

  // 生成匹配表达式
  let match_expr_val = self.gen_expr(match_type, true)

  // 创建子环境
  let old_var_env = self.var_env
  let old_mut_env = self.mutable_vars
  let old_depth = self.current_depth
  self.var_env = old_var_env.sub_env()
  self.mutable_vars = old_mut_env.sub_env()
  self.current_depth = self.current_depth + 1

  // 生成匹配分支
  let arms = self.gen_match_arms(match_type, expected_type)

  // 恢复环境
  self.var_env = old_var_env
  self.mutable_vars = old_mut_env
  self.current_depth = old_depth
  if arms.is_empty() {
    None
  } else {
    let match_expr = MatchExpr::{
      nested_level: self.current_depth,
      expr: match_expr_val,
      arms,
    }
    Some(IfLevelExpr(MatchExpr(match_expr)))
  }
}

///| 生成匹配分支
fn RandomGenerator::gen_match_arms(
  self : Self,
  match_type : Type,
  result_type : Type,
) -> Array[(Pattern, Expr)] {
  let arms = Array::new()
  match match_type {
    Bool => {
      // 布尔类型的匹配
      let true_expr = self.gen_expr(result_type, true)
      let false_expr = self.gen_expr(result_type, true)
      arms.push((Pattern::Bool(true), true_expr))
      arms.push((Pattern::Bool(false), false_expr))
    }
    Int => {
      // 整数类型的匹配
      let arm_count = self.rand_int(3) + 2 // 2-4个分支
      for i = 0; i < arm_count - 1; i = i + 1 {
        let pattern_val = self.rand_int(10)
        let arm_expr = self.gen_expr(result_type, true)
        arms.push((Pattern::Number(pattern_val), arm_expr))
      }
      // 添加通配符分支
      let wildcard_expr = self.gen_expr(result_type, true)
      arms.push((Pattern::WildCard, wildcard_expr))
    }
    Enum(enum_name, variants) =>
      // 枚举类型的匹配
      for variant in variants {
        let (variant_name, variant_types) = variant
        let patterns = Array::new()
        for variant_type in variant_types {
          // 生成简单的模式
          match variant_type {
            Int => patterns.push(Pattern::WildCard) // 简化为通配符
            Bool => patterns.push(Pattern::WildCard)
            _ => patterns.push(Pattern::WildCard)
          }
        }
        let arm_expr = self.gen_expr(result_type, true)
        arms.push(
          (
            Pattern::EnumPattern(Some(enum_name), variant_name, patterns),
            arm_expr,
          ),
        )
      }
    Tuple(elem_types) => {
      // 元组类型的匹配
      let patterns = Array::new()
      for elem_type in elem_types {
        // 生成简单的模式
        match elem_type {
          Int =>
            if self.rand_bool() == true {
              patterns.push(Pattern::Number(self.rand_int(5)))
            } else {
              patterns.push(Pattern::WildCard)
            }
          Bool =>
            if self.rand_bool() == true {
              patterns.push(Pattern::Bool(self.rand_bool()))
            } else {
              patterns.push(Pattern::WildCard)
            }
          _ => patterns.push(Pattern::WildCard)
        }
      }
      let arm_expr = self.gen_expr(result_type, true)
      arms.push((Pattern::Tuple(patterns), arm_expr))

      // 添加通配符分支
      let wildcard_expr = self.gen_expr(result_type, true)
      arms.push((Pattern::WildCard, wildcard_expr))
    }
    _ => {
      // 其他类型使用通配符
      let wildcard_expr = self.gen_expr(result_type, true)
      arms.push((Pattern::WildCard, wildcard_expr))
    }
  }
  arms
}

///| 增强的语句生成器，支持更多种类的语句
fn RandomGenerator::gen_stmt(self : Self) -> Stmt {
  // 根据深度和行数目标调整语句复杂度
  if self.current_depth >= self.max_depth - 1 {
    // 在深度较大时，生成简单语句
    let choices = [
      (0, 50), // let语句 - 增加权重
      (1, 30), // let mut语句 - 增加权重
      (2, 15), // 赋值语句
      (3, 5),
    ] // 表达式语句 - 大幅减少权重
    match self.weighted_choice(choices) {
      Some(0) => self.gen_let_stmt()
      Some(1) => self.gen_expr_stmt()
      Some(2) => self.gen_let_mut_stmt()
      Some(3) => self.gen_assign_stmt()
      _ => self.gen_let_stmt()
    }
  } else {
    // 在较浅深度时，可以生成更复杂的语句，确保可变变量得到使用
    let choices = [
      (0, 35), // let语句 - 适中权重
      (1, 20), // let mut语句 - 减少权重，避免生成过多未使用的可变变量
      (2, 25), // 赋值语句 - 增加权重，确保可变变量被使用
      (3, 8), // 表达式语句 - 减少权重
      (4, 5), // while循环 - 正常权重
      (5, 3), // 元组解构let - 减少权重
      (6, 1), // return语句 - 减少权重
      (7, 1), // 局部函数定义 - 大幅减少权重
    ]
    match self.weighted_choice(choices) {
      Some(0) => self.gen_let_stmt()
      Some(1) => self.gen_let_mut_stmt()
      Some(2) => self.gen_assign_stmt()
      Some(3) => self.gen_expr_stmt()
      Some(4) => self.gen_while_stmt()
      Some(5) => self.gen_let_tuple_stmt()
      Some(6) => self.gen_return_stmt()
      Some(7) => self.gen_local_func_def_stmt()
      _ => self.gen_let_stmt()
    }
  }
}

///|
fn RandomGenerator::gen_let_stmt(self : Self) -> Stmt {
  let var_name = self.gen_ident()
  let var_type = self.gen_type(true)
  let init_expr = self.gen_expr(var_type, false)

  // 将变量添加到环境中
  self.var_env.set(var_name, var_type)

  // 记录变量定义
  self.record_var_definition(var_name, var_type)

  // 更频繁地生成类型注解，提高代码可读性
  let type_annotation = if self.rand_int(10) < 7 {
    Some(var_type)
  } else {
    None
  }
  Let(var_name, type_annotation, init_expr)
}

///|
fn RandomGenerator::gen_let_mut_stmt(self : Self) -> Stmt {
  let var_name = self.gen_ident()
  let var_type = self.gen_type(true)
  let init_expr = self.gen_expr(var_type, false)

  // 将变量添加到环境中（包括可变变量环境）
  self.var_env.set(var_name, var_type)
  self.mutable_vars.set(var_name, var_type)

  // 记录变量定义
  self.record_var_definition(var_name, var_type)

  // 将变量添加到未使用的可变变量列表中
  self.unused_mutable_vars.set(var_name, var_type)
  let type_annotation = if self.rand_int(10) < 7 {
    Some(var_type)
  } else {
    None
  }
  LetMut(var_name, type_annotation, init_expr)
}

///| 生成元组解构let语句
fn RandomGenerator::gen_let_tuple_stmt(self : Self) -> Stmt {
  let tuple_size = self.rand_int(self.max_tuple_size - 1) + 2 // 至少2个元素
  let bindings = Array::new()
  let types = Array::new()

  // 首先确定所有类型
  for i = 0; i < tuple_size; i = i + 1 {
    let elem_type = self.gen_type(false)
    types.push(elem_type)
  }

  // 生成元组表达式（不能引用即将被绑定的变量）
  let tuple_type = Type::Tuple(types)
  let tuple_expr = self.gen_expr(tuple_type, false)

  // 然后生成绑定并将变量添加到环境（在生成表达式之后）
  for i = 0; i < tuple_size; i = i + 1 {
    let elem_type = types[i]
    if self.rand_bool() == true {
      // 生成标识符绑定
      let var_name = self.gen_ident()
      bindings.push(Binding::Ident(var_name))
      // 在生成表达式后才添加到环境，避免循环引用
      self.var_env.set(var_name, elem_type)
      // 记录变量定义
      self.record_var_definition(var_name, elem_type)
    } else {
      // 生成通配符绑定
      bindings.push(Binding::WhileCard)
    }
  }
  let type_annotation = if self.rand_bool() == true {
    Some(tuple_type)
  } else {
    None
  }
  LetTuple(bindings, type_annotation, tuple_expr)
}

///|
fn RandomGenerator::gen_assign_stmt(self : Self) -> Stmt {
  // 获取所有可变变量（包括父环境中的）
  let mut_vars = Array::new()
  self.collect_mutable_vars_recursive(self.mutable_vars, mut_vars)
  if mut_vars.is_empty() {
    // 如果没有可变变量，生成一个let语句
    return self.gen_let_stmt()
  }
  let (var_name, var_type) = self.choice(mut_vars).unwrap()

  // 验证变量确实存在于当前可访问的环境中
  match self.var_env.get(var_name) {
    Some(_) => {
      // 变量确实存在，可以安全赋值
      self.mark_mutable_var_used(var_name)

      // 根据变量类型生成合适的左值，但要确保简单性避免复杂的访问模式
      let left_value = LeftValue::Ident(var_name) // 始终使用简单的变量赋值
      let new_value = self.gen_expr(var_type, true) // 使用简单表达式避免复杂性
      Assign(left_value, new_value)
    }
    None =>
      // 变量不存在，生成let语句代替
      self.gen_let_stmt()
  }
}

// 辅助函数：递归收集可变变量

///|
fn RandomGenerator::collect_mutable_vars_recursive(
  self : Self,
  env : Env[Ident, Type],
  result : Array[(Ident, Type)],
) -> Unit {
  env.map.each(fn(var_name, var_type) {
    // 只添加在当前变量环境中确实存在的变量
    match self.var_env.get(var_name) {
      Some(_) => result.push((var_name, var_type))
      None => ()
    }
  })
  match env.parent {
    Some(parent) => self.collect_mutable_vars_recursive(parent, result)
    None => ()
  }
}

///|
fn RandomGenerator::gen_while_stmt(self : Self) -> Stmt {
  // 生成一个带有确定终止条件的while循环
  // 使用模式：let mut counter = 0; while counter < limit { ... counter = counter + 1 }

  // 创建子环境
  let old_var_env = self.var_env
  let old_mut_env = self.mutable_vars
  self.var_env = old_var_env.sub_env()
  self.mutable_vars = old_mut_env.sub_env()

  // 生成循环计数器变量
  let counter_name = self.gen_ident()
  let counter_type = Int
  let initial_value = IfLevelExpr(ApplyExpr(ValueExpr(IntExpr(0)))) // 从0开始

  // 将计数器添加到环境中
  self.var_env.set(counter_name, counter_type)
  self.mutable_vars.set(counter_name, counter_type)

  // 生成循环限制
  let limit = self.rand_int(10) + 1 // 1-10次循环
  let limit_expr = IfLevelExpr(ApplyExpr(ValueExpr(IntExpr(limit))))

  // 生成条件：counter < limit
  let counter_ref = IfLevelExpr(ApplyExpr(ValueExpr(IdentExpr(counter_name))))
  let cond = CmpExpr(Lt, counter_ref, limit_expr)

  // 生成循环体，并在最后添加计数器递增
  let body = self.gen_block_expr(Unit)

  // 在循环体中添加计数器递增语句
  let increment_expr = AddExpr(
    IfLevelExpr(ApplyExpr(ValueExpr(IdentExpr(counter_name)))),
    IfLevelExpr(ApplyExpr(ValueExpr(IntExpr(1)))),
  )
  let increment_stmt = Assign(LeftValue::Ident(counter_name), increment_expr)
  body.push(increment_stmt)

  // 恢复环境
  self.var_env = old_var_env
  self.mutable_vars = old_mut_env

  // 创建一个包含let mut和while的复合语句块
  let wrapper_block = BlockExpr::new(nested_level=self.current_depth)

  // 添加let mut语句
  let let_mut_stmt = LetMut(counter_name, Some(counter_type), initial_value)
  wrapper_block.push(let_mut_stmt)

  // 添加while语句
  let while_stmt = While(cond, body)
  wrapper_block.push(while_stmt)

  // 返回一个表达式语句包装这个块
  ExprStmt(IfLevelExpr(ApplyExpr(ValueExpr(BlockExpr(wrapper_block)))))
}

///|
fn RandomGenerator::gen_return_stmt(self : Self) -> Stmt {
  let return_expr = self.gen_expr(self.current_return_type, false)
  Return(return_expr)
}

///|
pub fn RandomGenerator::gen_local_func_def_stmt(self : Self) -> Stmt {
  // 减少局部函数的生成，因为它们容易引起作用域问题
  if self.current_depth >= self.max_depth - 2 {
    return self.gen_let_stmt() // 在深层嵌套中不生成局部函数
  }
  let param_count = self.rand_int(3) // 0-2个参数，减少复杂度
  let params = Array::new()

  // 生成函数名和类型
  let func_name = self.gen_ident()
  let ret_ty = self.gen_basic_type() // 使用基础类型确保安全

  // 生成参数类型
  let param_types = Array::new()
  for i = 0; i < param_count; i = i + 1 {
    let param_type = self.gen_basic_type() // 使用基础类型确保安全
    param_types.push(param_type)
  }

  // 将函数类型添加到当前环境中（在生成函数体之前）
  let func_type = Type::Func(param_types, ret_ty)
  self.var_env.set(func_name, func_type)

  // 创建完全独立的函数作用域，不继承外部变量
  let old_var_env = self.var_env
  let old_mut_env = self.mutable_vars
  self.var_env = Env::new() // 全新的环境，不继承任何外部变量
  self.mutable_vars = Env::new()

  // 生成参数并添加到函数作用域
  for i = 0; i < param_count; i = i + 1 {
    let param_name = self.gen_ident()
    let param_type = param_types[i]
    params.push((param_name, Some(param_type)))

    // 将参数添加到函数内部环境中
    self.var_env.set(param_name, param_type)
    // 记录参数为已定义变量
    self.record_var_definition(param_name, param_type)
    // 参数被定义时即被使用
    self.mark_var_used(param_name)
  }

  // 生成函数体 - 使用变量跟踪
  let old_return_type = self.current_return_type
  self.current_return_type = ret_ty
  let body = self.gen_block_expr_with_usage_tracking(ret_ty)

  // 恢复环境
  self.current_return_type = old_return_type
  self.var_env = old_var_env
  self.mutable_vars = old_mut_env
  LocalFuncDef(func_name, params, Some(ret_ty), body)
}

///|
fn RandomGenerator::gen_expr_stmt(self : Self) -> Stmt {
  // 表达式语句应该只生成Unit类型或if/match表达式，避免需要let _ = 前缀的情况
  let expr = match self.rand_int(3) {
    0 =>
      // 生成Unit表达式
      self.gen_expr(Unit, true)
    1 =>
      // 生成if表达式（不需要let _ = 前缀）
      match self.gen_if_expr(Unit) {
        Some(expr) => expr
        None => self.gen_expr(Unit, true)
      }
    _ =>
      // 生成match表达式（不需要let _ = 前缀）
      match self.gen_match_expr(Unit) {
        Some(expr) => expr
        None => self.gen_expr(Unit, true)
      }
  }
  ExprStmt(expr)
}

///| 生成对未使用类型的构造和使用语句
fn RandomGenerator::gen_force_use_unused_types(
  self : Self,
  block : BlockExpr,
) -> Unit {
  // 强制使用所有结构体类型
  self.struct_defs.each(fn(struct_name, struct_def) {
    if true { // 使用所有结构体
      // 生成结构体构造表达式并赋值给变量
      let var_name = self.gen_ident()
      let struct_expr = self.gen_struct_construct_expr(
        struct_name,
        struct_def.fields,
      )
      let let_stmt = Let(var_name, Some(struct_def.type_of()), struct_expr)
      block.push(let_stmt)

      // 访问所有结构体字段
      for field in struct_def.fields {
        let (field_name, field_type) = field
        let field_access = IfLevelExpr(
          ApplyExpr(
            DotAcc(
              ApplyExpr::ValueExpr(ValueExpr::IdentExpr(var_name)),
              field_name,
            ),
          ),
        )
        match field_type {
          Int => {
            // 为Int类型生成println调用
            let println_call = IfLevelExpr(
              ApplyExpr(
                Call(
                  ApplyExpr::ValueExpr(ValueExpr::IdentExpr(Ident("println"))),
                  [field_access],
                ),
              ),
            )
            block.push(ExprStmt(println_call))
          }
          _ =>
            // 为其他类型生成简单的表达式语句
            block.push(ExprStmt(field_access))
        }
      }

      // 使用整个结构体
      let usage_expr = IfLevelExpr(ApplyExpr(ValueExpr(IdentExpr(var_name))))
      block.push(ExprStmt(usage_expr))
    }
  })

  // 强制使用所有枚举类型，通过match表达式
  self.enum_defs.each(fn(enum_name, enum_def) {
    if true { // 使用所有枚举
      // 生成枚举构造表达式并赋值给变量
      let var_name = self.gen_ident()
      let enum_expr = self.gen_enum_construct_expr(enum_name, enum_def.variants)
      let let_stmt = Let(var_name, Some(enum_def.type_of()), enum_expr)
      block.push(let_stmt)

      // 生成match表达式使用所有变体
      if !enum_def.variants.is_empty() {
        let match_arms = Array::new()
        for variant in enum_def.variants {
          let (variant_name, variant_types) = variant
          let patterns = Array::new()
          for _ in variant_types {
            patterns.push(Pattern::WildCard) // 使用通配符模式
          }
          let pattern = Pattern::EnumPattern(
            Some(enum_name),
            variant_name,
            patterns,
          )
          let arm_expr = IfLevelExpr(ApplyExpr(ValueExpr(UnitExpr)))
          match_arms.push((pattern, arm_expr))
        }
        let var_expr = IfLevelExpr(ApplyExpr(ValueExpr(IdentExpr(var_name))))
        let match_expr = MatchExpr::{
          nested_level: self.current_depth + 1,
          expr: var_expr,
          arms: match_arms,
        }
        let match_stmt = ExprStmt(IfLevelExpr(MatchExpr(match_expr)))
        block.push(match_stmt)
      }

      // 使用整个枚举
      let usage_expr = IfLevelExpr(ApplyExpr(ValueExpr(IdentExpr(var_name))))
      block.push(ExprStmt(usage_expr))
    }
  })

  // 强制调用所有函数
  self.func_defs.each(fn(func_name, func_def) {
    if true { // 调用所有函数
      // 生成函数调用
      let args = Array::new()
      for param in func_def.params {
        let (_, param_type) = param
        args.push(self.gen_expr(param_type, true))
      }
      let func_call = IfLevelExpr(
        ApplyExpr(
          Call(ApplyExpr::ValueExpr(ValueExpr::IdentExpr(func_name)), args),
        ),
      )
      if func_def.ret_ty == Unit {
        // Unit返回类型，直接作为表达式语句
        block.push(ExprStmt(func_call))
      } else {
        // 非Unit返回类型，赋值给变量然后使用
        let var_name = self.gen_ident()
        let let_stmt = Let(var_name, Some(func_def.ret_ty), func_call)
        block.push(let_stmt)

        // 根据返回类型决定如何使用
        match func_def.ret_ty {
          Int => {
            let var_expr = IfLevelExpr(
              ApplyExpr(ValueExpr(IdentExpr(var_name))),
            )
            let println_call = IfLevelExpr(
              ApplyExpr(
                Call(
                  ApplyExpr::ValueExpr(ValueExpr::IdentExpr(Ident("println"))),
                  [var_expr],
                ),
              ),
            )
            block.push(ExprStmt(println_call))
          }
          _ => {
            let usage_expr = IfLevelExpr(
              ApplyExpr(ValueExpr(IdentExpr(var_name))),
            )
            block.push(ExprStmt(usage_expr))
          }
        }
      }
    }
  })

  // 强制使用所有顶层变量
  self.var_env.map.each(fn(var_name, var_type) {
    // 检查这个变量是否是在全局作用域（非函数参数）
    if self.current_depth <= 1 { // 在main函数的顶层
      match var_type {
        Int => {
          // 为Int类型生成println调用
          let var_expr = IfLevelExpr(ApplyExpr(ValueExpr(IdentExpr(var_name))))
          let println_call = IfLevelExpr(
            ApplyExpr(
              Call(
                ApplyExpr::ValueExpr(ValueExpr::IdentExpr(Ident("println"))),
                [var_expr],
              ),
            ),
          )
          block.push(ExprStmt(println_call))
        }
        _ => {
          // 为其他类型生成简单的表达式语句
          let var_expr = IfLevelExpr(ApplyExpr(ValueExpr(IdentExpr(var_name))))
          block.push(ExprStmt(var_expr))
        }
      }
    }
  })
}

///| 生成代码块并在结束时处理未使用变量
fn RandomGenerator::gen_block_expr_with_usage_tracking(
  self : Self,
  return_type : Type,
) -> BlockExpr {
  // 保存当前已定义变量状态
  let scope_start_vars = Map::new()
  self.all_defined_vars.each(fn(k, v) { scope_start_vars.set(k, v) })

  // 生成正常的代码块
  let block = self.gen_block_expr(return_type)

  // 为当前作用域中定义但未使用的变量生成使用语句
  self.all_defined_vars.each(fn(var_name, var_type) {
    // 检查这个变量是否是在当前作用域中新定义的
    match scope_start_vars.get(var_name) {
      None =>
        // 这是一个新定义的变量，检查是否未使用
        match self.used_vars.get(var_name) {
          Some(false) =>
            // 变量未使用，生成使用语句
            match self.var_env.get(var_name) {
              Some(_) => {
                // 变量在当前环境中可见，生成使用语句
                match var_type {
                  Int => {
                    // 为Int类型生成println调用
                    let var_expr = IfLevelExpr(
                      ApplyExpr(ValueExpr(IdentExpr(var_name))),
                    )
                    let println_call = IfLevelExpr(
                      ApplyExpr(
                        Call(
                          ApplyExpr::ValueExpr(
                            ValueExpr::IdentExpr(Ident("println")),
                          ),
                          [var_expr],
                        ),
                      ),
                    )
                    block.push(ExprStmt(println_call))
                  }
                  _ => {
                    // 为其他类型生成简单的表达式语句
                    let var_expr = IfLevelExpr(
                      ApplyExpr(ValueExpr(IdentExpr(var_name))),
                    )
                    block.push(ExprStmt(var_expr))
                  }
                }
                // 标记变量已使用
                self.mark_var_used(var_name)
              }
              None => () // 变量不可见，跳过
            }
          _ => () // 变量已使用或未定义，跳过
        }
      Some(_) => () // 变量不是在当前作用域定义的，跳过
    }
  })

  // 额外增强：对于任何在当前环境中可见但标记为未使用的变量，强制使用
  self.var_env.map.each(fn(var_name, var_type) {
    match self.used_vars.get(var_name) {
      Some(false) => {
        // 未使用的变量，强制生成使用语句
        match var_type {
          Int => {
            let var_expr = IfLevelExpr(
              ApplyExpr(ValueExpr(IdentExpr(var_name))),
            )
            let println_call = IfLevelExpr(
              ApplyExpr(
                Call(
                  ApplyExpr::ValueExpr(ValueExpr::IdentExpr(Ident("println"))),
                  [var_expr],
                ),
              ),
            )
            block.push(ExprStmt(println_call))
          }
          _ => {
            let var_expr = IfLevelExpr(
              ApplyExpr(ValueExpr(IdentExpr(var_name))),
            )
            block.push(ExprStmt(var_expr))
          }
        }
        self.mark_var_used(var_name)
      }
      _ => ()
    }
  })

  // 特别处理可变变量：确保所有可变变量的可变性都被使用
  self.mutable_vars.map.each(fn(var_name, var_type) {
    match self.var_env.get(var_name) {
      Some(_) => {
        // 可变变量在当前环境中可见，生成多次赋值以使用其可变性
        let left_value = LeftValue::Ident(var_name)

        // 第一次赋值
        let new_value1 = self.gen_expr(var_type, true) // 生成简单表达式
        block.push(Assign(left_value, new_value1))

        // 第二次赋值，确保可变性被使用
        let new_value2 = self.gen_expr(var_type, true) // 生成另一个简单表达式
        block.push(Assign(left_value, new_value2))
        self.mark_mutable_var_used(var_name)
      }
      None => () // 变量不在当前环境中可见，跳过
    }
  })

  // 清空未使用列表
  self.unused_mutable_vars = Map::new()

  // 在main函数中强制使用一些顶层定义
  if self.current_depth <= 1 { // 只在顶层块中生成
    self.gen_force_use_unused_types(block)
  }
  block
}

///| 增强的代码块生成器，支持更复杂的代码结构
fn RandomGenerator::gen_block_expr(
  self : Self,
  return_type : Type,
) -> BlockExpr {
  let block = BlockExpr::new(nested_level=self.current_depth)

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

  // 根据目标行数动态调整语句数量
  let base_stmt_count = if self.current_lines < self.total_lines_target / 2 {
    // 如果当前行数少于目标的一半，生成更多语句
    self.rand_int(25) + 15 // 15-39个语句
  } else if self.current_lines < self.total_lines_target * 3 / 4 {
    // 如果接近目标，适中数量
    self.rand_int(15) + 10 // 10-24个语句
  } else {
    // 如果接近或超过目标，较少语句
    self.rand_int(8) + 5 // 5-12个语句
  }
  let stmt_count = if self.current_depth <= 2 {
    base_stmt_count
  } else {
    base_stmt_count / (self.current_depth - 1) // 深度越大，语句越少
  }
  for i = 0; i < stmt_count; i = i + 1 {
    let stmt = self.gen_stmt()
    block.push(stmt)
    self.current_lines = self.current_lines + self.estimate_stmt_lines(stmt)

    // 如果已经达到目标行数，停止生成更多语句
    if self.current_lines >= self.total_lines_target {
      break
    }
  }

  // 在函数结束前，为所有未使用的可变变量生成强制赋值
  // 但只有在当前环境中确实存在这些变量时才这样做
  let force_assignments = self.gen_safe_force_assign_unused_mutables()
  for assign_stmt in force_assignments {
    block.push(assign_stmt)
    self.current_lines = self.current_lines + 1
  }

  // 生成返回表达式（如果需要）
  if return_type != Unit {
    let last_expr = if self.rand_bool() == true {
      self.gen_literal_expr(return_type) // 有时生成字面量
    } else {
      self.gen_expr(return_type, false) // 有时生成复杂表达式
    }
    block.set_last_expr(last_expr)
    self.current_lines = self.current_lines + 1
  }

  // 恢复环境
  self.var_env = old_var_env
  self.mutable_vars = old_mut_env
  self.current_depth = old_depth
  block
}

///| 估算语句的行数（用于控制总行数）
fn RandomGenerator::estimate_stmt_lines(self : Self, stmt : Stmt) -> Int {
  match stmt {
    Let(_, _, _) => 1
    LetMut(_, _, _) => 1
    LetTuple(_, _, _) => 1
    Assign(_, _) => 1
    ExprStmt(_) => 1
    Return(_) => 1
    LocalFuncDef(_, params, _, body) =>
      2 + params.length() + self.estimate_block_lines(body)
    While(_, body) => 1 + self.estimate_block_lines(body)
  }
}

///| 估算代码块的行数
fn RandomGenerator::estimate_block_lines(self : Self, block : BlockExpr) -> Int {
  let lines = 2 // { 和 }
  let stmt_lines = block.stmts.fold(init=0, fn(acc, stmt) {
    acc + self.estimate_stmt_lines(stmt)
  })
  let expr_lines = match block.last_expr {
    Some(_) => 1
    None => 0
  }
  lines + stmt_lines + expr_lines
}

///| 增强的顶层定义生成器
pub fn RandomGenerator::gen_top_let(self : Self) -> TopLet {
  let name = self.gen_ident()
  let ty = self.gen_type(true)
  let value = self.gen_expr(ty, false)

  // 将全局变量添加到环境中
  self.var_env.set(name, ty)

  // 总是生成类型注解以确保类型安全
  let type_annotation = Some(ty)
  { name, ty: type_annotation, value }
}

///|
pub fn RandomGenerator::gen_struct_def(self : Self) -> StructDef {
  let name = self.gen_upper()
  let field_count = self.rand_int(5) + 1 // 1-5个字段，减少复杂度
  let fields = Array::new()
  for i = 0; i < field_count; i = i + 1 {
    let field_name = self.gen_ident()
    // 对于结构体字段，主要使用基础类型或简单的复合类型
    let field_type = if self.rand_int(10) < 7 {
      self.gen_basic_type() // 70%概率使用基础类型
    } else {
      // 30%概率使用简单的复合类型
      match self.rand_int(2) {
        0 => Array(self.gen_basic_type())
        _ => {
          let tuple_size = self.rand_int(3) + 2
          let tuple_types = Array::new()
          for j = 0; j < tuple_size; j = j + 1 {
            tuple_types.push(self.gen_basic_type())
          }
          Tuple(tuple_types)
        }
      }
    }
    fields.push((field_name, field_type))
  }
  let struct_def = { generic_param: None, name, fields }

  // 注册结构体定义
  self.struct_defs.set(name, struct_def)

  // 将结构体类型添加到类型环境
  self.type_env.set(name, struct_def.type_of())
  struct_def
}

///|
pub fn RandomGenerator::gen_enum_def(self : Self) -> EnumDef {
  let name = self.gen_upper()
  let variant_count = self.rand_int(4) + 1 // 1-4个变体，减少复杂度
  let variants = Array::new()
  for i = 0; i < variant_count; i = i + 1 {
    let variant_name = self.gen_upper()
    let field_count = self.rand_int(4) // 0-3个字段，减少复杂度
    let variant_fields = Array::new()
    for j = 0; j < field_count; j = j + 1 {
      // 对于枚举变体，主要使用基础类型
      let field_type = if self.rand_int(10) < 8 {
        self.gen_basic_type() // 80%概率使用基础类型
      } else {
        Array(self.gen_basic_type()) // 20%概率使用基础类型的数组
      }
      variant_fields.push(field_type)
    }
    variants.push((variant_name, variant_fields))
  }
  let enum_def = { generic_param: None, name, variants }

  // 注册枚举定义
  self.enum_defs.set(name, enum_def)

  // 将枚举类型添加到类型环境
  self.type_env.set(name, enum_def.type_of())
  enum_def
}

///|
pub fn RandomGenerator::gen_top_func_def(self : Self) -> TopFuncDef {
  let name = self.gen_ident()
  let param_count = self.rand_int(self.max_func_params)
  let params = Array::new()

  // 创建新的环境作用域
  let old_var_env = self.var_env
  let old_mut_env = self.mutable_vars
  self.var_env = old_var_env.sub_env()
  self.mutable_vars = old_mut_env.sub_env()

  // 生成参数，使用基础类型确保类型安全
  for i = 0; i < param_count; i = i + 1 {
    let param_name = self.gen_ident()
    let param_type = self.gen_basic_type() // 使用基础类型确保安全
    params.push((param_name, param_type))

    // 将参数添加到环境中
    self.var_env.set(param_name, param_type)
    // 记录参数为已定义变量
    self.record_var_definition(param_name, param_type)
    // 参数被定义时即被使用
    self.mark_var_used(param_name)
  }

  // 生成返回类型和函数体
  let ret_ty = self.gen_basic_type() // 使用基础类型确保安全
  let old_return_type = self.current_return_type
  self.current_return_type = ret_ty

  // 使用变量跟踪的代码块生成
  let body = self.gen_block_expr_with_usage_tracking(ret_ty)

  // 恢复环境和返回类型
  self.current_return_type = old_return_type
  self.var_env = old_var_env
  self.mutable_vars = old_mut_env
  let func_def = { generic_param: None, name, params, ret_ty, body }

  // 注册函数定义
  self.func_defs.set(name, func_def)
  func_def
}

///| 生成main函数（不带参数和泛型参数）
pub fn RandomGenerator::gen_main_func(self : Self) -> TopFuncDef {
  let name : Ident = Ident("main")
  let params : Array[(Ident, Type)] = [] // main函数没有参数
  let ret_ty = Unit // main函数返回Unit

  // 创建新的环境作用域
  let old_var_env = self.var_env
  let old_mut_env = self.mutable_vars
  self.var_env = old_var_env.sub_env()
  self.mutable_vars = old_mut_env.sub_env()
  let old_return_type = self.current_return_type
  self.current_return_type = Unit

  // 生成一个较大的函数体，包含更多语句
  let body = self.gen_block_expr_with_usage_tracking(Unit)

  // 恢复环境和返回类型
  self.current_return_type = old_return_type
  self.var_env = old_var_env
  self.mutable_vars = old_mut_env
  { generic_param: None, name, params, ret_ty, body }
}

///|
pub fn RandomGenerator::gen_top_decl(self : Self) -> TopDecl {
  let choices = [
    (0, 30), // 顶层let
    (1, 25), // 结构体定义  
    (2, 25), // 函数定义
    (3, 20),
  ] // 枚举定义
  match self.weighted_choice(choices) {
    Some(0) => TopDecl::TopLet(self.gen_top_let())
    Some(1) => TopDecl::StructDef(self.gen_struct_def())
    Some(2) => TopDecl::TopFuncDef(self.gen_top_func_def())
    Some(3) => TopDecl::EnumDef(self.gen_enum_def())
    _ => TopDecl::TopLet(self.gen_top_let())
  }
}

///| 程序生成器，确保包含main函数并生成合理长度的程序
pub fn RandomGenerator::gen_program(self : Self) -> Program {
  let top_decls = Array::new()

  // 首先生成适量基础类型定义
  let struct_count = self.rand_int(5) + 3 // 3-7个结构体
  for i = 0; i < struct_count; i = i + 1 {
    top_decls.push(TopDecl::StructDef(self.gen_struct_def()))
    self.current_lines = self.current_lines + 5
  }
  let enum_count = self.rand_int(4) + 2 // 2-5个枚举
  for i = 0; i < enum_count; i = i + 1 {
    top_decls.push(TopDecl::EnumDef(self.gen_enum_def()))
    self.current_lines = self.current_lines + 6
  }

  // 生成适量函数和顶层变量
  let decl_count = self.rand_int(40) + 40 // 40-79个声明
  for i = 0; i < decl_count; i = i + 1 {
    let decl = self.gen_top_decl()
    top_decls.push(decl)

    // 估算行数
    match decl {
      TopDecl::TopLet(_) => self.current_lines = self.current_lines + 1
      TopDecl::TopFuncDef(func_def) =>
        self.current_lines = self.current_lines +
          3 +
          self.estimate_block_lines(func_def.body)
      TopDecl::StructDef(_) => self.current_lines = self.current_lines + 5
      TopDecl::EnumDef(_) => self.current_lines = self.current_lines + 6
    }
  }

  // 最后生成main函数（必须的）
  let main_func = self.gen_main_func()
  top_decls.push(TopDecl::TopFuncDef(main_func))
  self.current_lines = self.current_lines +
    self.estimate_block_lines(main_func.body) +
    2
  { top_decls, }
}
