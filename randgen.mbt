/// Enhanced random program generator - based on csmith approach, focused on generating complex type-safe MiniMoonbit programs

/// Environment management - used to track variable and type bindings
struct Env[K, V] {
  map : Map[K, V]
  parent : Env[K, V]?
}

/// Create a new environment
fn[K, V] Env::new() -> Env[K, V] {
  { map: Map::new(), parent: None }
}

/// Create a sub-environment
fn[K, V] Env::sub_env(self : Self[K, V]) -> Env[K, V] {
  { map: Map::new(), parent: Some(self) }
}

/// Get value from environment (searches parent environments if needed)
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

/// Set value in current environment
fn[K : Eq + Hash, V] Env::set(self : Self[K, V], key : K, value : V) -> Unit {
  self.map.set(key, value)
}

/// Collect all variables of a specific type from environment
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

/// Generator state and configuration
pub struct RandProgGenerator {
  rand : @random.Rand

  // Enhanced configuration parameters for generating complex programs
  max_depth : Int
  max_stmt_count : Int
  max_array_size : Int
  max_tuple_size : Int
  max_func_params : Int
  max_nested_level : Int
  total_lines_target : Int

  // Environment tracking
  mut var_env : Env[Ident, Type] // Variable type environment
  mut mutable_vars : Env[Ident, Type] // Mutable variable environment
  mut unused_mutable_vars : Map[Ident, Type] // Unused mutable variables (require forced assignment)
  type_env : Env[Upper, Type] // Custom type environment

  // Track all defined variables and their usage
  mut all_defined_vars : Map[Ident, Type] // All defined variables
  mut used_vars : Map[Ident, Bool] // Used variables

  // Defined types and functions
  struct_defs : Map[Upper, StructDef]
  enum_defs : Map[Upper, EnumDef]
  func_defs : Map[Ident, TopFuncDef]

  // Name counter for uniqueness
  mut name_counter : Int

  // Current context
  mut current_return_type : Type
  mut current_depth : Int
  mut current_nested_level : Int

  // Line count tracking
  mut current_lines : Int
}

/// Create new random program generator
pub fn RandProgGenerator::new(seed~ : Int = 0) -> RandProgGenerator {
  let g = RandProgGenerator::{
    rand: @random.Rand::new(),
    max_depth: 4, // Reduce max depth to avoid stack overflow
    max_stmt_count: 15, // Moderate statement count
    max_array_size: 5, // Moderate array size
    max_tuple_size: 4, // Moderate tuple size
    max_func_params: 4, // Moderate function parameter count
    max_nested_level: 3, // Reduce nesting level limit
    total_lines_target: 2000, // Target line count
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

/// Helper functions
fn RandProgGenerator::rand_int(self : Self, max : Int) -> Int {
  if max <= 0 {
    return 0
  }
  self.rand.int(limit=max)
}

/// Generate random boolean
fn RandProgGenerator::rand_bool(self : Self) -> Bool {
  self.rand_int(2) == 0
}

/// Generate random double
fn RandProgGenerator::rand_double(self : Self) -> Double {
  self.rand.double() * 1000.0 // Expand range to generate more diverse floating point numbers
}

/// Choose random element from array
fn[T] RandProgGenerator::choice(self : Self, choices : Array[T]) -> T? {
  if choices.is_empty() {
    None
  } else {
    Some(choices[self.rand_int(choices.length())])
  }
}

/// Weighted random choice
fn[T] RandProgGenerator::weighted_choice(
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

  // Fallback choice
  self.choice(choices.map(fn(choice) { choice.0 }))
}

/// Generate identifier
fn RandProgGenerator::gen_ident(self : Self) -> Ident {
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

/// Generate uppercase identifier
fn RandProgGenerator::gen_upper(self : Self) -> Upper {
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

/// Generate basic types
fn RandProgGenerator::gen_basic_type(self : Self) -> Type {
  match self.rand_int(4) {
    0 => Unit
    1 => Bool
    2 => Int
    _ => Double
  }
}

/// Enhanced type generator supporting more complex nested types
fn RandProgGenerator::gen_type(self : Self, allow_complex : Bool) -> Type {
  if self.current_nested_level >= self.max_nested_level ||
    !allow_complex ||
    self.current_depth >= self.max_depth - 1 {
    return self.gen_basic_type()
  }

  // Use weighted selection, basic types have higher weight to ensure compilable programs
  let choices = [
    (0, 80), // Basic types - significantly increased weight
    (1, 10), // Array types - reduced weight  
    (2, 8), // Tuple types - reduced weight
    (3, 1), // Custom struct types - greatly reduced weight
    (4, 1),
  ] // Custom enum types - greatly reduced weight
  match self.weighted_choice(choices) {
    Some(0) => self.gen_basic_type()
    Some(1) => self.gen_array_type()
    Some(2) => self.gen_tuple_type()
    Some(3) => self.gen_custom_struct_type()
    Some(4) => self.gen_custom_enum_type()
    _ => self.gen_basic_type()
  }
}

/// Generate array type
fn RandProgGenerator::gen_array_type(self : Self) -> Type {
  self.current_nested_level = self.current_nested_level + 1
  let element_type = self.gen_type(true)
  self.current_nested_level = self.current_nested_level - 1
  Array(element_type)
}

/// Generate tuple type
fn RandProgGenerator::gen_tuple_type(self : Self) -> Type {
  let size = self.rand_int(self.max_tuple_size - 1) + 2 // At least 2 elements
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

/// Generate custom struct type
fn RandProgGenerator::gen_custom_struct_type(self : Self) -> Type {
  // Randomly select from defined structs
  let available_structs = Array::new()
  self.struct_defs.each(fn(name, _) { available_structs.push(name) })
  match self.choice(available_structs) {
    Some(struct_name) =>
      match self.struct_defs.get(struct_name) {
        Some(struct_def) => struct_def.type_of()
        None => self.gen_basic_type() // Fallback
      }
    None => self.gen_basic_type() // If no structs available, return basic type
  }
}

/// Generate custom enum type
fn RandProgGenerator::gen_custom_enum_type(self : Self) -> Type {
  // Randomly select from defined enums
  let available_enums = Array::new()
  self.enum_defs.each(fn(name, _) { available_enums.push(name) })
  match self.choice(available_enums) {
    Some(enum_name) =>
      match self.enum_defs.get(enum_name) {
        Some(enum_def) => enum_def.type_of()
        None => self.gen_basic_type() // Fallback
      }
    None => self.gen_basic_type() // If no enums available, return basic type
  }
}

/// Find all variables of specified type
fn RandProgGenerator::find_all_vars_of_type(
  self : Self,
  target_type : Type,
) -> Array[Ident] {
  self.var_env.collect_vars_of_type(target_type)
}

/// Mark mutable variable as used (remove from unused list)
fn RandProgGenerator::mark_mutable_var_used(
  self : Self,
  var_name : Ident,
) -> Unit {
  // Create new Map to replace removal operation
  let new_map = Map::new()
  self.unused_mutable_vars.each(fn(name, ty) {
    if name != var_name {
      new_map.set(name, ty)
    }
  })
  self.unused_mutable_vars = new_map
}

/// Record variable definition
fn RandProgGenerator::record_var_definition(
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
    new_map.set(var_name, false) // Initially mark as unused
    new_map
  }
}

/// Mark variable as used
fn RandProgGenerator::mark_var_used(self : Self, var_name : Ident) -> Unit {
  self.used_vars = {
    let new_map = Map::new()
    self.used_vars.each(fn(k, v) { new_map.set(k, v) })
    new_map.set(var_name, true)
    new_map
  }
}

/// Safely generate forced assignment statements for unused mutable variables
fn RandProgGenerator::gen_safe_force_assign_unused_mutables(
  self : Self,
) -> Array[Stmt] {
  let assignments = Array::new()
  self.unused_mutable_vars.each(fn(var_name, var_type) {
    // Only generate assignment if variable is actually visible in current environment
    match self.var_env.get(var_name) {
      Some(_) => {
        let new_value = self.gen_expr(var_type, true)
        let assign_stmt = Assign(LeftValue::Ident(var_name), new_value)
        assignments.push(assign_stmt)
      }
      None => () // Variable not visible, skip
    }
  })

  // Clear unused list
  self.unused_mutable_vars = Map::new()
  assignments
}

/// Generate complex literal expressions
fn RandProgGenerator::gen_literal_expr(self : Self, ty : Type) -> Expr {
  match ty {
    Unit => IfLevelExpr(ApplyExpr(ValueExpr(UnitExpr)))
    Bool => IfLevelExpr(ApplyExpr(ValueExpr(BoolExpr(self.rand_bool()))))
    Int => {
      // Generate more diverse integers
      let value = match self.rand_int(4) {
        0 => self.rand_int(10) // Small integers 0-9
        1 => self.rand_int(1000) // Medium integers 0-999
        2 => self.rand_int(100000) // Large integers 0-99999
        _ => self.rand_int(2) * 2 - 1 // -1 or 1
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
    _ => IfLevelExpr(ApplyExpr(ValueExpr(UnitExpr))) // For other types, return Unit
  }
}

/// Generate struct constructor expression
fn RandProgGenerator::gen_struct_construct_expr(
  self : Self,
  name : Upper,
  fields : Array[(Ident, Type)],
) -> Expr {
  let field_assignments = Array::new()
  for field in fields {
    let (field_name, field_type) = field
    let field_value = self.gen_expr(field_type, true) // Use simple expressions for field values
    field_assignments.push((field_name, field_value))
  }
  IfLevelExpr(ApplyExpr(ValueExpr(StructConstruct(name, field_assignments))))
}

/// Generate enum constructor expression
fn RandProgGenerator::gen_enum_construct_expr(
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
    None => IfLevelExpr(ApplyExpr(ValueExpr(UnitExpr))) // If no variants, return Unit
  }
}

/// Generate variable reference expression - ensure referenced variables actually exist in current environment
fn RandProgGenerator::gen_var_expr(self : Self, ty : Type) -> Expr? {
  // First find variables of specified type in current environment
  let available_vars = self.var_env.collect_vars_of_type(ty)
  if available_vars.is_empty() {
    return None
  }

  // Randomly select from available variables
  match self.choice(available_vars) {
    Some(v) =>
      // Verify variable actually exists
      match self.var_env.get(v) {
        Some(_) => {
          // Mark variable as used
          self.mark_var_used(v)

          // If mutable variable, mark as used
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

/// Generate complex array expressions
fn RandProgGenerator::gen_array_expr(self : Self, elem_type : Type) -> Expr {
  match self.rand_int(2) {
    0 => {
      // Array::make(size, init_value)
      let size_val = self.rand_int(self.max_array_size) + 1
      let size_expr = IfLevelExpr(ApplyExpr(ValueExpr(IntExpr(size_val))))
      let init_expr = self.gen_expr(elem_type, true)
      IfLevelExpr(ApplyExpr(ValueExpr(ArrayMake(size_expr, init_expr))))
    }
    _ => {
      // [elem1, elem2, ...] - ensure at least one element
      let size = self.rand_int(self.max_array_size) + 1 // At least 1 element
      let elements = Array::new()
      for i = 0; i < size; i = i + 1 {
        elements.push(self.gen_expr(elem_type, true))
      }
      IfLevelExpr(ApplyExpr(ValueExpr(ArrayExpr(elements))))
    }
  }
}

/// Generate complex tuple expressions
fn RandProgGenerator::gen_tuple_expr(
  self : Self,
  elem_types : Array[Type],
) -> Expr {
  let elements = Array::new()
  for elem_type in elem_types {
    elements.push(self.gen_expr(elem_type, true))
  }
  IfLevelExpr(ApplyExpr(ValueExpr(TupleExpr(elements))))
}

/// Generate function call expression - ensure only calling actually existing and accessible functions
fn RandProgGenerator::gen_function_call_expr(
  self : Self,
  expected_return_type : Type,
) -> Expr? {
  // Find functions with matching return type (including top-level and local functions in current scope)
  let available_funcs = Array::new()

  // Find function type variables in current environment (local functions) - only check currently accessible ones
  self.var_env.map.each(fn(var_name, var_type) {
    match var_type {
      Func(param_types, return_type) =>
        if return_type == expected_return_type {
          // Verify function actually exists in current environment
          match self.var_env.get(var_name) {
            Some(_) => available_funcs.push((var_name, param_types))
            None => ()
          }
        }
      _ => ()
    }
  })

  // Only consider top-level functions when there are few available functions, to avoid complexity
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
        args.push(self.gen_expr(param_type, true)) // Use simple expressions
      }
      let func_apply = ApplyExpr::ValueExpr(ValueExpr::IdentExpr(func_name))
      Some(IfLevelExpr(ApplyExpr(Call(func_apply, args))))
    }
    None => None
  }
}

/// Generate array access expression
fn RandProgGenerator::gen_array_access_expr(
  self : Self,
  elem_type : Type,
) -> Expr? {
  // Find array type variables
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

/// Generate dot access expression (struct field access)
fn RandProgGenerator::gen_dot_access_expr(
  self : Self,
  expected_type : Type,
) -> Expr? {
  // Find struct variables with matching field types
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

/// Enhanced expression generator supporting more complex expression structures
pub fn RandProgGenerator::gen_expr(self : Self, ty : Type, simple : Bool) -> Expr {
  if self.current_depth >= self.max_depth || simple {
    // At max depth or when simple expressions required, prioritize literals or variable references
    let choices = [
      (0, 60), // Literals - significantly increased weight
      (1, 35), // Variable references - increased weight
      (2, 3), // Function calls - significantly reduced weight
      (3, 2),
    ] // Field access - significantly reduced weight
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

/// Generate complex Unit expressions
fn RandProgGenerator::gen_complex_unit_expr(self : Self) -> Expr {
  let choices = [
    (0, 40), // Literal Unit
    (1, 20), // Variable reference
    (2, 15), // Function call
    (3, 15), // If expression
    (4, 10),
  ] // Expression statement
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
      // Generate a compound expression containing Unit expression
      let inner_expr = self.gen_expr(Unit, true)
      IfLevelExpr(ApplyExpr(ValueExpr(GroupExpr(inner_expr))))
    }
    _ => self.gen_literal_expr(Unit)
  }
}

/// Generate complex Bool expressions
fn RandProgGenerator::gen_complex_bool_expr(self : Self) -> Expr {
  let choices = [
    (0, 20), // Literal Bool
    (1, 18), // Variable reference
    (2, 18), // Comparison expression
    (3, 15), // Logical expression
    (4, 12), // If expression
    (5, 10), // Function call
    (6, 7),
  ] // Negation expression
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

/// Generate complex Int expressions
fn RandProgGenerator::gen_complex_int_expr(self : Self) -> Expr {
  let choices = [
    (0, 35), // Literal Int
    (1, 30), // Variable reference
    (2, 15), // Arithmetic expression
    (3, 8), // If expression
    (4, 5), // Function call
    (5, 4), // Match expression
    (6, 2), // Array access
    (7, 1), // Field access
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

/// Generate complex Double expressions
fn RandProgGenerator::gen_complex_double_expr(self : Self) -> Expr {
  let choices = [
    (0, 25), // Literal Double
    (1, 20), // Variable reference
    (2, 20), // Arithmetic expression
    (3, 12), // If expression
    (4, 10), // Function call
    (5, 8), // Match expression
    (6, 3), // Array access
    (7, 2), // Field access
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

/// Generate complex Array expressions
fn RandProgGenerator::gen_complex_array_expr(
  self : Self,
  elem_type : Type,
) -> Expr {
  let choices = [
    (0, 30), // Array literal
    (1, 25), // Variable reference
    (2, 20), // Array::make
    (3, 15), // Function call
    (4, 10),
  ] // Field access
  match self.weighted_choice(choices) {
    Some(0) => self.gen_array_expr(elem_type)
    Some(1) =>
      match self.gen_var_expr(Array(elem_type)) {
        Some(expr) => expr
        None => self.gen_array_expr(elem_type)
      }
    Some(2) => {
      // Ensure Array::make first parameter is at least 1
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

/// Generate complex Tuple expressions
fn RandProgGenerator::gen_complex_tuple_expr(
  self : Self,
  elem_types : Array[Type],
) -> Expr {
  let choices = [
    (0, 40), // Tuple literal
    (1, 25), // Variable reference
    (2, 20), // Function call
    (3, 15),
  ] // Field access
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

/// Generate complex Struct expressions
fn RandProgGenerator::gen_complex_struct_expr(
  self : Self,
  name : Upper,
  fields : Array[(Ident, Type)],
) -> Expr {
  let choices = [
    (0, 40), // Struct constructor
    (1, 30), // Variable reference
    (2, 20), // Function call
    (3, 10),
  ] // Field access
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

/// Generate complex Enum expressions
fn RandProgGenerator::gen_complex_enum_expr(
  self : Self,
  name : Upper,
  variants : Array[(Upper, Array[Type])],
) -> Expr {
  let choices = [
    (0, 40), // Enum constructor
    (1, 30), // Variable reference
    (2, 20), // Function call
    (3, 10),
  ] // Field access
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

/// Generate arithmetic expressions (only for Int and Double)
fn RandProgGenerator::gen_arithmetic_expr(self : Self, ty : Type) -> Expr {
  // Always generate simple binary expressions to avoid infinite recursion
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

/// Generate comparison expressions (returns Bool)
fn RandProgGenerator::gen_comparison_expr(self : Self) -> Expr {
  let ty = if self.rand_bool() { Type::Int } else { Type::Double }

  // Generate simple comparison to avoid recursive complexity
  let left = self.gen_expr(ty, true)
  let right = self.gen_expr(ty, true)
  self.make_comparison(left, right)
}

/// Helper function to create comparison
fn RandProgGenerator::make_comparison(
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

/// Generate logical expressions (returns Bool)
fn RandProgGenerator::gen_logical_expr(self : Self) -> Expr {
  match self.rand_int(2) {
    0 => {
      // AND expression
      let left = self.gen_expr(Bool, true) // Use simple expressions
      let right = self.gen_expr(Bool, true)
      AndExpr(left, right)
    }
    _ => {
      // OR expression
      let left = self.gen_expr(Bool, true) // Use simple expressions
      let right = self.gen_expr(Bool, true)
      OrExpr(left, right)
    }
  }
}

/// Generate if expressions
pub fn RandProgGenerator::gen_if_expr(self : Self, expected_type : Type) -> Expr? {
  // Prevent overly deep nesting
  if self.current_depth >= self.max_depth - 1 {
    return None
  }

  // Generate condition expression (use current environment, don't introduce new variables)
  let cond = self.gen_expr(Bool, true)

  // Generate then block, important: don't create new sub-environment, since if expressions shouldn't introduce new scope
  let old_depth = self.current_depth
  self.current_depth = self.current_depth + 1

  // Generate then block - only use expressions, don't add new variable declarations
  let then_block = if expected_type == Unit {
    let block = BlockExpr::new(nested_level=self.current_depth - 1)
    let unit_expr = self.gen_expr(Unit, true)
    block.set_last_expr(unit_expr)
    block
  } else {
    let block = BlockExpr::new(nested_level=self.current_depth - 1)
    // Directly generate return expression, don't generate statements to avoid scope issues
    let return_expr = self.gen_expr(expected_type, true) // Use simple expressions
    block.set_last_expr(return_expr)
    block
  }

  // Generate else block - also don't create new scope
  let else_part = if self.rand_bool() == true &&
    self.current_depth < self.max_depth - 2 {
    // Generate else if, but be careful with recursion depth
    match self.gen_if_expr(expected_type) {
      Some(nested_if) =>
        match nested_if {
          IfLevelExpr(IfExpr(if_expr)) => Either::Left(if_expr)
          _ => {
            // If not an if expression, create simple else block
            let else_block = BlockExpr::new(nested_level=self.current_depth - 1)
            let else_expr = self.gen_expr(expected_type, true)
            else_block.set_last_expr(else_expr)
            Either::Right(else_block)
          }
        }
      None => {
        // Generate simple else block
        let else_block = BlockExpr::new(nested_level=self.current_depth - 1)
        let else_expr = self.gen_expr(expected_type, true)
        else_block.set_last_expr(else_expr)
        Either::Right(else_block)
      }
    }
  } else {
    // Generate simple else block
    let else_block = BlockExpr::new(nested_level=self.current_depth - 1)
    let else_expr = self.gen_expr(expected_type, true)
    else_block.set_last_expr(else_expr)
    Either::Right(else_block)
  }

  // Restore depth
  self.current_depth = old_depth
  let if_expr = IfExpr::{ cond, then_: then_block, else_: else_part }
  Some(IfLevelExpr(IfExpr(if_expr)))
}

/// Generate match expressions
pub fn RandProgGenerator::gen_match_expr(
  self : Self,
  expected_type : Type,
) -> Expr? {
  // Prevent overly deep nesting
  if self.current_depth >= self.max_depth - 1 {
    return None
  }

  // Select appropriate type for matching
  let match_type = match self.rand_int(4) {
    0 => Type::Bool
    1 => Type::Int
    2 => {
      // Try to use defined enum types
      let available_enums = Array::new()
      self.enum_defs.each(fn(_, enum_def) {
        available_enums.push(enum_def.type_of())
      })
      match self.choice(available_enums) {
        Some(enum_type) => enum_type
        None => Type::Bool // Fallback
      }
    }
    _ =>
      // Simple tuple type
      Type::Tuple([Type::Int, Type::Bool])
  }

  // Generate match expression
  let match_expr_val = self.gen_expr(match_type, true)

  // Create sub-environment
  let old_var_env = self.var_env
  let old_mut_env = self.mutable_vars
  let old_depth = self.current_depth
  self.var_env = old_var_env.sub_env()
  self.mutable_vars = old_mut_env.sub_env()
  self.current_depth = self.current_depth + 1

  // Generate match arms
  let arms = self.gen_match_arms(match_type, expected_type)

  // Restore environment
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

/// Generate match arms
fn RandProgGenerator::gen_match_arms(
  self : Self,
  match_type : Type,
  result_type : Type,
) -> Array[(Pattern, Expr)] {
  let arms = Array::new()
  match match_type {
    Bool => {
      // Boolean type matching
      let true_expr = self.gen_expr(result_type, true)
      let false_expr = self.gen_expr(result_type, true)
      arms.push((Pattern::Bool(true), true_expr))
      arms.push((Pattern::Bool(false), false_expr))
    }
    Int => {
      // Integer type matching
      let arm_count = self.rand_int(3) + 2 // 2-4 arms
      for i = 0; i < arm_count - 1; i = i + 1 {
        let pattern_val = self.rand_int(10)
        let arm_expr = self.gen_expr(result_type, true)
        arms.push((Pattern::Number(pattern_val), arm_expr))
      }
      // Add wildcard arm
      let wildcard_expr = self.gen_expr(result_type, true)
      arms.push((Pattern::WildCard, wildcard_expr))
    }
    Enum(enum_name, variants) =>
      // Enum type matching
      for variant in variants {
        let (variant_name, variant_types) = variant
        let patterns = Array::new()
        for variant_type in variant_types {
          // Generate simple patterns
          match variant_type {
            Int => patterns.push(Pattern::WildCard) // Simplify to wildcards
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
      // Tuple type matching
      let patterns = Array::new()
      for elem_type in elem_types {
        // Generate simple patterns
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

      // Add wildcard arm
      let wildcard_expr = self.gen_expr(result_type, true)
      arms.push((Pattern::WildCard, wildcard_expr))
    }
    _ => {
      // Other types use wildcards
      let wildcard_expr = self.gen_expr(result_type, true)
      arms.push((Pattern::WildCard, wildcard_expr))
    }
  }
  arms
}

/// Enhanced statement generator supporting more statement types
pub fn RandProgGenerator::gen_stmt(self : Self) -> Stmt {
  // Adjust statement complexity based on depth and line count target
  if self.current_depth >= self.max_depth - 1 {
    // At greater depth, generate simple statements
    let choices = [
      (0, 50), // Let statements - increased weight
      (1, 30), // Let mut statements - increased weight
      (2, 15), // Assignment statements
      (3, 5),
    ] // Expression statements - significantly reduced weight
    match self.weighted_choice(choices) {
      Some(0) => self.gen_let_stmt()
      Some(1) => self.gen_expr_stmt()
      Some(2) => self.gen_let_mut_stmt()
      Some(3) => self.gen_assign_stmt()
      _ => self.gen_let_stmt()
    }
  } else {
    // At shallower depth, can generate more complex statements, ensure mutable variables get used
    let choices = [
      (0, 35), // Let statements - moderate weight
      (1, 20), // Let mut statements - reduced weight to avoid too many unused mutable variables
      (2, 25), // Assignment statements - increased weight to ensure mutable variables are used
      (3, 8), // Expression statements - reduced weight
      (4, 5), // While loops - normal weight
      (5, 3), // Tuple destructuring let - reduced weight
      (6, 1), // Return statements - reduced weight
      (7, 1), // Local function definitions - greatly reduced weight
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

/// Generate let statement
pub fn RandProgGenerator::gen_let_stmt(self : Self) -> Stmt {
  let var_name = self.gen_ident()
  let var_type = self.gen_type(true)
  let init_expr = self.gen_expr(var_type, false)

  // Add variable to environment
  self.var_env.set(var_name, var_type)

  // Record variable definition
  self.record_var_definition(var_name, var_type)

  // Generate type annotations more frequently to improve code readability
  let type_annotation = if self.rand_int(10) < 7 {
    Some(var_type)
  } else {
    None
  }
  Let(var_name, type_annotation, init_expr)
}

/// Generate let mut statement
pub fn RandProgGenerator::gen_let_mut_stmt(self : Self) -> Stmt {
  let var_name = self.gen_ident()
  let var_type = self.gen_type(true)
  let init_expr = self.gen_expr(var_type, false)

  // Add variable to environment (including mutable variable environment)
  self.var_env.set(var_name, var_type)
  self.mutable_vars.set(var_name, var_type)

  // Record variable definition
  self.record_var_definition(var_name, var_type)

  // Add variable to unused mutable variables list
  self.unused_mutable_vars.set(var_name, var_type)
  let type_annotation = if self.rand_int(10) < 7 {
    Some(var_type)
  } else {
    None
  }
  LetMut(var_name, type_annotation, init_expr)
}

/// Generate tuple destructuring let statement
pub fn RandProgGenerator::gen_let_tuple_stmt(self : Self) -> Stmt {
  let tuple_size = self.rand_int(self.max_tuple_size - 1) + 2 // At least 2 elements
  let bindings = Array::new()
  let types = Array::new()

  // First determine all types
  for i = 0; i < tuple_size; i = i + 1 {
    let elem_type = self.gen_type(false)
    types.push(elem_type)
  }

  // Generate tuple expression (cannot reference variables that are about to be bound)
  let tuple_type = Type::Tuple(types)
  let tuple_expr = self.gen_expr(tuple_type, false)

  // Then generate bindings and add variables to environment (after generating expression)
  for i = 0; i < tuple_size; i = i + 1 {
    let elem_type = types[i]
    if self.rand_bool() == true {
      // Generate identifier binding
      let var_name = self.gen_ident()
      bindings.push(Binding::Ident(var_name))
      // Only add to environment after generating expression to avoid circular reference
      self.var_env.set(var_name, elem_type)
      // Record variable definition
      self.record_var_definition(var_name, elem_type)
    } else {
      // Generate wildcard binding
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

/// Generate assignment statement
pub fn RandProgGenerator::gen_assign_stmt(self : Self) -> Stmt {
  // Get all mutable variables (including those in parent environments)
  let mut_vars = Array::new()
  self.collect_mutable_vars_recursive(self.mutable_vars, mut_vars)
  if mut_vars.is_empty() {
    // If no mutable variables, generate a let statement
    return self.gen_let_stmt()
  }
  let (var_name, var_type) = self.choice(mut_vars).unwrap()

  // Verify variable actually exists in currently accessible environment
  match self.var_env.get(var_name) {
    Some(_) => {
      // Variable actually exists, can safely assign
      self.mark_mutable_var_used(var_name)

      // Generate appropriate left value based on variable type, but ensure simplicity to avoid complex access patterns
      let left_value = LeftValue::Ident(var_name) // Always use simple variable assignment
      let new_value = self.gen_expr(var_type, true) // Use simple expressions to avoid complexity
      Assign(left_value, new_value)
    }
    None =>
      // Variable doesn't exist, generate let statement instead
      self.gen_let_stmt()
  }
}

// Helper function: recursively collect mutable variables

/// Recursively collect mutable variables
fn RandProgGenerator::collect_mutable_vars_recursive(
  self : Self,
  env : Env[Ident, Type],
  result : Array[(Ident, Type)],
) -> Unit {
  env.map.each(fn(var_name, var_type) {
    // Only add variables that actually exist in current variable environment
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

/// Generate while statement
pub fn RandProgGenerator::gen_while_stmt(self : Self) -> Stmt {
  // Generate a while loop with definite termination condition
  // Use pattern: let mut counter = 0; while counter < limit { ... counter = counter + 1 }

  // Create sub-environment
  let old_var_env = self.var_env
  let old_mut_env = self.mutable_vars
  self.var_env = old_var_env.sub_env()
  self.mutable_vars = old_mut_env.sub_env()

  // Generate loop counter variable
  let counter_name = self.gen_ident()
  let counter_type = Int
  let initial_value = IfLevelExpr(ApplyExpr(ValueExpr(IntExpr(0)))) // Start from 0

  // Add counter to environment
  self.var_env.set(counter_name, counter_type)
  self.mutable_vars.set(counter_name, counter_type)

  // Generate loop limit
  let limit = self.rand_int(10) + 1 // 1-10 iterations
  let limit_expr = IfLevelExpr(ApplyExpr(ValueExpr(IntExpr(limit))))

  // Generate condition: counter < limit
  let counter_ref = IfLevelExpr(ApplyExpr(ValueExpr(IdentExpr(counter_name))))
  let cond = CmpExpr(Lt, counter_ref, limit_expr)

  // Generate loop body and add counter increment at the end
  let body = self.gen_block_expr(Unit)

  // Add counter increment statement in loop body
  let increment_expr = AddExpr(
    IfLevelExpr(ApplyExpr(ValueExpr(IdentExpr(counter_name)))),
    IfLevelExpr(ApplyExpr(ValueExpr(IntExpr(1)))),
  )
  let increment_stmt = Assign(LeftValue::Ident(counter_name), increment_expr)
  body.push(increment_stmt)

  // Restore environment
  self.var_env = old_var_env
  self.mutable_vars = old_mut_env

  // Create a compound statement block containing let mut and while
  let wrapper_block = BlockExpr::new(nested_level=self.current_depth)

  // Add let mut statement
  let let_mut_stmt = LetMut(counter_name, Some(counter_type), initial_value)
  wrapper_block.push(let_mut_stmt)

  // Add while statement
  let while_stmt = While(cond, body)
  wrapper_block.push(while_stmt)

  // Return expression statement wrapping this block
  ExprStmt(IfLevelExpr(ApplyExpr(ValueExpr(BlockExpr(wrapper_block)))))
}

/// Generate return statement
pub fn RandProgGenerator::gen_return_stmt(self : Self) -> Stmt {
  let return_expr = self.gen_expr(self.current_return_type, false)
  Return(return_expr)
}

/// Generate local function definition statement
pub fn RandProgGenerator::gen_local_func_def_stmt(self : Self) -> Stmt {
  // Reduce local function generation as they easily cause scope issues
  if self.current_depth >= self.max_depth - 2 {
    return self.gen_let_stmt() // Don't generate local functions in deep nesting
  }
  let param_count = self.rand_int(3) // 0-2 parameters, reduce complexity
  let params = Array::new()

  // Generate function name and type
  let func_name = self.gen_ident()
  let ret_ty = self.gen_basic_type() // Use basic types to ensure safety

  // Generate parameter types
  let param_types = Array::new()
  for i = 0; i < param_count; i = i + 1 {
    let param_type = self.gen_basic_type() // Use basic types to ensure safety
    param_types.push(param_type)
  }

  // Add function type to current environment (before generating function body)
  let func_type = Type::Func(param_types, ret_ty)
  self.var_env.set(func_name, func_type)

  // Create completely independent function scope, don't inherit external variables
  let old_var_env = self.var_env
  let old_mut_env = self.mutable_vars
  self.var_env = Env::new() // Brand new environment, don't inherit any external variables
  self.mutable_vars = Env::new()

  // Generate parameters and add to function scope
  for i = 0; i < param_count; i = i + 1 {
    let param_name = self.gen_ident()
    let param_type = param_types[i]
    params.push((param_name, Some(param_type)))

    // Add parameters to function internal environment
    self.var_env.set(param_name, param_type)
    // Record parameters as defined variables
    self.record_var_definition(param_name, param_type)
    // Parameters are used when defined
    self.mark_var_used(param_name)
  }

  // Generate function body - use variable tracking
  let old_return_type = self.current_return_type
  self.current_return_type = ret_ty
  let body = self.gen_block_expr_with_usage_tracking(ret_ty)

  // Restore environment
  self.current_return_type = old_return_type
  self.var_env = old_var_env
  self.mutable_vars = old_mut_env
  LocalFuncDef(func_name, params, Some(ret_ty), body)
}

/// Generate expression statement
pub fn RandProgGenerator::gen_expr_stmt(self : Self) -> Stmt {
  // Expression statements should only generate Unit type or if/match expressions, avoid cases requiring let _ = prefix
  let expr = match self.rand_int(3) {
    0 =>
      // Generate Unit expression
      self.gen_expr(Unit, true)
    1 =>
      // Generate if expression (doesn't need let _ = prefix)
      match self.gen_if_expr(Unit) {
        Some(expr) => expr
        None => self.gen_expr(Unit, true)
      }
    _ =>
      // Generate match expression (doesn't need let _ = prefix)
      match self.gen_match_expr(Unit) {
        Some(expr) => expr
        None => self.gen_expr(Unit, true)
      }
  }
  ExprStmt(expr)
}

/// Generate constructor and usage statements for unused types
fn RandProgGenerator::gen_force_use_unused_types(
  self : Self,
  block : BlockExpr,
) -> Unit {
  // Force use of all struct types
  self.struct_defs.each(fn(struct_name, struct_def) {
    if true { // Use all structs
      // Generate struct constructor expression and assign to variable
      let var_name = self.gen_ident()
      let struct_expr = self.gen_struct_construct_expr(
        struct_name,
        struct_def.fields,
      )
      let let_stmt = Let(var_name, Some(struct_def.type_of()), struct_expr)
      block.push(let_stmt)

      // Access all struct fields
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
            // Generate println call for Int type
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
            // Generate simple expression statement for other types
            block.push(ExprStmt(field_access))
        }
      }

      // Use entire struct
      let usage_expr = IfLevelExpr(ApplyExpr(ValueExpr(IdentExpr(var_name))))
      block.push(ExprStmt(usage_expr))
    }
  })

  // Force use of all enum types through match expressions
  self.enum_defs.each(fn(enum_name, enum_def) {
    if true { // Use all enums
      // Generate enum constructor expression and assign to variable
      let var_name = self.gen_ident()
      let enum_expr = self.gen_enum_construct_expr(enum_name, enum_def.variants)
      let let_stmt = Let(var_name, Some(enum_def.type_of()), enum_expr)
      block.push(let_stmt)

      // Generate match expression using all variants
      if !enum_def.variants.is_empty() {
        let match_arms = Array::new()
        for variant in enum_def.variants {
          let (variant_name, variant_types) = variant
          let patterns = Array::new()
          for _ in variant_types {
            patterns.push(Pattern::WildCard) // Use wildcard patterns
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

      // Use entire enum
      let usage_expr = IfLevelExpr(ApplyExpr(ValueExpr(IdentExpr(var_name))))
      block.push(ExprStmt(usage_expr))
    }
  })

  // Force call all functions
  self.func_defs.each(fn(func_name, func_def) {
    if true { // Call all functions
      // Generate function call
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
        // Unit return type, directly as expression statement
        block.push(ExprStmt(func_call))
      } else {
        // Non-Unit return type, assign to variable then use
        let var_name = self.gen_ident()
        let let_stmt = Let(var_name, Some(func_def.ret_ty), func_call)
        block.push(let_stmt)

        // Decide how to use based on return type
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

  // Force use all top-level variables
  self.var_env.map.each(fn(var_name, var_type) {
    // Check if this variable is in global scope (not function parameter)
    if self.current_depth <= 1 { // At top level of main function
      match var_type {
        Int => {
          // Generate println call for Int type
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
          // Generate simple expression statement for other types
          let var_expr = IfLevelExpr(ApplyExpr(ValueExpr(IdentExpr(var_name))))
          block.push(ExprStmt(var_expr))
        }
      }
    }
  })
}

/// Generate code block and handle unused variables at the end
fn RandProgGenerator::gen_block_expr_with_usage_tracking(
  self : Self,
  return_type : Type,
) -> BlockExpr {
  // Save current defined variables state
  let scope_start_vars = Map::new()
  self.all_defined_vars.each(fn(k, v) { scope_start_vars.set(k, v) })

  // Generate normal code block
  let block = self.gen_block_expr(return_type)

  // Generate usage statements for variables defined but unused in current scope
  self.all_defined_vars.each(fn(var_name, var_type) {
    // Check if this variable is newly defined in current scope
    match scope_start_vars.get(var_name) {
      None =>
        // This is a newly defined variable, check if unused
        match self.used_vars.get(var_name) {
          Some(false) =>
            // Variable unused, generate usage statement
            match self.var_env.get(var_name) {
              Some(_) => {
                // Variable visible in current environment, generate usage statement
                match var_type {
                  Int => {
                    // Generate println call for Int type
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
                    // Generate simple expression statement for other types
                    let var_expr = IfLevelExpr(
                      ApplyExpr(ValueExpr(IdentExpr(var_name))),
                    )
                    block.push(ExprStmt(var_expr))
                  }
                }
                // Mark variable as used
                self.mark_var_used(var_name)
              }
              None => () // Variable not visible, skip
            }
          _ => () // Variable already used or undefined, skip
        }
      Some(_) => () // Variable not defined in current scope, skip
    }
  })

  // Additional enhancement: for any variable visible in current environment but marked as unused, force use
  self.var_env.map.each(fn(var_name, var_type) {
    match self.used_vars.get(var_name) {
      Some(false) => {
        // Unused variable, force generate usage statement
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

  // Special handling for mutable variables: ensure all mutable variables' mutability is used
  self.mutable_vars.map.each(fn(var_name, var_type) {
    match self.var_env.get(var_name) {
      Some(_) => {
        // Mutable variable visible in current environment, generate multiple assignments to use its mutability
        let left_value = LeftValue::Ident(var_name)

        // First assignment
        let new_value1 = self.gen_expr(var_type, true) // Generate simple expression
        block.push(Assign(left_value, new_value1))

        // Second assignment, ensure mutability is used
        let new_value2 = self.gen_expr(var_type, true) // Generate another simple expression
        block.push(Assign(left_value, new_value2))
        self.mark_mutable_var_used(var_name)
      }
      None => () // Variable not visible in current environment, skip
    }
  })

  // Clear unused list
  self.unused_mutable_vars = Map::new()

  // Force use some top-level definitions in main function
  if self.current_depth <= 1 { // Only generate in top-level block
    self.gen_force_use_unused_types(block)
  }
  block
}

/// Enhanced code block generator supporting more complex code structures
pub fn RandProgGenerator::gen_block_expr(
  self : Self,
  return_type : Type,
) -> BlockExpr {
  let block = BlockExpr::new(nested_level=self.current_depth)

  // Check recursion depth to avoid infinite recursion
  if self.current_depth >= self.max_depth {
    // At max depth, only generate simple return expression
    if return_type != Unit {
      let last_expr = self.gen_literal_expr(return_type)
      block.set_last_expr(last_expr)
    }
    return block
  }

  // Create sub-environment
  let old_var_env = self.var_env
  let old_mut_env = self.mutable_vars
  let old_depth = self.current_depth
  self.var_env = old_var_env.sub_env()
  self.mutable_vars = old_mut_env.sub_env()
  self.current_depth = self.current_depth + 1

  // Dynamically adjust statement count based on target line count
  let base_stmt_count = if self.current_lines < self.total_lines_target / 2 {
    // If current lines less than half of target, generate more statements
    self.rand_int(25) + 15 // 15-39 statements
  } else if self.current_lines < self.total_lines_target * 3 / 4 {
    // If approaching target, moderate amount
    self.rand_int(15) + 10 // 10-24 statements
  } else {
    // If approaching or exceeding target, fewer statements
    self.rand_int(8) + 5 // 5-12 statements
  }
  let stmt_count = if self.current_depth <= 2 {
    base_stmt_count
  } else {
    base_stmt_count / (self.current_depth - 1) // Greater depth, fewer statements
  }
  for i = 0; i < stmt_count; i = i + 1 {
    let stmt = self.gen_stmt()
    block.push(stmt)
    self.current_lines = self.current_lines + self.estimate_stmt_lines(stmt)

    // If target line count reached, stop generating more statements
    if self.current_lines >= self.total_lines_target {
      break
    }
  }

  // Before function ends, generate forced assignments for all unused mutable variables
  // But only do this when these variables actually exist in current environment
  let force_assignments = self.gen_safe_force_assign_unused_mutables()
  for assign_stmt in force_assignments {
    block.push(assign_stmt)
    self.current_lines = self.current_lines + 1
  }

  // Generate return expression (if needed)
  if return_type != Unit {
    let last_expr = if self.rand_bool() == true {
      self.gen_literal_expr(return_type) // Sometimes generate literals
    } else {
      self.gen_expr(return_type, false) // Sometimes generate complex expressions
    }
    block.set_last_expr(last_expr)
    self.current_lines = self.current_lines + 1
  }

  // Restore environment
  self.var_env = old_var_env
  self.mutable_vars = old_mut_env
  self.current_depth = old_depth
  block
}

/// Estimate statement line count (for controlling total line count)
fn RandProgGenerator::estimate_stmt_lines(self : Self, stmt : Stmt) -> Int {
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

/// Estimate code block line count
fn RandProgGenerator::estimate_block_lines(self : Self, block : BlockExpr) -> Int {
  let lines = 2 // { and }
  let stmt_lines = block.stmts.fold(init=0, fn(acc, stmt) {
    acc + self.estimate_stmt_lines(stmt)
  })
  let expr_lines = match block.last_expr {
    Some(_) => 1
    None => 0
  }
  lines + stmt_lines + expr_lines
}

/// Enhanced top-level definition generator
pub fn RandProgGenerator::gen_top_let(self : Self) -> TopLet {
  let name = self.gen_ident()
  let ty = self.gen_type(true)
  let value = self.gen_expr(ty, false)

  // Add global variable to environment
  self.var_env.set(name, ty)

  // Always generate type annotation to ensure type safety
  let type_annotation = Some(ty)
  { name, ty: type_annotation, value }
}

/// Generate struct definition
pub fn RandProgGenerator::gen_struct_def(self : Self) -> StructDef {
  let name = self.gen_upper()
  let field_count = self.rand_int(5) + 1 // 1-5 fields, reduce complexity
  let fields = Array::new()
  for i = 0; i < field_count; i = i + 1 {
    let field_name = self.gen_ident()
    // For struct fields, mainly use basic types or simple composite types
    let field_type = if self.rand_int(10) < 7 {
      self.gen_basic_type() // 70% probability use basic types
    } else {
      // 30% probability use simple composite types
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

  // Register struct definition
  self.struct_defs.set(name, struct_def)

  // Add struct type to type environment
  self.type_env.set(name, struct_def.type_of())
  struct_def
}

/// Generate enum definition
pub fn RandProgGenerator::gen_enum_def(self : Self) -> EnumDef {
  let name = self.gen_upper()
  let variant_count = self.rand_int(4) + 1 // 1-4 variants, reduce complexity
  let variants = Array::new()
  for i = 0; i < variant_count; i = i + 1 {
    let variant_name = self.gen_upper()
    let field_count = self.rand_int(4) // 0-3 fields, reduce complexity
    let variant_fields = Array::new()
    for j = 0; j < field_count; j = j + 1 {
      // For enum variants, mainly use basic types
      let field_type = if self.rand_int(10) < 8 {
        self.gen_basic_type() // 80% probability use basic types
      } else {
        Array(self.gen_basic_type()) // 20% probability use basic type arrays
      }
      variant_fields.push(field_type)
    }
    variants.push((variant_name, variant_fields))
  }
  let enum_def = { generic_param: None, name, variants }

  // Register enum definition
  self.enum_defs.set(name, enum_def)

  // Add enum type to type environment
  self.type_env.set(name, enum_def.type_of())
  enum_def
}

/// Generate top-level function definition
pub fn RandProgGenerator::gen_top_func_def(self : Self) -> TopFuncDef {
  let name = self.gen_ident()
  let param_count = self.rand_int(self.max_func_params)
  let params = Array::new()

  // Create new environment scope
  let old_var_env = self.var_env
  let old_mut_env = self.mutable_vars
  self.var_env = old_var_env.sub_env()
  self.mutable_vars = old_mut_env.sub_env()

  // Generate parameters, use basic types to ensure type safety
  for i = 0; i < param_count; i = i + 1 {
    let param_name = self.gen_ident()
    let param_type = self.gen_basic_type() // Use basic types to ensure safety
    params.push((param_name, param_type))

    // Add parameters to environment
    self.var_env.set(param_name, param_type)
    // Record parameters as defined variables
    self.record_var_definition(param_name, param_type)
    // Parameters are used when defined
    self.mark_var_used(param_name)
  }

  // Generate return type and function body
  let ret_ty = self.gen_basic_type() // Use basic types to ensure safety
  let old_return_type = self.current_return_type
  self.current_return_type = ret_ty

  // Use variable tracking code block generation
  let body = self.gen_block_expr_with_usage_tracking(ret_ty)

  // Restore environment and return type
  self.current_return_type = old_return_type
  self.var_env = old_var_env
  self.mutable_vars = old_mut_env
  let func_def = { generic_param: None, name, params, ret_ty, body }

  // Register function definition
  self.func_defs.set(name, func_def)
  func_def
}

/// Generate main function (without parameters and generic parameters)
pub fn RandProgGenerator::gen_main_func(self : Self) -> TopFuncDef {
  let name : Ident = Ident("main")
  let params : Array[(Ident, Type)] = [] // main function has no parameters
  let ret_ty = Unit // main function returns Unit

  // Create new environment scope
  let old_var_env = self.var_env
  let old_mut_env = self.mutable_vars
  self.var_env = old_var_env.sub_env()
  self.mutable_vars = old_mut_env.sub_env()
  let old_return_type = self.current_return_type
  self.current_return_type = Unit

  // Generate a larger function body with more statements
  let body = self.gen_block_expr_with_usage_tracking(Unit)

  // Restore environment and return type
  self.current_return_type = old_return_type
  self.var_env = old_var_env
  self.mutable_vars = old_mut_env
  { generic_param: None, name, params, ret_ty, body }
}

/// Generate top-level declaration
pub fn RandProgGenerator::gen_top_decl(self : Self) -> TopDecl {
  let choices = [
    (0, 30), // Top-level let
    (1, 25), // Struct definition  
    (2, 25), // Function definition
    (3, 20),
  ] // Enum definition
  match self.weighted_choice(choices) {
    Some(0) => TopDecl::TopLet(self.gen_top_let())
    Some(1) => TopDecl::StructDef(self.gen_struct_def())
    Some(2) => TopDecl::TopFuncDef(self.gen_top_func_def())
    Some(3) => TopDecl::EnumDef(self.gen_enum_def())
    _ => TopDecl::TopLet(self.gen_top_let())
  }
}

/// Program generator ensuring main function inclusion and reasonable program length
pub fn RandProgGenerator::gen_program(self : Self) -> Program {
  let top_decls = Array::new()

  // First generate appropriate amount of basic type definitions
  let struct_count = self.rand_int(5) + 3 // 3-7 structs
  for i = 0; i < struct_count; i = i + 1 {
    top_decls.push(TopDecl::StructDef(self.gen_struct_def()))
    self.current_lines = self.current_lines + 5
  }
  let enum_count = self.rand_int(4) + 2 // 2-5 enums
  for i = 0; i < enum_count; i = i + 1 {
    top_decls.push(TopDecl::EnumDef(self.gen_enum_def()))
    self.current_lines = self.current_lines + 6
  }

  // Generate appropriate amount of functions and top-level variables
  let decl_count = self.rand_int(40) + 40 // 40-79 declarations
  for i = 0; i < decl_count; i = i + 1 {
    let decl = self.gen_top_decl()
    top_decls.push(decl)

    // Estimate line count
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

  // Finally generate main function (required)
  let main_func = self.gen_main_func()
  top_decls.push(TopDecl::TopFuncDef(main_func))
  self.current_lines = self.current_lines +
    self.estimate_block_lines(main_func.body) +
    2
  { top_decls, }
}
