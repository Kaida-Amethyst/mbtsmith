# MiniMbt-Smith

MiniMbt-Smith是一个检测MiniMoonbit编译器实现稳健性的工具，它可以根据[MiniMoonbit2025标准的语法](./MiniMoonbit.g4)，随机生成符合语法的MiniMoonbit代码。

本工具基于csmith的思路设计，专注于生成具有一定长度和复杂度的MiniMoonbit程序，用于测试MiniMoonbit编译器的稳健性。

## 安装与导入

在你的MoonBit项目目录中运行：

```bash
# 更新依赖
moon update

# 添加mbtsmith包
moon add Kaida-Amethyst/mbtsmith
```

然后在代码中使用：

```moonbit
fn main {
  let generator = @mbtsmith.RandProgGenerator::new()
  let prog = generator.gen_program()
  println(prog)
}
```

## API接口说明

### RandProgGenerator类

`RandProgGenerator`是核心的随机程序生成器类，提供了完整的程序生成功能。

#### 构造函数

```moonbit
pub fn RandProgGenerator::new(seed~:Int = 0) -> RandProgGenerator
```

- `seed`：可选的随机种子，用于生成可重复的随机程序。默认为0。

#### 主要生成方法

##### 1. 生成完整程序

```moonbit
pub fn RandProgGenerator::gen_program(self: Self) -> Program
```

生成一个完整的MiniMoonbit程序，包含：
- 多个结构体定义（3-7个）
- 多个枚举定义（2-5个）
- 多个顶层函数和变量声明（40-79个）
- 一个必需的main函数

##### 2. 生成顶层声明

```moonbit
pub fn RandProgGenerator::gen_top_decl(self: Self) -> TopDecl
pub fn RandProgGenerator::gen_top_let(self: Self) -> TopLet
pub fn RandProgGenerator::gen_top_func_def(self: Self) -> TopFuncDef
pub fn RandProgGenerator::gen_struct_def(self: Self) -> StructDef
pub fn RandProgGenerator::gen_enum_def(self: Self) -> EnumDef
pub fn RandProgGenerator::gen_main_func(self: Self) -> TopFuncDef
```

##### 3. 生成表达式

```moonbit
pub fn RandProgGenerator::gen_expr(self: Self, ty: Type, simple: Bool) -> Expr
pub fn RandProgGenerator::gen_if_expr(self: Self, expected_type: Type) -> Expr?
pub fn RandProgGenerator::gen_match_expr(self: Self, expected_type: Type) -> Expr?
```

- `ty`：期望的表达式类型
- `simple`：是否生成简单表达式（避免复杂嵌套）
- `expected_type`：if/match表达式的期望返回类型

##### 4. 生成语句

```moonbit
pub fn RandProgGenerator::gen_local_func_def_stmt(self: Self) -> Stmt
```

### AST语法树

项目提供了完整的AST语法树定义，所有Ast都实现了`Show` trait，可以直接转换为字符串输出：

#### 核心AST类型

- `Program`：完整程序
- `TopDecl`：顶层声明（函数、结构体、枚举、变量）
- `Expr`：表达式
- `Stmt`：语句
- `Type`：类型系统
- `Pattern`：模式匹配

#### 标识符类型

- `Ident`：普通标识符（变量名、函数名等）
- `Upper`：大写标识符（类型名、枚举变体名等）

## 使用示例

### 基本用法

```moonbit
fn main {
  // 创建生成器
  let generator = @mbtsmith.RandProgGenerator::new()
  
  // 生成完整程序
  let program = generator.gen_program()
  
  // 输出程序代码
  println(program)
}
```

### 使用指定种子

```moonbit
fn main {
  // 使用种子42创建生成器，相同的种子生成相同的程序，如果不给seed参数，默认为0。
  let generator = @mbtsmith.RandProgGenerator::new(seed=42)
  let program = generator.gen_program()
  println(program)
}
```

### 生成特定组件

```moonbit
fn main {
  let generator = @mbtsmith.RandProgGenerator::new()
  
  // 生成单个结构体定义
  let struct_def = generator.gen_struct_def()
  println("Struct: \{struct_def}")
  
  // 生成单个函数定义
  let func_def = generator.gen_top_func_def()
  println("Function: \{func_def}")
  
  // 生成Int类型的表达式
  let int_expr = generator.gen_expr(@mbtsmith.Type::Int, simple=true)
  println("Expression: \{int_expr}")
}
```

## 测试编译器

生成的程序可以用于测试MiniMoonbit编译器：

```bash
# 生成测试程序
moon run main > test_program.mbt

# 使用MoonBit检查语法正确性，--warn-list -A消除所有警告
moon check --warn-list -A test_program.mbt

# 使用你的MiniMoonbit编译器进行测试
your_compiler test_program.mbt
```

## 注意

`mbtsmith`当前只能生成满足语法和类型定义的minimoonbit程序，但在运行阶段可能会出现问题。

## MiniMoonbit2025标准语法

```antlr4
grammar MiniMoonBit;

prog: top_level* EOF;

// Top-level
// 
// Top level declarations should start at the beginning of the line, i.e.
// token.column == 0. Since this is non-context-free, it is not included in this
// backend-agnostic ANTLR grammar.
top_level: top_let_decl | toplevel_fn_decl | struct_decl | enum_decl;
top_let_decl:
	'let' IDENTIFIER (':' type)? '=' expr ';';
toplevel_fn_decl: (main_fn_decl | top_fn_decl);

// Function declarations
// 
// `fn main` does not accept parameters and return type
main_fn_decl: 'fn' 'main' fn_body;

top_fn_decl:
	'fn' ('[' UPPER_IDENTIFIER ']')? IDENTIFIER '(' param_list? ')' '->' type fn_body;
param_list: param (',' param)*;
param: IDENTIFIER type_annotation;
fn_body: '{' stmt* expr? '}';

struct_decl:
	'struct' UPPER_IDENTIFIER ('[' UPPER_IDENTIFIER ']')?  '{' struct_field_list? '}';
struct_field_list:
	struct_field (';' struct_field)* ';'?;
struct_field:
	IDENTIFIER type_annotation;

enum_decl:
 'enum' UPPER_IDENTIFIER ('[' UPPER_IDENTIFIER ']')? '{' enum_variant_list? '}';
enum_variant_list:
	enum_variant (';' enum_variant)* ';'?;
enum_variant:
  UPPER_IDENTIFIER ('(' enum_variant_field_list? ')')?;
enum_variant_field_list:
	type (',' type)*;

nontop_fn_decl:
	'fn' IDENTIFIER '(' nontop_param_list? ')' (
		'->' type
	)? fn_body;
nontop_param_list:
	nontop_param (',' nontop_param)*;
nontop_param: IDENTIFIER type_annotation?;

// Statements
stmt:
	let_tuple_stmt
	| let_mut_stmt
	| let_stmt
	| fn_decl_stmt
	| assign_stmt
	| while_stmt
	| return_stmt
	| expr_stmt;

binding: IDENTIFIER | WILDCARD ;

let_tuple_stmt:
	'let' '(' binding (',' binding)* ')' type_annotation? '=' expr ';' ;
let_mut_stmt:
	'let' 'mut' IDENTIFIER type_annotation? '=' expr ';';
let_stmt:
	'let' binding type_annotation? '=' expr ';';
type_annotation: COLON type;

fn_decl_stmt: nontop_fn_decl;

// x[y] = z;
assign_stmt: left_value '=' expr ';';

// while x { ... }
while_stmt: 'while' expr '{' stmt* '}';

return_stmt:
	'return' expr? ';' ;

expr_stmt: expr ';';

left_value:
  IDENTIFIER
  | left_value '.' IDENTIFIER
  | left_value '[' expr ']';

// Expressions, in order of precedence.
expr: // not associative
	or_level_expr;

or_level_expr: // left associative
	or_level_expr OR and_level_expr
	| and_level_expr;

and_level_expr: // left associative
	and_level_expr AND cmp_level_expr
	| cmp_level_expr;

cmp_level_expr: // not associative
	add_sub_level_expr CMP_OPERATOR add_sub_level_expr
	| add_sub_level_expr;

add_sub_level_expr: // left associative
	add_sub_level_expr '+' mul_div_level_expr
	| add_sub_level_expr '-' mul_div_level_expr
	| mul_div_level_expr;

mul_div_level_expr: // left associative
	mul_div_level_expr '*' if_level_expr
	| mul_div_level_expr '/' if_level_expr
	| mul_div_level_expr '%' if_level_expr
	| if_level_expr;

if_level_expr: get_or_apply_level_expr | if_expr | match_expr;
if_expr: 'if' expr block_expr ('else' (if_expr | block_expr))?;

match_expr: 'match' expr '{' match_arm_list '}';
match_arm_list:
	match_arm (';' match_arm)* ';'?;
match_arm:
 	pattern '=>' expr;

pattern:
  NUMBER
  | 'true'
  | 'false'
  | '(' pattern (',' pattern)* ')' // Tuple pattern
  | WILDCARD // Wildcard pattern
  | IDENTIFIER // Variable pattern
  | (UPPER_IDENTIFIER '::')? UPPER_IDENTIFIER ('(' pattern ( ',' pattern )* ')')?; // Enum variant pattern

get_or_apply_level_expr:
	value_expr (
		'[' expr ']'
		| '(' (expr (',' expr)*)? ')'
		| '.' IDENTIFIER
	)*;

// Value expressions
value_expr:
	array_make_expr
	| struct_construct_expr // eg: Point::{ x: 1, y: 2 }
	| enum_construct_expr // eg: Point(1, 2)
	| unit_expr
	| group_expr
	| tuple_expr
	| array_expr
	| bool_expr
	| identifier_expr
	| block_expr
	| neg_expr
	| floating_point_expr
	| int_expr
	| not_expr;
unit_expr: '(' ')'; // ()
group_expr: '(' expr ')'; // (x)
tuple_expr:
	'(' expr (',' expr)+ ')'; // (x, y); 1-tuple is not allowed
array_expr:
	'[' expr (',' expr)* ']'; // [x, y, z]
block_expr: '{' stmt* expr? '}'; // { blah; blah; }
bool_expr: 'true' | 'false';
neg_expr: '-' value_expr;
floating_point_expr: NUMBER '.' NUMBER?; // 1.0 | 1.
int_expr: NUMBER; // 1
not_expr: '!' expr ; // !x
array_make_expr:
	ARRAY '::' 'make' '(' expr ',' expr ')'; // Array::make(x, y)
struct_construct_expr:
	UPPER_IDENTIFIER '::' '{' struct_field_expr_list? '}'; // Point::{ x: 1, y: 2 }
struct_field_expr_list:
	struct_field_expr (',' struct_field_expr)*;
struct_field_expr:
	IDENTIFIER ':' expr; // x: 1
enum_construct_expr:
  (UPPER_IDENTIFIER '::')? UPPER_IDENTIFIER ('(' enum_construct_field_list? ')')?;
enum_construct_field_list:
	expr (',' expr)*; // Point(1, 2)
identifier_expr: IDENTIFIER;

// Types
type:
	'Unit'
	| 'Bool'
	| 'Int'
	| 'Double'
	| array_type
	| tuple_type
	| function_type
	| user_defined_type // User-defined type
	| generic_type; // Generic type, e.g. [T]
array_type: 'Array' '[' type ']';
tuple_type: '(' type (',' type)* ')'; // (Int, Bool)
function_type:
	'(' type (',' type)* ')' '->' type; // (Int, Bool) -> Int
generic_type: UPPER_IDENTIFIER '[' type ']'; // [T]
user_defined_type: UPPER_IDENTIFIER;

// Tokens

TRUE: 'true';
FALSE: 'false';
UNIT: 'Unit';
BOOL: 'Bool';
INT: 'Int';
DOUBLE: 'Double';
ARRAY: 'Array';
NOT: 'not';
IF: 'if';
ELSE: 'else';
FN: 'fn';
LET: 'let';
NUMBER: [0-9]+;
UPPER_IDENTIFIER: [A-Z][a-zA-Z0-9_]*;
WILDCARD: '_';
IDENTIFIER: [a-zA-Z_][a-zA-Z0-9_]*;
CMP_OPERATOR: '==' | '!=' | '>=' | '<=' | '<' | '>';
AND: '&&';
OR: '||';
DOT: '.';
ADD: '+';
SUB: '-';
MUL: '*';
DIV: '/';
ASSIGN: '=';
LPAREN: '(';
RPAREN: ')';
LBRACKET: '[';
RBRACKET: ']';
LCURLYBRACKET: '{';
RCURLYBRACKET: '}';
ARROW: '->';
COLON: ':';
SEMICOLON: ';';
COMMA: ',';
WS: [ \t\r\n]+ -> skip;
COMMENT: '//' ~[\r\n]* -> skip;
```

## 许可证

Apache-2.0

