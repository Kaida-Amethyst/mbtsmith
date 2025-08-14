
fn main {
  let ast_gen = @ast.RandAstGenerator::new()
  //let ast_gen = @ast.RandAstGenerator::new(generic=Some("T"))

  for i in 0..<5 {
    println(ast_gen.gen_top_let())
  }

  for i in 0..<5 {
    println(ast_gen.gen_struct_def())
  }

  for i in 0..<5 {
    println(ast_gen.gen_enum_def())
  }
}
