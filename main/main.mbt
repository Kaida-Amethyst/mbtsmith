
fn main {
  let ast_gen = @ast.RandAstGenerator::new()
  let up = ast_gen.gen_upper()
  let id = ast_gen.gen_ident()

  println(up)
  println(id)
}
