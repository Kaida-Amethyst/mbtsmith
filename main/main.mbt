
fn main {
  let generator = @ast.RandomGenerator::new()
  let program : @ast.Program = generator.gen_program()
  println(program)
}
