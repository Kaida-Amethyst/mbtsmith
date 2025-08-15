
fn main {
  let generator = @mbtsmith.RandomGenerator::new(seed=2)
  let program : @mbtsmith.Program = generator.gen_program()
  println(program)
}
