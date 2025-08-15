///|
fn main {
  let generator = @mbtsmith.RandomGenerator::new()
  let prog = generator.gen_program()
  println(prog)
}
