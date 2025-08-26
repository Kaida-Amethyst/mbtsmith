///|
fn main {
  let generator = @mbtsmith.RandProgGenerator::new()
  let prog = generator.gen_program()
  println(prog)
}
