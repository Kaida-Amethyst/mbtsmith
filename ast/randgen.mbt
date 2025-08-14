struct Env[K, V] {
  map: Map[K, V]
  parent: Env[K, V]?
}

fn[K, V] Env::new() -> Env[K, V] {
  Env::{ map: Map::new(), parent: None }
}

fn[K, V] Env::sub_env(self: Self[K, V]) -> Env[K, V] {
  Env::{ map: Map::new(), parent: Some(self) }
}

fn[K:Eq+Hash, V] Env::get(self: Self[K, V], key: K) -> V? {
  match self.map.get(key) {
    Some(value) => Some(value);
    None => match self.parent {
      Some(parent) => parent.get(key);
      None => None;
    }
  }
}

fn[K:Eq+Hash, V] Env::set(self: Self[K, V], key: K, value: V) -> Unit {
  self.map.set(key, value);
}

fn[K, V: Eq] Env::collect_keys_by_value(self: Self[K, V], value: V) -> Array[K] {
  let keys = Array::new();
  for pair in self.map {
    let (key, v) = pair;
    if v == value {
      keys.push(key);
    }
  }
  if self.parent is Some(parent) {
    keys.append(parent.collect_keys_by_value(value));
  }
  keys
}

pub struct RandAstGenerator {
  rand: @random.Rand
  upper_max_len: Int
  ident_max_len: Int
  ty_env: Env[Upper, Type]
  id_env: Env[Ident, Type]
}

pub fn RandAstGenerator::new(upper_max_len~: Int = 10, ident_max_len~:Int=10) -> RandAstGenerator {
  RandAstGenerator::{
    rand: @random.Rand::new(),
    upper_max_len,
    ident_max_len,
    ty_env: Env::new(),
    id_env: Env::new(),
  }
}

///|
pub fn RandAstGenerator::int(self: Self, limit: Int) -> Int {
  self.rand.int(limit~)
}

///|
///
/// Always return positive integers.
pub fn RandAstGenerator::pos_int(self: Self, limit: Int) -> Int {
  let limit = limit.abs()
  let limit = limit.reinterpret_as_uint()
  self.rand.uint(limit~).reinterpret_as_int()
}

pub fn[T] RandAstGenerator::choice(self: Self, choices: Array[T]) -> T {
  let length = choices.length();
  if length == 0 {
    println("Fetal Error: Cannot choose from an empty array");
    panic()
  }
  let index = self.pos_int(length);
  choices[index]
}

const UPPER_CHARS: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
const LOWER_CHARS: String = "abcdefghijklmnopqrstuvwxyz";
const DIGITS: String = "0123456789";
const IDENT_CHARS: String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_";
const NON_UPPER_CHARS: String = "abcdefghijklmnopqrstuvwxyz0123456789_";

///|
pub fn RandAstGenerator::gen_upper(self: Self) -> Upper {
  fn helper() {
    let str_builder = StringBuilder::new();
    let first_char = self.choice(UPPER_CHARS.to_array());
    str_builder.write_char(first_char);
    for i in 0..<self.pos_int(self.upper_max_len) {
      let c = self.choice(IDENT_CHARS.to_array());
      str_builder.write_char(c);
    }
    Upper(str_builder.to_string())
  }
  let mut upper = helper()
  if self.ty_env.get(upper) is Some(_) {
    upper = helper()
  }
  upper
}

///|
pub fn RandAstGenerator::gen_ident(self: Self) -> Ident {
  fn helper() -> Ident {
    let str_builder = StringBuilder::new();
    let first_char = self.choice(NON_UPPER_CHARS.to_array());
    str_builder.write_char(first_char);
    for i in 0..<self.pos_int(self.ident_max_len) {
      let c = self.choice(IDENT_CHARS.to_array());
      str_builder.write_char(c);
    }
    Ident(str_builder.to_string())
  }
  let mut ident = helper();
  while self.id_env.get(ident) is Some(_) {
    ident = helper()
  }
  ident
}

pub fn RandAstGenerator::gen_double(self: Self) -> Double {
  self.rand.double()
}

pub fn RandAstGenerator::gen_bool(self: Self) -> Bool {
  self.rand.int() % 2 == 0
}


