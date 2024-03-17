mod solver;
use crate::solver::*;//SolverBuilder;

macro_rules! decode {
    ($s:expr) => {{
      assert_eq!($s.len(), 2);
      let chars = $s.as_bytes();
      assert!(chars[0] >= b'A' && chars[0] <= b'I');
      assert!(chars[1] >= b'1' && chars[1] <= b'9');
      ((chars[0] - b'A') * 9 + chars[1] - b'1') as u32
    }}
}
// fn decode(coord: String) -> u32 {
//   assert_eq!(coord.len(), 2);
//   let chars = coord.as_bytes();
//   assert!(chars[0] >= b'A' && chars[0] <= b'I');
//   assert!(chars[1] >= b'1' && chars[1] <= b'9');
//   ((chars[0] - b'A') * 9 + chars[1] - b'1') as u32
// }

fn main() {
  env_logger::init();
  let mut solver_builder = SolverBuilder::default();
  solver_builder.name("Jaco".to_owned());
  solver_builder.constraint(Constraint::Region(10));
  let solver = solver_builder.build();
  println!("Hello, world!");
  println!("A1 == {}", decode!("A1"));
  println!("B2 == {}", decode!("B2"));
  println!("F6 == {}", decode!("F6"));
  solver.solve();
}
