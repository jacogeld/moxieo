#[macro_use]
mod solver;
use crate::solver::*;

fn main() {
  env_logger::init();
  let solver = SolverBuilder::default()
    .name("Jaco".to_owned())
    .add_row_regions()
    .add_column_regions()
    .given(decode!("A1"), 2)
    .build();
  let response = solver.solve();
  match response {
    Answer::Err(_) => println!("No solution"),
    Answer::Multiple(_) => println!("Multiple solutions"),
    Answer::Single(solution) => println!("Solution: {}", solution),
  }
}
