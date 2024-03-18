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
  println!("Hello, world!");
  solver.solve();
}
