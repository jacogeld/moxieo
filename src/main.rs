#[macro_use]
mod solver;
use crate::solver::*;

fn main() {
  env_logger::init();
  let solver = SolverBuilder::default()
    .name("Jaco".to_owned())
    .add_regular_regions()
    //.given(decode!("A1"), 2)
    //.given(decode!("B2"), 3)
    //.given(decode!("C3"), 4)
    // .givens("*1234567834567891*".to_owned())
    .givens("3********|*4135268*|*2918437*|*7354689*|*9287351*|*8621974*|*6479513*|*1543826*|*********".to_owned())
    .build();
  let response = solver.solve();
  match response {
    Answer::Err(_) => println!("No solution"),
    Answer::Multiple(_) => println!("Multiple solutions"),
    Answer::Single(solution) => println!("Solution: {}", solution),
  }
}
