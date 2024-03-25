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
    //.givens("3********|*4135268*|*2918437*|*7354689*|*9287351*|*8621974*|*6479513*|*1543826*|*********".to_owned())
    //.givens("*1234567834567891*".to_owned())
    //--.givens("***3*****|*********|******3**|**3******|*********|*********|*********|*********|*3*******".to_owned())
    //.givens("4*****938|*32*941**|*953**24*|37*6*9**4|529**1673|6*47*3*9*|957**83**|**39**4**|24**3*7*9".to_owned())
    //.givens("*********|231*9****|*65**31**|**8924***|1***5***6|***1367**|**93**57*|****1*843|*********".to_owned())
    //.givens("*******15|79*******|***2*****|*****87*6|**1******|******9**|*7****83*|4**15****|***3*****".to_owned())
    //.givens("*179*36**|****8****|9*****5*7|*72*1*43*|***4*2*7*|*6437*25*|7*1****65|****3****|**56*172*".to_owned())
    .givens("*32**61**|41*******|***9*1***|5***9***4|*6*****7*|3***2***5|***5*8***|*******19|**7***86*".to_owned())
    .build();
  let response = solver.solve();
  match response {
    Answer::Err(_) => println!("No solution"),
    Answer::Multiple(_) => println!("Multiple solutions"),
    Answer::Single(solution) => println!("Solution: {}", solution),
  }
}
