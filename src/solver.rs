use std::fmt;

use log::*;

// ------------------------------------------------------------------------------
// Macro to decode coordinates
// ------------------------------------------------------------------------------

macro_rules! decode {
    ($s:expr) => {{
      let str = $s;
      assert_eq!(str.len(), 2);
      let chars = str.as_bytes();
      assert!(chars[0] >= b'A' && chars[0] <= b'I');
      assert!(chars[1] >= b'1' && chars[1] <= b'9');
      ((chars[0] - b'A') * 9 + chars[1] - b'1') as u32
    }}
}

// ------------------------------------------------------------------------------
// Macro to collect bits
// ------------------------------------------------------------------------------

macro_rules! bits {
  ($($args:expr),*) => {{
      let result : u128 = 0;
      $(
          let result = result | (1 << $args);
      )*
      result
  }}
}

// ------------------------------------------------------------------------------
// Constraints
// ------------------------------------------------------------------------------

#[derive(Clone)]
struct Region {
  cells: u128,
}

#[derive(Clone)]
struct Given {
  cell: u32,
  value: u32,
}

#[derive(Clone)]
struct Cage {
  cells: u128,
  sum: u32,
}

// ------------------------------------------------------------------------------
// Answer returned by the solver
// ------------------------------------------------------------------------------

pub enum Answer<S, T, E> {
  Single(S),
  Multiple(T),
  Err(E),
}

// ------------------------------------------------------------------------------
// Collection of counters for describing the steps of the solution.
// ------------------------------------------------------------------------------

struct Stats {
  count_naked_singles: u32,
}

impl Stats {

  fn new() -> Stats {
    Stats {
      count_naked_singles: 0,
    }
  }

}

// ------------------------------------------------------------------------------
// State of the solver
// ------------------------------------------------------------------------------

pub struct State {
  candidates: [u32; 81],
  solution: [u32; 81],
  unassigned_count: u32,
  updated: bool,
  is_viable: bool,
  stats: Stats,
}

impl State {

  fn new(solver: &Solver) -> State {
    let mut state = State {
      candidates: [(1 << 9) - 1; 81],
      solution: [0; 81],
      unassigned_count: 81,
      updated: false,
      is_viable: true,
      stats: Stats::new(),
    };
    for given in &solver.givens {
      let cell = given.cell as usize;
      let value = given.value;
      assert!(cell < 81);
      assert!(value >= 1 && value <= 9);
      state.set_solution(cell, value);
    }
    state
  }

  fn is_viable(&self) -> bool { self.is_viable }

  fn is_solved(&self) -> bool { self.unassigned_count == 0 }

  fn set_solution(&mut self, cell: usize, value: u32) {
    self.candidates[cell] = 0;
    self.solution[cell] = value;
    self.unassigned_count -= 1;
    self.updated = true;
  }

  fn naked_singles(&mut self) -> bool {
    let mut changed = false;
    for r in 0..9 {
      for c in 0..9 {
        let a = r * 9 + c;
        for v in 1..10 {
          if self.candidates[a] == 1 << (v - 1) {
            self.set_solution(a, v);
            self.stats.count_naked_singles += 1;
            debug!("({},{}) = {}", r + 1, c + 1, v);
            changed = true;
          }
        }
      }
    }
    changed
  }

  fn solve(&mut self) {
    self.updated = true;
    while self.updated && self.is_viable {
      self.updated = false;
      self.naked_singles();
    }
  }

}

impl fmt::Display for State {

    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      let strrep = self.candidates
        .iter()
        .map(|x| if *x == 0 { '*' } else { char::from_u32(x + 48).unwrap() })
        .collect::<String>();
      write!(f, "{}", strrep)
    }

}

// ------------------------------------------------------------------------------
// Solver
// ------------------------------------------------------------------------------

pub struct Solver {
  name: String, // TODO: Remove the name field
  regions: Vec<Region>,
  givens: Vec<Given>,
  cages: Vec<Cage>,
}

impl Solver {

  pub fn solve(&self) -> Answer<State, State, String> {
    let start_time = std::time::Instant::now();
    let mut state = State::new(self);
    state.solve();
    info!("name: {}", self.name);
    info!("solve time: {:.2}", start_time.elapsed().as_millis());
    if !state.is_viable() {
      Answer::Err(String::from("no solution"))
    } else if state.is_solved() {
      Answer::Single(state)
    } else {
      Answer::Multiple(state)
    }
  }
  
}

// ------------------------------------------------------------------------------
// SolverBuilder
// ------------------------------------------------------------------------------

pub struct SolverBuilder {
  name: String,
  regions: Vec<Region>,
  givens: Vec<Given>,
  cages: Vec<Cage>,
}

impl SolverBuilder {

  pub fn default() -> SolverBuilder {
    SolverBuilder {
      name: String::from(""),
      regions: Vec::new(),
      givens: Vec::new(),
      cages: Vec::new(),
    }
  }

  pub fn name(&mut self, name: String) -> &mut Self {
    self.name = name;
    self
  }

  pub fn region(&mut self, cells: u128) -> &mut Self {
    self.regions.push(Region { cells });
    self
  }

  pub fn given(&mut self, cell: u32, value: u32) -> &mut Self {
    self.givens.push(Given { cell, value });
    self
  }

  pub fn cage(&mut self, cells: u128, sum: u32) -> &mut Self {
    self.cages.push(Cage { cells, sum });
    self
  }

  pub fn add_row_regions(&mut self) -> &mut Self {
    let mut row = bits!(
      decode!("A1"),
      decode!("A2"),
      decode!("A3"),
      decode!("A4"),
      decode!("A5"),
      decode!("A6"),
      decode!("A7"),
      decode!("A8"),
      decode!("A9")
    );
    for _ in 0..9 {
      self.regions.push(Region { cells: row });
      row = row << 9;
    }
    self
  }

  pub fn add_column_regions(&mut self) -> &mut Self {
    let mut col = bits!(
      decode!("A1"),
      decode!("B1"),
      decode!("C1"),
      decode!("D1"),
      decode!("E1"),
      decode!("F1"),
      decode!("G1"),
      decode!("H1"),
      decode!("I1")
    );
    for _ in 0..9 {
      self.regions.push(Region { cells: col });
      col = col << 1;
    }
    self
  }

  pub fn build(&mut self) -> Solver {
    Solver {
      name: self.name.clone(),
      regions: self.regions.clone(),
      givens: self.givens.clone(),
      cages: self.cages.clone(),
    }
  }

}