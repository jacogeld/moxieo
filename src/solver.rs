use std::fmt;

use log::*;

// ------------------------------------------------------------------------------
// Type aliases
// ------------------------------------------------------------------------------

type Address = u32;
type AddressBitset = u128;

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
      let result : AddressBitset = 0;
      $(
          let result = result | (1 << $args);
      )*
      result
  }}
}

// ------------------------------------------------------------------------------
// Macro to collect bits
// ------------------------------------------------------------------------------

macro_rules! decode_to_bits {
  ($($args:expr),*) => {{
      let result : AddressBitset = 0;
      $(
          let result = result | (1 << decode!($args));
      )*
      result
  }}
}

// ------------------------------------------------------------------------------
// Constraints
// ------------------------------------------------------------------------------

#[derive(Clone)]
struct Region {
  cells: AddressBitset,
}

#[derive(Clone)]
struct Given {
  cell: Address,
  value: u32,
}

#[derive(Clone)]
struct Cage {
  cells: AddressBitset,
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

pub struct State<'a> {
  candidates: [u32; 81],
  solution: [u32; 81],
  unassigned_count: u32,
  updated: bool,
  is_viable: bool,
  stats: Stats,
  solver: &'a Solver,
}

impl State<'_> {

  fn new(solver: &Solver) -> State {
    let mut state = State {
      candidates: [(1 << 9) - 1; 81],
      solution: [0; 81],
      unassigned_count: 81,
      updated: false,
      is_viable: true,
      stats: Stats::new(),
      solver: solver,
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
    // for region in self.solver.regions {
    //   if region & (1 << cell)
    // }
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

impl fmt::Display for State<'_> {

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

  pub fn region(&mut self, cells: AddressBitset) -> &mut Self {
    self.regions.push(Region { cells });
    self
  }

  pub fn given(&mut self, cell: Address, value: u32) -> &mut Self {
    self.givens.push(Given { cell, value });
    self
  }

  pub fn cage(&mut self, cells: AddressBitset, sum: u32) -> &mut Self {
    self.cages.push(Cage { cells, sum });
    self
  }

  pub fn add_row_regions(&mut self) -> &mut Self {
    let mut row = decode_to_bits!("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9");
    for _ in 0..9 {
      self.regions.push(Region { cells: row });
      row = row << 9;
    }
    self
  }

  pub fn add_column_regions(&mut self) -> &mut Self {
    let mut col = decode_to_bits!("A1", "B1", "C1", "D1", "E1", "F1", "G1", "H1", "I1");
    for _ in 0..9 {
      self.regions.push(Region { cells: col });
      col = col << 1;
    }
    self
  }

  pub fn add_box_regions(&mut self) -> &mut Self {
    self.regions.push(Region { cells: decode_to_bits!("A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3") });
    self.regions.push(Region { cells: decode_to_bits!("D1", "D2", "D3", "E1", "E2", "E3", "F1", "F2", "F3") });
    self.regions.push(Region { cells: decode_to_bits!("G1", "G2", "G3", "H1", "H2", "H3", "I1", "I2", "I3") });
    self.regions.push(Region { cells: decode_to_bits!("A4", "A5", "A6", "B4", "B5", "B6", "C4", "C5", "C6") });
    self.regions.push(Region { cells: decode_to_bits!("D4", "D5", "D6", "E4", "E5", "E6", "F4", "F5", "F6") });
    self.regions.push(Region { cells: decode_to_bits!("G4", "G5", "G6", "H4", "H5", "H6", "I4", "I5", "I6") });
    self.regions.push(Region { cells: decode_to_bits!("A7", "A8", "A9", "B7", "B8", "B9", "C7", "C8", "C9") });
    self.regions.push(Region { cells: decode_to_bits!("D7", "D8", "D9", "E7", "E8", "E9", "F7", "F8", "F9") });
    self.regions.push(Region { cells: decode_to_bits!("G7", "G8", "G9", "H7", "H8", "H9", "I7", "I8", "I9") });
    self
  }

  pub fn add_regular_regions(&mut self) -> &mut Self {
    self.add_row_regions();
    self.add_column_regions();
    self.add_box_regions();
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