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
// Macro to collect bits
// ------------------------------------------------------------------------------

macro_rules! decode_to_bits {
  ($($args:expr),*) => {{
      let result : u128 = 0;
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
  cells_bitset: u128,
  cells: Vec<u32>,
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
    let value_mask = !(1 << (value - 1));
    'outer: for region in &self.solver.regions_for[cell] {
      // debug!("region (cell=={}, value=={}) {:?}", cell, value, region.cells);
      for neighbor in &region.cells {
        let index = *neighbor as usize;
        if cell == index { continue; }
        if self.solution[index] != 0 { continue; }
        self.candidates[index] &= value_mask;
        if self.candidates[index] == 0 {
          self.is_viable = false;
          break 'outer;
        }
      }
    }
  }

  fn naked_singles(&mut self) -> bool {
    let mut changed = false;
    'outer: for r in 0..9 {
      for c in 0..9 {
        let a = r * 9 + c;
        for v in 1..10 {
          if self.candidates[a] == 1 << (v - 1) {
            self.set_solution(a, v);
            self.stats.count_naked_singles += 1;
            debug!("({},{}) = {}", r + 1, c + 1, v);
            changed = true;
            if !self.is_viable { break 'outer; }
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
      let strrep = self.solution
        .iter()
        .map(|x| if *x == 0 { '*' } else { char::from_u32(x + 48).unwrap() })
        .collect::<String>();
      write!(f, "{}", strrep)
    }

}

impl fmt::Debug for State<'_> {

    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      let mut strrep = [[' '; 55]; 37];
      for r in 0..37 {
        for c in 0..55 {
          let n = (if r % 12 == 0 { 2 } else if r % 4 == 0 { 1 } else { 0 })
            + (if c % 18 == 0 { 6 } else if c % 6 == 0 { 3 } else { 0 });
          strrep[r][c] = match n {
            0 => ' ', 1 => '-', 2 => '=',
            3 => ':', 6 => '|', _ => '+',
          };
        }
      }
      for r in 0..9 {
        let ry = r * 4 + 1;
        for c in 0..9 {
          let a = r * 9 + c;
          let cx = c * 6 + 1;
          if self.solution[a] != 0 { strrep[ry][cx] = char::from_u32(self.solution[a] + 48).unwrap(); }
          if self.candidates[a] & 1 << 0 != 0 { strrep[ry + 0][cx + 2] = '1'; }
          if self.candidates[a] & 1 << 1 != 0 { strrep[ry + 0][cx + 3] = '2'; }
          if self.candidates[a] & 1 << 2 != 0 { strrep[ry + 0][cx + 4] = '3'; }
          if self.candidates[a] & 1 << 3 != 0 { strrep[ry + 1][cx + 2] = '4'; }
          if self.candidates[a] & 1 << 4 != 0 { strrep[ry + 1][cx + 3] = '5'; }
          if self.candidates[a] & 1 << 5 != 0 { strrep[ry + 1][cx + 4] = '6'; }
          if self.candidates[a] & 1 << 6 != 0 { strrep[ry + 2][cx + 2] = '7'; }
          if self.candidates[a] & 1 << 7 != 0 { strrep[ry + 2][cx + 3] = '8'; }
          if self.candidates[a] & 1 << 8 != 0 { strrep[ry + 2][cx + 4] = '9'; }
        }
      }
      let str = strrep
        .iter()
        .map(|q| q.iter().collect::<String>())
        .collect::<Vec<_>>()
        .join("\n");
      writeln!(f, "\n{}", str)
    }

}

// ------------------------------------------------------------------------------
// Solver
// ------------------------------------------------------------------------------

pub struct Solver {
  name: String, // TODO: Remove the name field
  regions: Vec<Region>,
  regions_for: [Vec<Region>; 81],
  givens: Vec<Given>,
  cages: Vec<Cage>,
}

impl Solver {

  pub fn solve(&self) -> Answer<State, State, String> {
    let start_time = std::time::Instant::now();
    let mut state = State::new(self);
    info!("{:?}", state);
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

  pub fn region(&mut self, cells: Vec<u32>) -> &mut Self {
    let mut cells_bitset = 0u128;
    for cell in 0..81 {
      if cells.contains(&cell) {
        cells_bitset |= 1 << cell;
      }
    }
    self.regions.push(Region { cells_bitset, cells });
    self
  }

  pub fn region_bitset(&mut self, cells_bitset: u128) -> &mut Self {
    let mut cells: Vec<u32> = Vec::new();
    for cell in 0..81 {
      if cells_bitset & 1 << cell != 0 {
        cells.push(cell);
      }
    }
    self.regions.push(Region { cells_bitset, cells });
    self
  }

  pub fn given(&mut self, cell: u32, value: u32) -> &mut Self {
    self.givens.push(Given { cell, value });
    self
  }

  pub fn givens(&mut self, values: String) -> &mut Self {
    let mut r = 0;
    let mut c = 0;
    for value in values.into_bytes() {
      match value {
        b'*' => c += 1,
        b'1'..=b'9' => { self.given(r * 9 + c, (value - b'0').into()); c += 1 },
        _ => ()
      }
      if c == 9 {
        c = 0;
        r += 1;
      }
    }
    self
  }

  pub fn cage(&mut self, cells: u128, sum: u32) -> &mut Self {
    self.cages.push(Cage { cells, sum });
    self
  }

  pub fn add_row_regions(&mut self) -> &mut Self {
    let mut row = decode_to_bits!("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9");
    for _ in 0..9 {
      self.region_bitset(row);
      row = row << 9;
    }
    self
  }

  pub fn add_column_regions(&mut self) -> &mut Self {
    let mut col = decode_to_bits!("A1", "B1", "C1", "D1", "E1", "F1", "G1", "H1", "I1");
    for _ in 0..9 {
      self.region_bitset(col);
      col = col << 1;
    }
    self
  }

  pub fn add_box_regions(&mut self) -> &mut Self {
    self.region_bitset(decode_to_bits!("A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3"));
    self.region_bitset(decode_to_bits!("D1", "D2", "D3", "E1", "E2", "E3", "F1", "F2", "F3"));
    self.region_bitset(decode_to_bits!("G1", "G2", "G3", "H1", "H2", "H3", "I1", "I2", "I3"));
    self.region_bitset(decode_to_bits!("A4", "A5", "A6", "B4", "B5", "B6", "C4", "C5", "C6"));
    self.region_bitset(decode_to_bits!("D4", "D5", "D6", "E4", "E5", "E6", "F4", "F5", "F6"));
    self.region_bitset(decode_to_bits!("G4", "G5", "G6", "H4", "H5", "H6", "I4", "I5", "I6"));
    self.region_bitset(decode_to_bits!("A7", "A8", "A9", "B7", "B8", "B9", "C7", "C8", "C9"));
    self.region_bitset(decode_to_bits!("D7", "D8", "D9", "E7", "E8", "E9", "F7", "F8", "F9"));
    self.region_bitset(decode_to_bits!("G7", "G8", "G9", "H7", "H8", "H9", "I7", "I8", "I9"));
    self
  }

  pub fn add_regular_regions(&mut self) -> &mut Self {
    self.add_row_regions();
    self.add_column_regions();
    self.add_box_regions();
    self
  }

  pub fn build(&mut self) -> Solver {
    let mut regions_for: [Vec<Region>; 81] = std::array::from_fn(|_| Vec::new());
    for cell in 0..81 {
      for region in &self.regions {
        if region.cells_bitset & (1 << cell) != 0 {
          regions_for[cell].push(region.clone());
        }
      }
    }
    Solver {
      name: self.name.clone(),
      regions: self.regions.clone(),
      regions_for,
      givens: self.givens.clone(),
      cages: self.cages.clone(),
    }
  }

}

// ------------------------------------------------------------------------------
// Tests
// ------------------------------------------------------------------------------

#[test]
fn check_one_naked_single() {
  let solver = SolverBuilder::default()
    .add_regular_regions()
    .givens("*12345678".to_owned())
    .build();
  if let Answer::Multiple(state) = solver.solve() {
    assert_eq!(1, state.stats.count_naked_singles);
  } else {
    assert!(false);
  }
}

#[test]
fn check_naked_singles() {
  let solver = SolverBuilder::default()
    .add_regular_regions()
    .givens("3********|*4135268*|*2918437*|*7354689*|*9287351*|*8621974*|*6479513*|*1543826*|*********".to_owned())
    .build();
  if let Answer::Single(state) = solver.solve() {
    assert_eq!(31, state.stats.count_naked_singles);
  } else {
    assert!(false);
  }
}
