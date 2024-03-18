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
// State of the solver
// ------------------------------------------------------------------------------

struct State {
  candidates: [u32; 81],
  updated: bool,
}

impl State {

  fn new(solver: &Solver) -> State {
    let mut candidates: [u32; 81] = [0; 81];
    for given in &solver.givens {
      let cell = given.cell as usize;
      let value = given.value;
      assert!(cell < 81);
      assert!(value >= 1 && value <= 9);
      candidates[cell] = 1 << (value - 1);
    }
    State {
      candidates,
      updated: false,
    }
  }

}

// ------------------------------------------------------------------------------
// Solver
// ------------------------------------------------------------------------------

pub struct Solver {
  name: String,
  regions: Vec<Region>,
  givens: Vec<Given>,
  cages: Vec<Cage>,
}

impl Solver {

  pub fn solve(&self) {
    let start_time = std::time::Instant::now();
    println!("Hello, world!2");
    trace!("trace message");
    debug!("debug message");
    info!("info message");
    warn!("warn message");
    error!("error message");
    info!("name: {}", self.name);
    info!("solve time: {:.2}", start_time.elapsed().as_millis());
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