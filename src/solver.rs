use log::*;

#[derive(Clone)]
pub enum Constraint {
  Region(u32)
}

// ------------------------------------------------------------------------------
// Solver
// ------------------------------------------------------------------------------

pub struct Solver {
  name: String,
  constraints: Vec<Constraint>,
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
    info!("constraints: {}", self.constraints.len());
    info!("solve time: {:.2}", start_time.elapsed().as_millis());
  }
  
}

// ------------------------------------------------------------------------------
// SolverBuilder
// ------------------------------------------------------------------------------

pub struct SolverBuilder {
  name: String,
  constraints: Vec<Constraint>,
}

impl SolverBuilder {

  pub fn default() -> SolverBuilder {
    SolverBuilder {
      name: String::from(""),
      constraints: Vec::new(),
    }
  }

  pub fn name(&mut self, name: String) -> &mut Self {
    self.name = name;
    self
  }

  pub fn constraint(&mut self, constraint: Constraint) -> &mut Self {
    self.constraints.push(constraint);
    self
  }

  pub fn build(&mut self) -> Solver {
    Solver {
      name: self.name.clone(),
      constraints: self.constraints.clone(),
    }
  }

}