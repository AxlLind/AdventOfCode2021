// a macro to automatically time the solution
#[macro_export]
macro_rules! main {
  ($($body:tt)+) => {
    fn main() {
      let now = std::time::Instant::now();
      let (p1,p2) = { $($body)+ };
      let time = now.elapsed().as_millis();
      println!("Part one: {}", p1);
      println!("Part two: {}", p2);
      println!("Time: {}ms", time);
    }
  }
}
