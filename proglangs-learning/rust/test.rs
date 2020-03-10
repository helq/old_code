#[derive(Copy, Clone)]
struct Pt { x:f32, y:f32 }

// This works, but may be expensive:
fn dist1(p1 : Pt, p2: Pt) -> f32 {
   let xd = p1.x - p2.x;
   let yd = p1.y - p2.y;
   (xd * xd + yd * yd).sqrt()
}

// Same, using references:
fn dist2(p1 : &Pt, p2: &Pt) -> f32 {
   let xd = p1.x - p2.x;
   let yd = p1.y - p2.y;
   (xd * xd + yd * yd).sqrt()
}

// Usage:
fn main() {
   let a = Pt { x:1.0, y:2.0 };
   let b = Pt { x:0.0, y:3.0 };
   println!("{}", dist1(a, b));
   println!("{}", dist2(&a, &b));
}
