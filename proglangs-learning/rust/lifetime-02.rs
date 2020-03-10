// taken from: https://doc.rust-lang.org/book/second-edition/ch10-03-lifetime-syntax.html

fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
  if x.len() > y.len() {
    x
  } else {
    y
  }
}

fn main() {
  let string1 = String::from("abc");
  //let result : &str; // <- declaring result here and not inside the smaller scope makes rustc to
  //abort compilation because there is no warranty string2 will be available to result after its
  //scope ends

  {
    let string2 = String::from("wxyz");
    let result = longest(string1.as_str(), string2.as_str());

    println!("The longest string is \"{}\"", result);
  }

}
