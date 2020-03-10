fn main() {
    let r;

    {
        let x = 5;
        r = &x; // <- failiure
    }

    println!("r: {}", r);
}
