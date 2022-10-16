use math_evaluation::calc;

fn main() {
    let x = calc("(0.0625 + 0.1875) + (0.375 + 2 * 0.1875)");
    println!("{:?}", x);
}
