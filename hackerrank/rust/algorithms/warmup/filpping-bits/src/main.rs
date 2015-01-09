extern crate core;
use core::str::FromStr;
use std::io;

fn main() {
    let t = read::<usize>();

    let mut res = Vec::with_capacity(t);
    for _ in range(0, t) {
        let x = read::<u32>();
        res.push(!x);
    }

    print_result(res.as_slice());
}

fn print_result(xs: &[u32]) {
    for x in xs.iter() {
        println!("{}", x)
    }
}

fn read<T: FromStr>() -> T {
    let input = io::stdin().read_line()
        .ok()
        .expect("Failed to read line");
    input.trim().parse::<T>()
        .expect(format!("Failed to parse '{}'", input).as_slice())
}


#[test]
fn it_works() {
}
