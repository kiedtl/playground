use std::env;
use std::process;
use std::vec::Vec;

fn main() {
    let args = env::args().collect::<Vec<String>>();

    // exit if no arguments
    if args.len() < 4 {
        process::exit(1);
    }

    let x = args[1].parse::<i32>().unwrap();
    let y = args[3].parse::<i32>().unwrap();
    let op = args[2].clone();

    if op == "<<" {
        println!("result: {}", x << y);
    } else if op == ">>" {
        println!("result: {}", x >> y);
    } else if op == "&" {
        println!("result: {}", x & y);
    } else if op == "|" {
        println!("result: {}", x | y);
    } else if op == "^" {
        println!("result: {}", x ^ y);
    }
}
