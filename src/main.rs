use std::env;

extern crate snc;

fn main() {
    // TODO: Use getopts (https://doc.rust-lang.org/getopts/getopts/struct.Options.html) for
    // argument parsing.
    // TODO: Have this accept a config file and an optional path to query.
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Usage: snc <file> [path...]");
        return;
    }
    // TODO: Handle path.
    // let path = args.get(3);

    println!("Parsing {}", args[1]);
    match snc::load(args[1].clone()) {
        // TODO: Implement display for ConfigMap; print.
        Ok(config) => println!("Parsed fine: {:?}", config),
        Err(e) => println!("Did not parse: {}", e),
    }
}
