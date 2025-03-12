use std::fs;

fn main() {
    let json_str = fs::read_to_string("twitter.json").unwrap();

    let json = json_parser::parse_json(&json_str).unwrap();

    println!("{:?}", json);
}
