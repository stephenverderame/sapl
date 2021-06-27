use std::env;
use sapl_lib::*;
fn main() {
    let args: Vec<String> = env::args().collect();
    if args.contains(&"repl".to_owned()) {
        sapl_repl()
    }
}

fn sapl_repl() {
    use std::io;
    let mut buffer = String::new();
    let cin = io::stdin();
    let mut last_read: usize = 0;
    loop {
        let mut ln = String::new();
        let read = cin.read_line(&mut ln).unwrap();
        if ln.contains("#QUIT_REPL") { break; }
        buffer.push_str(&ln);
        if last_read == 0 && read == 0 {
            let res = parse_sapl(buffer.as_bytes());
            println!("{:?}", res);

        }
        last_read = read;

    }
}
