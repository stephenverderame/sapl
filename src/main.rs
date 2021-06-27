use std::env;
use sapl_lib::*;
use std::io::Read;
fn main() {
    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);
    if args.contains(&"repl".to_owned()) {
        sapl_repl()
    } else if args.len() == 3 && args[1] == "parse" {
        let file = args[2].clone();        
        match std::fs::File::open(file) {
            Ok(mut file) => {
                let mut code = String::new();
                file.read_to_string(&mut code).unwrap();
                let res = parse_sapl(code.as_bytes());
                match res {
                    Res::Vl(v) | Res::Ret(v) => println!("{}", v),
                    e => println!("{:?}", e),
                }
            },
            _ => {
                println!("Cannot open file");
            }
        }
    }
}

fn sapl_repl() {
    use std::io;
    use std::io::Write;
    use sapl_lib::Res::*;
    let mut buffer = String::new();
    let cin = io::stdin();
    let mut last_read: usize = 0;
    io::stdout().write_all("SAPL REPL v1.0. Type '#QUIT' to quit\n".as_bytes()).unwrap();
    let mut env = get_std_environment();
    loop {
        let mut ln = String::new();
        cin.read_line(&mut ln).unwrap();
        if ln.contains("#QUIT") { break; }
        buffer.push_str(&ln);
        ln = ln.trim_end().to_owned();
        if last_read == 0 && ln.len() == 0 {
            match parse_and_eval(buffer.as_bytes(), &mut Some(&mut env)) {
                Vl(res) | Exn(res) | Ret(res) => println!("{}", res),
                Bad(msg) => println!("{}", msg),
            }
            buffer.clear();

        }
        last_read = ln.len();

    }
}
