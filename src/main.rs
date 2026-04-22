mod expr;
mod parser;

use crate::{
    expr::{Expr, VarName},
    parser::{Lexer, ParserError},
};
use rustyline::{DefaultEditor, error::ReadlineError};
use std::{
    collections::HashMap,
    fmt,
    io::{self, Read, Write},
};

type Env = HashMap<String, Expr>;

#[derive(PartialEq)]
enum CurrMode<'a> {
    Default,
    Let(&'a str),
    LetStar(&'a str),
    All,
    Num,
    LoadStd,
}

#[derive(PartialEq)]
enum Error<'a> {
    LetCommandError,
    NonExistingBinding(&'a str),
    UnknownCommand(&'a str),
    ParserError(ParserError),
    ExceedLimit,
    Quit,
}

impl fmt::Display for Error<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::LetCommandError => write!(f, "failed to parse :let command"),
            Error::NonExistingBinding(s) => {
                write!(f, "`{s}` binding does not exist in environment")
            }
            Error::UnknownCommand(s) => {
                write!(
                    f,
                    "unknown command `{s}`.\n{help_msg}",
                    help_msg = help_msg()
                )
            }
            Error::ParserError(err) => write!(f, "{err}"),
            Error::ExceedLimit => write!(f, "exceeded evaluation cycle limit"),
            Error::Quit => write!(f, "Quitting..."),
        }
    }
}

const STDLIB: &[(&str, &str)] = &[
    ("id", "λx.x"),
    ("const", "λx.λy.x"),
    ("flip", "λf.λx.λy.f y x"),
    // boolean logic
    ("true", "const"),
    ("false", "flip true"),
    ("not", "λp.p false true"),
    ("and", "λp1.λp2.p1 p2 p1"),
    ("or", "λp1.λp2.p1 p1 p2"),
    // pairs
    ("cons", "λx.λy.λf.f x y"),
    ("fst", "λp.p true"),
    ("snd", "λp.p false"),
    // numbers
    ("zero", "false"),
    ("one", "λf.λx.f x"),
    ("succ", "λn.λf.λx.f (n f x)"),
    ("add", "λm.λn.m succ n"),
    ("mul", "λm.λn.m (add n) zero"),
    ("pow", "λb.λn.n b"),
    ("phi", "λp.cons (snd p) (succ (snd p))"),
    ("pred", "λn.fst (n phi (cons zero zero))"),
    ("sub", "λm.λn.n pred m"),
    // more numbers
    ("two", "succ one"),
    ("three", "succ two"),
    ("four", "succ three"),
    ("Y", "(λx.x x) (λx.x x)"),
];

fn help_msg() -> String {
    let mut help_msg =
        "Use `:help <command>` or `:h <command>` for details about each command\nList of commands:"
            .to_string();
    for c in [
        "all", "env", "h", "help", "let", "let*", "loadstd", "num", "quit", "q",
    ] {
        help_msg.push_str("\n - ");
        help_msg.push_str(c);
    }
    help_msg
}

fn replace_from_env(expr: Expr, env: &Env) -> Expr {
    let mut expr: Expr = expr;
    let mut var_id = 0usize;
    for (key, replacement) in env {
        let var = VarName {
            name: key.to_string(),
            id: 0,
        };
        expr = expr.replace(&var, replacement, &mut var_id);
    }
    expr
}

fn reduce_apps(expr: Expr, num: usize) -> usize {
    match expr {
        Expr::App(f, body) if matches!(*f, Expr::Var(_)) => reduce_apps(*body, num + 1),
        _ => num,
    }
}

fn app_to_num(expr: Expr) -> usize {
    match expr {
        Expr::Abs(_, body) => match *body {
            Expr::Abs(_, body) => reduce_apps(*body, 0),
            _ => 0,
        },
        _ => 0,
    }
}

fn eval_loop<F>(expr: Expr, in_loop_action: F) -> (Expr, bool)
where
    F: Fn(&Expr) -> bool,
{
    let mut var_id = 0usize;
    let mut curr = expr.bind_vars(&mut var_id);
    for _ in 0..2000 {
        let mut next = curr.clone().reduce(&mut var_id);
        if next == curr {
            // Try to find if we can reduce further by updating variable IDs
            curr = curr.free_variables().bind_vars(&mut var_id);
            next = curr.clone().reduce(&mut var_id);
            if next == curr {
                return (curr, false);
            }
        }
        if !in_loop_action(&curr) {
            return (curr, true);
        }
        curr = next;
    }
    (curr, true)
}

fn process_line<'b, 'a>(input: &'a str, env: &'b mut Env) -> Result<(), Error<'a>> {
    let (mode, input) = match input.split_once(char::is_whitespace).unwrap_or((input, "")) {
        (":env", input) => {
            let saved_key = input.trim();
            if saved_key.is_empty() {
                for (key, expr) in env.iter() {
                    println!("{key} = {expr}");
                }
                return Ok(());
            }

            let saved_value = env
                .get(saved_key)
                .ok_or(Error::NonExistingBinding(saved_key))?;
            println!("{saved_key} = {saved_value}");
            return Ok(());
        }
        (":quit", _) | (":q", _) => return Err(Error::Quit),
        (":help", command) | (":h", command) => {
            let command = command.trim();
            let help_msg = help_msg();
            if command.is_empty() {
                println!("{help_msg}");
                return Ok(());
            }

            match command {
                "all" => {
                    println!(
                        ":all <expr>\nReduce expression as much as possible and print each reduction"
                    );
                }
                "env" => {
                    println!(
                        ":env <binding?>\nPrint current environment. If binding name is provided, print only it"
                    );
                }
                "h" | "help" => {
                    println!("{help_msg}");
                }
                "let" => {
                    println!(":let <binding> <expr>\nBind a certain name to an expression");
                }
                "let*" => {
                    println!(":let* <binding> <expr>\nBind a certain name to a reduced expression");
                }
                "loadstd" => {
                    println!(":loadstd\nLoad standard library");
                }
                "num" => {
                    println!(
                        ":num <expr>\nConvert Church numerals to a number (f.e. `f (f (f x))` => `3`)"
                    );
                }
                "quit" | "q" => {
                    println!(":quit OR :q\nQuit the interpreter");
                }
                s => return Err(Error::UnknownCommand(s)),
            }
            return Ok(());
        }
        (":all", rest) => (CurrMode::All, rest.trim()),
        (":num", rest) => (CurrMode::Num, rest.trim()),
        (":loadstd", rest) => (CurrMode::LoadStd, rest),
        (command, rest) if command == ":let" || command == ":let*" => {
            let (binding, expr) = rest
                .split_once(char::is_whitespace)
                .ok_or(Error::LetCommandError)?;
            let mode = if command == ":let" {
                CurrMode::Let(binding.trim())
            } else {
                CurrMode::LetStar(binding.trim())
            };
            (mode, expr.trim())
        }
        (command, _) if command.starts_with(":") => return Err(Error::UnknownCommand(command)),
        _ => (CurrMode::Default, input.trim()),
    };

    if mode == CurrMode::LoadStd {
        for (key, value) in STDLIB {
            let parsed = Lexer::new(value)
                .parse_expr()
                .map_err(|err| Error::ParserError(err))?;
            let (expr, _) = eval_loop(replace_from_env(parsed, env), |_| true);
            env.insert(key.to_string(), expr.free_variables());
        }
        return Ok(());
    }

    let parsed = Lexer::new(input)
        .parse_expr()
        .map_err(|err| Error::ParserError(err))?;
    let expr = replace_from_env(parsed, env);
    match mode {
        CurrMode::Let(binding) => {
            env.insert(binding.to_string(), expr);
            Ok(())
        }
        CurrMode::LetStar(binding) => {
            let (expr, exceeded_limit) = eval_loop(expr, |_| true);
            if exceeded_limit {
                return Err(Error::ExceedLimit);
            }
            env.insert(binding.to_string(), expr.free_variables());
            Ok(())
        }
        CurrMode::Num => {
            let (expr, exceeded_limit) = eval_loop(expr, |_| true);
            if exceeded_limit {
                return Err(Error::ExceedLimit);
            }
            let num = app_to_num(expr);
            println!("{num}");
            Ok(())
        }
        CurrMode::All => {
            let (expr, exceed_limit) = eval_loop(expr, |expr| {
                println!("{expr}");
                true
            });
            if exceed_limit {
                println!("....");
            } else {
                println!("{expr}");
            }
            Ok(())
        }
        CurrMode::Default => {
            let (expr, dont_print) = eval_loop(expr, |expr| {
                print!("{expr} # Reduce? Y/n ");
                if let Err(_) = io::stdout().flush() {
                    return false;
                }
                let mut byte = [0u8];
                if let Err(_) = io::stdin().read_exact(&mut byte) {
                    return false;
                }
                !(byte[0] == b'n' || byte[0] == b'N')
            });
            if !dont_print {
                println!("{expr}");
            }
            Ok(())
        }
        CurrMode::LoadStd => unreachable!(),
    }
}

fn main() -> rustyline::Result<()> {
    let mut rl = DefaultEditor::new()?;
    let mut env = HashMap::new();

    loop {
        let readline = rl.readline("λ> ");
        match readline {
            Ok(line) => {
                if line.trim().is_empty() {
                    continue;
                }
                rl.add_history_entry(&line)?;
                if let Err(err) = process_line(&line, &mut env) {
                    if err == Error::Quit {
                        return Ok(());
                    }
                    eprintln!("Error: {err}");
                }
            }
            Err(ReadlineError::Interrupted) => break,
            Err(ReadlineError::Eof) => break,

            Err(err) => return Err(err),
        }
    }

    Ok(())
}
