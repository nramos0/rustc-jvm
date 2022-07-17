extern crate clap;
use clap::{App, Arg};

use jvm_bytecode::class_file::{access_flag::*, *};
use std::fs::File;
use std::io::BufWriter;

// "./test/test/src/main.rs"
fn run(input: &str, class_name: &str) -> std::io::Result<()> {
    let string = std::fs::read_to_string(input)?;
    let tokens = rustc_jvm_lexer::tokenize(&string[..]);

    let mut class = ClassFileBuilder::build()
        .version(0, 61)
        .access_flags(AccessFlagBuilder::class().acc_public().flag())
        .this_class(class_name)
        .super_class("java/lang/Object")
        .interfaces()
        .fields()
        .methods();
    class.constant_pool.include_java_system_out();

    rustc_jvm_parser::parse(tokens, &mut class);

    let class = class.attributes().to_class_file();

    class.write_to(BufWriter::new(
        File::create(&format!("{}.class", class_name)[..]).expect("Unable to create file"),
    ))?;

    Ok(())
}

fn main() {
    let matches = App::new("rustc_jvm")
        .version("1.0")
        .author("Nicholas Ramos")
        .about("A Rust compiler that targets the JVM")
        .arg(
            Arg::with_name("input")
                .short("i")
                .value_name("FILE")
                .help("A Rust source file")
                .required(true)
                .takes_value(true),
        )
        .arg(
            Arg::with_name("class")
                .short("c")
                .value_name("CLASS_NAME")
                .help("The name of the Java class to output")
                .required(true)
                .takes_value(true),
        )
        .get_matches();

    if let Err(e) = run(
        matches.value_of("input").unwrap(),
        matches.value_of("class").unwrap(),
    ) {
        eprintln!("{}", e);
    };
}
