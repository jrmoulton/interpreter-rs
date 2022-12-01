use criterion::{black_box, criterion_group, criterion_main, Criterion};
use lexer::Lexer;

fn lex_test_func() {
    let mut lexer = Lexer::new(include_str!("large_file.dmd").to_string());
    for _ in lexer.next() {}
}

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("large file", |b| b.iter(lex_test_func));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
