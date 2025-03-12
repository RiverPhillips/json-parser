use criterion::{Criterion, criterion_group, criterion_main};
use json_parser::parse_json;
use std::hint::black_box;

fn benchmark(c: &mut Criterion) {
    let file_content = std::fs::read_to_string("10mb.json").unwrap();

    c.bench_function("twitter.json", |b| {
        b.iter(|| {
            // Code to benchmark
            let result = parse_json(black_box(&file_content));
            result.unwrap();
        })
    });
}

criterion_group!(benches, benchmark);
criterion_main!(benches);
