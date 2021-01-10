#/bin/bash

# https://www.rustup.rs/
curl https://sh.rustup.rs -sSf | sh

# https://github.com/racer-rust/racer
cargo install racer
rustup component add rust-src

# https://github.com/rust-lang-nursery/rustfmt
cargo install rustfmt-nightly
