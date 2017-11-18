#![recursion_limit = "1024"]

extern crate clap;
#[macro_use]
extern crate error_chain;

use std::cmp::Eq;
use std::hash::Hash;
use std::collections::HashMap;
use std::io;
use std::io::BufRead;

mod errors {
    error_chain!{}
}

use errors::*;

quick_main!(run);

fn run() -> Result<()> {
    let stdin = io::stdin();
    let stdin = stdin.lock();
    let freqs = stdin
        .lines()
        .fold(HashMap::new(), |mut m, rl| {
            count_item(&mut m, &rl.unwrap());
            m
        });
    let mut pairs: Vec<(String, usize)> = freqs.into_iter().collect();
    pairs.sort();

    pairs
        .into_iter()
        .for_each(|(k, v)| println!("{}\t{}", k, v));

    Ok(())
}

fn count_item<K>(freq_table: &mut HashMap<K, usize>, item: &K)
where
    K: Eq + Hash + Clone,
{
    let previous = freq_table.get(&item).unwrap_or(&0).clone();
    freq_table.insert(item.clone(), previous + 1);
}
