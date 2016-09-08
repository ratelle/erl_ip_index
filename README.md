# `erl_ip_index`

There was no README, until now...

## Fuzzing

This NIF can be fuzzed with [american fuzzy lop][afl] through [niffy][niffy].
Compile niffy with `afl-gcc` and point `make` to `fuzz_skeleton`.

    make fuzz FUZZ_SKELETON=../niffy/fuzz_skeleton

[afl]: http://lcamtuf.coredump.cx/afl/
[niffy]: https://github.com/tokenrove/niffy
