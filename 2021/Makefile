DAYS  := 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
.DEFAULT_GOAL := help
.PHONY: $(DAYS) all help

inputs/%.in:
	../fetch.sh 2021 $*

src/bin/%.rs:
	DAY=$* envsubst < src/template.rs > $@

$(DAYS): %: inputs/%.in src/bin/%.rs
	cargo run --quiet --release --bin $*

all: $(patsubst src/bin/%.rs,inputs/%.in,$(wildcard src/bin/*.rs))
	cargo run --quiet --release

help:
	@$(MAKE) --no-print-directory -C .. help
