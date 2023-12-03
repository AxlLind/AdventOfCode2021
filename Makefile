DAYS  := $(shell seq -w 25)
TODAY := $(shell date +%y%m%d)

# if we're in the AOC month, set default goal to today's problem
.DEFAULT_GOAL := $(if $(filter $(TODAY),$(DAYS:%=2312%)),$(TODAY:2312%=%),help)
.PHONY: $(DAYS) all help

inputs/%.in:
	./fetch.sh $*

src/bin/%.rs: inputs/%.in
	echo "\n"                                       >> $@
	echo "#[aoc::main($*)]"                         >> $@
	echo "fn main(input: &str) -> (usize, usize) {" >> $@
	echo "  (0, 0)"                                 >> $@
	echo "}"                                        >> $@

$(DAYS): %: src/bin/%.rs
	cargo run --release --bin $*

all:
	cargo run --release

help:
	@echo 'usage: make [TARGET..]'
	@echo 'Makefile used to download input, setup files, and run aoc.'
	@echo
	@echo 'make           - run todays solution'
	@echo 'make [01..25]  - run a specific days solution'
	@echo 'make all       - run all days solutions'
