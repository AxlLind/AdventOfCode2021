DAYS  := $(shell seq -w 25)
TODAY := $(shell date +%y%m%d)

# if we're in the AOC month, set default goal to today's problem
.DEFAULT_GOAL := $(if $(filter $(TODAY),$(DAYS:%=2312%)),$(TODAY:2312%=%),help)

inputs/%.in:
	./fetch.sh $*

src/bin/%.rs: inputs/%.in
	echo -e "\n\n"                                   > $@
	echo "#[aoc::main($*)]"                         >> $@
	echo "fn main(input: &str) -> (usize, usize) {" >> $@
	echo "  (0, 0)"                                 >> $@
	echo "}"                                        >> $@

.PHONY: $(DAYS)
$(DAYS): %: src/bin/%.rs
	cargo run --release --bin $*

.PHONY: run-all
run-all:
	cargo run --release

help:
	@echo 'usage: make [TARGET..]'
	@echo 'Makefile used to setup and run aoc'
	@echo
	@echo 'make           - run todays problem'
	@echo 'make [01..25]  - run a specific days solution'
	@echo 'make run-all   - run all days solutions'
