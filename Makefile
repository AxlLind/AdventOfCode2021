DAYS  := 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
TODAY := $(shell date +%y%m%d)

# if we're in the AOC month, set default goal to today's problem
.DEFAULT_GOAL := $(if $(filter $(TODAY),$(DAYS:%=2312%)),$(TODAY:2312%=%),help)
.PHONY: $(DAYS) all help

inputs/%.in:
	./fetch.sh $*

src/bin/%.rs:
	DAY=$* envsubst < src/template.rs > $@

$(DAYS): %: src/bin/%.rs inputs/%.in
	cargo run --quiet --release --bin $*

all:
	cargo run --quiet --release

help:
	@echo 'usage: make [TARGET..]'
	@echo 'Automatically downloads input, sets up files, and runs solutions.'
	@echo
	@echo 'TARGET:'
	@echo '  {01..25}  run a specific day, e.g 01'
	@echo '  all       run all days'
	@echo '  help      show this help text'
	@echo
	@echo "During the AoC month 'make' will run the current day's solution"
