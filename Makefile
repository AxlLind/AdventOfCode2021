DAYS  := $(shell seq -w 25)
TODAY := $(shell date +%y%m%d)
.DEFAULT_GOAL := $(if $(filter $(TODAY),$(DAYS:%=2312%)),$(TODAY:2312%=%),run-all)

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
