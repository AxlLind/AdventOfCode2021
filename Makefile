DAYS  := $(shell seq -w 25)
TODAY := $(shell date +%y%m%d)

# if we're in the AOC month, set default goal to today's problem
.DEFAULT_GOAL := $(if $(filter $(TODAY),$(DAYS:%=2312%)),$(TODAY:2312%=%),help)
.PHONY: $(DAYS) all help

inputs/%.in:
	./fetch.sh $*

src/bin/%.rs:
	DAY=$* envsubst < src/template.rs > $@

#@ run a specific day, e.g 01
$(DAYS): %: src/bin/%.rs inputs/%.in
	cargo run --quiet --release --bin $*

#@ run all days
all:
	cargo run --quiet --release

#@ show this help text
help:
	@echo 'usage: make [TARGET..]'
	@echo 'Makefile used to automatically download input, setup files, and run solutions.'
	@echo
	@echo 'TARGET:'
	@awk '{                                           \
	  if (desc ~ /^#@ /)                              \
	    printf "  %s%s\n", $$1, substr(desc, 4, 100); \
	  desc = $$0                                      \
	}' $(MAKEFILE_LIST) | column -t -s ':'
ifneq ($(.DEFAULT_GOAL),help)
	@echo
	@echo "Type 'make' to automatically run today's solution."
endif
