DAYS  := 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
.DEFAULT_GOAL := help
.PHONY: $(DAYS) all help

inputs/%.in:
	../fetch.sh 2015 $*

$(DAYS): %: inputs/%.in
	clj -M src/$*.clj

all: $(DAYS)

help:
	@$(MAKE) --no-print-directory -C .. help
