MLCOMP ?= mlkit

.PHONY: all
all:
	$(MLCOMP) set.mlb
	$(MLCOMP) mono_set.mlb
	$(MLCOMP) string_set.mlb
	$(MLCOMP) int_set.mlb
	$(MLCOMP) word_set.mlb
	$(MLCOMP) mono_map.mlb
	$(MLCOMP) string_map.mlb
	$(MLCOMP) int_map.mlb

.PHONY: clean
clean:
	rm -rf run MLB *~ */*~ */MLB
