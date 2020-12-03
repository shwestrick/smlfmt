SOURCES = $(wildcard *.sml) $(wildcard *.mlb) $(wildcard **/*.sml) $(wildcard **/*.mlb)

main: $(SOURCES)
	mlton -mlb-path-var 'COMPAT mlton' main.mlb

.PHONY: clean
clean:
	rm -f main
