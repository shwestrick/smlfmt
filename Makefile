SOURCES = $(wildcard src/*.sml) $(wildcard src/*.mlb) $(wildcard src/**/*.sml) $(wildcard src/**/*.mlb)

main: $(SOURCES)
	mlton -mlb-path-var 'COMPAT mlton' -output main src/main.mlb

main.dbg: $(SOURCES)
	mlton -mlb-path-var 'COMPAT mlton' -const 'Exn.keepHistory true' -output main.dbg src/main.mlb

.PHONY: clean
clean:
	rm -f main
