SOURCES = $(wildcard *.sml) $(wildcard *.mlb) $(wildcard **/*.sml) $(wildcard **/*.mlb)

main: $(SOURCES)
	mlton -mlb-path-var 'COMPAT mlton' main.mlb

main.dbg: $(SOURCES)
	mlton -mlb-path-var 'COMPAT mlton' -const 'Exn.keepHistory true' -output main.dbg main.mlb

.PHONY: clean
clean:
	rm -f main
