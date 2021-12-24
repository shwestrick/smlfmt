SOURCES = $(wildcard src/*.sml) $(wildcard src/*.mlb) $(wildcard src/**/*.sml) $(wildcard src/**/*.mlb)

smlfmt: $(SOURCES)
	mlton -mlb-path-var 'COMPAT mlton' -output smlfmt src/smlfmt.mlb

demo: $(SOURCES)
	mlton -mlb-path-var 'COMPAT mlton' -output demo src/demo.mlb

demo.dbg: $(SOURCES)
	mlton -mlb-path-var 'COMPAT mlton' -const 'Exn.keepHistory true' -output demo.dbg src/demo.mlb

.PHONY: clean
clean:
	rm -f demo smlfmt
