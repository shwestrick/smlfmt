SOURCES = $(wildcard *.sml) $(wildcard *.mlb) $(wildcard **/*.sml) $(wildcard **/*.mlb)

main: $(SOURCES)
	mlton -mlb-path-var 'COMPAT mlton' main.mlb

# main: $(SOURCES)
# 	mlton -mlb-path-var 'COMPAT mlton' -const 'Exn.keepHistory true' main.mlb

.PHONY: clean
clean:
	rm -f main
