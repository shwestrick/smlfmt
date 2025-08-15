PREFIX ?= /usr/local

SOURCES = $(wildcard src/*.sml) $(wildcard src/*.mlb) $(wildcard src/**/*.sml) $(wildcard src/**/*.mlb)

# MLCOMP = mlton
MLCOMP = mpl

smlfmt: $(SOURCES)
	$(MLCOMP) -output smlfmt src/smlfmt.mlb

smlfmt.dbg: $(SOURCES)
	$(MLCOMP) -const 'Exn.keepHistory true' -output smlfmt.dbg src/smlfmt.mlb

demo: $(SOURCES)
	$(MLCOMP) -output demo src/demo.mlb

demo.dbg: $(SOURCES)
	$(MLCOMP) -const 'Exn.keepHistory true' -output demo.dbg src/demo.mlb

.PHONY: install
install: smlfmt
	install -d $(DESTDIR)$(PREFIX)/bin/
	install -m 755 smlfmt $(DESTDIR)$(PREFIX)/bin/

.PHONY: test
test: smlfmt
	test/runall

.PHONY: clean
clean:
	rm -f demo smlfmt
