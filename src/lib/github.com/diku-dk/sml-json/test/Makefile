MLCOMP ?= mlkit

.PHONY: test
test: json_test.res
	cat $^

%.res: %.out
	@(diff -aq $< $<.ok > /dev/null 2>&1; \
         if [ $$? -eq 0 ]; then \
             echo "OK:  $*" > $@ \
         ; else \
             if [ -e $<.ok ]; then \
                echo "ERR: $* - file $< differs from $<.ok"; \
                echo "ERR: $* - file $< differs from $<.ok" > $@ \
             ; else \
                echo "ERR: $* - file $<.ok does not exist"; \
                echo "ERR: $* - file $<.ok does not exist" > $@ \
             ; fi \
             ; exit 1 \
         ;fi)

%.out: %.exe
	./$< > $@

%.exe: %.mlb
	$(MLCOMP) -output $@ $<

.PHONY: clean
clean:
	rm -rf MLB *.out *~ *.exe *.res run
