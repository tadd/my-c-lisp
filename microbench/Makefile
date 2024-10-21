BIN=../lisp
SRC!=ls src/*.erb
TARGET=$(SRC:src/%.erb=%)
TIMEBIN=/bin/time -f '%U' $(BIN) 2>&1
NLOOP=5

all: benchmark

target: $(TARGET)

benchmark: $(TARGET)
	@for t in $^; do \
	    echo $$t:; \
	    for i in `seq ${NLOOP}`; do \
	        $(TIMEBIN) $(TARGET); \
	    done \
	done

%.scm: src/%.scm.erb
	erb -T - $< > $@

clean:
	rm -f $(TARGET)

.PHONY: all target benchmark clean
