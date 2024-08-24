CC=gcc
CFLAGS=-std=gnu17 -O0 -ggdb3 -Wall -Wextra $(XCFLAGS)
LIBS=-lm
ANALYZER=-fanalyzer
SANITIZER=-fsanitize=undefined #,address

SRC_COMMON=lisp.c utils.c
SRC=$(SRC_COMMON) main.c
SRC_TEST=$(SRC_COMMON) test-basic.c
OBJ=$(SRC:.c=.o)
OBJ_TEST=$(SRC_TEST:.c=.o)

all: lisp test

lisp: $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS)

test-basic: $(OBJ_TEST)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS) -lcriterion

test: test-c test-scheme

test-c: test-basic
	./$<
test-c-san: test-basic-san
	./$<

test-scheme: lisp
	./$< test/test.scm
test-scheme-san: lisp-san
	./$< test/test.scm

clean:
	rm -f lisp test-basic *-san *.o *.s

analyze: $(OBJ:.o=.analyzer)

sanitize: lisp-san test-san
test-san: test-c-san test-scheme-san

lisp-san: $(OBJ:.o=.san.o)
	$(CC) $(CFLAGS) $(SANITIZER) -o $@ $^ $(LIBS)

test-basic-san: $(OBJ_TEST:.o=.san.o)
	$(CC) $(CFLAGS) $(SANITIZER) -o $@ $^ $(LIBS) -lcriterion

%.o: %.c
	$(CC) $(CFLAGS) -c $<

%.s: %.c
	$(CC) $(CFLAGS) -S -fverbose-asm -c $<

%.analyzer: %.c
	$(CC) $(CFLAGS) $(ANALYZER) -c $< -o /dev/null

%.san.o: %.c
	$(CC) $(CFLAGS) $(SANITIZER) -c $< -o $@

utils.o: utils.h
lisp.o main.o: lisp.h utils.h
test-basic.o: lisp.h

.PHONY: all clean test test-c test-scheme analyze sanitize
