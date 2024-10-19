CC=gcc
OPTFLAGS=-O0 -ggdb3
CFLAGS=-std=gnu17 -Wall -Wextra $(OPTFLAGS) $(XCFLAGS)
LIBS=-lm
ANALYZER=-fanalyzer
SANITIZER=-fsanitize=undefined #,address

OBJ_COMMON=lisp.o utils.o
OBJ=$(OBJ_COMMON) main.o
OBJ_TEST=$(OBJ_COMMON) basic-test.o

all: lisp test

lisp: $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS)

basic-test: $(OBJ_TEST)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS) -lcriterion

test: test-c test-scheme

test-c: basic-test
	./$<
test-c-san: basic-test-san
	./$<

test-scheme: lisp
	./$< test.scm
test-scheme-san: lisp-san
	./$< test.scm

clean:
	rm -f lisp basic-test *-san *.o

analyze: $(OBJ:.o=.analyzer)

sanitize: lisp-san test-san
test-san: test-c-san test-scheme-san

lisp-san: $(OBJ:.o=.san.o)
	$(CC) $(CFLAGS) $(SANITIZER) -o $@ $^ $(LIBS)

basic-test-san: $(OBJ_TEST:.o=.san.o)
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
basic-test.o: lisp.h

.PHONY: all clean test test-c test-scheme analyze sanitize \
	test-san test-c-san test-scheme-san
