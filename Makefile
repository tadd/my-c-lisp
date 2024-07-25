CC=gcc
CFLAGS=-std=gnu17 -O0 -ggdb3 -Wall -Wextra $(XCFLAGS)
LIBS=-lm
ANALYZER=-fanalyzer

SRC_COMMON=lisp.c utils.c
SRC=$(SRC_COMMON) main.c
SRC_TEST=$(SRC_COMMON) test-lisp.c
OBJ=$(SRC:.c=.o)
OBJ_TEST=$(SRC_TEST:.c=.o)

all: lisp test

lisp: $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS)

test-lisp: $(OBJ_TEST)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS) -lcriterion

test: test-lisp
	./$<

clean:
	rm -f *.o lisp test-lisp

analyze: $(OBJ:.o=.analyzer)

%.o: %.c
	$(CC) $(CFLAGS) -c $<

%.analyzer: %.c
	$(CC) $(CFLAGS) $(ANALYZER) -c $< -o /dev/null

utils.o: utils.h
lisp.o main.o: lisp.h utils.h
test-lisp.o: lisp.h

.PHONY: all clean test analyze
