SRC=main.c lisp.c utils.c
OBJ=$(SRC:.c=.o)
CFLAGS=-std=gnu17 -O0 -ggdb3 -Wall -Wextra $(XCFLAGS)
LIBS=-lm
ANALYZER=-fanalyzer

all: lisp test

lisp: $(OBJ)
	gcc $(CFLAGS) -o $@ $^ $(LIBS)

test_lisp: lisp.o utils.o test_lisp.o
	gcc $(CFLAGS) -o $@ $^ $(LIBS) -lcriterion

%.o: %.c lisp.h utils.h
	gcc $(CFLAGS) -c $<

test: test_lisp
	./$<

clean:
	rm -f *.o lisp test_lisp

%.analyzer: %.c
	$(CC) $(CFLAGS) $(ANALYZER) -c $< -o /dev/null

analyze: $(OBJ:.o=.analyzer)

.PHONY: all clean test analyze
