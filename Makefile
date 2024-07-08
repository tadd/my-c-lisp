SRC=main.c lisp.c utils.c
OBJ=$(SRC:.c=.o)
CFLAGS=-std=gnu2x -O0 -ggdb3 -Wall -Wextra $(XCFLAGS)

all: lisp

lisp: $(OBJ)
	gcc $(CFLAGS) -o $@ $^

test_lisp: lisp.o utils.o test_lisp.o
	gcc $(CFLAGS) -o $@ $^ -lcriterion

%.o: %.c lisp.h utils.h
	gcc $(CFLAGS) -c $<

test: test_lisp
	./$<

clean:
	rm -f *.o lisp test_lisp

.PHONY: all clean test
