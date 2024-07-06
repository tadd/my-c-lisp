SRC=lisp.c utils.c
OBJ=$(SRC:.c=.o)
CFLAGS=-std=gnu2x -O0 -ggdb3 -Wall -Wextra

all: lisp

lisp: $(OBJ)
	gcc $(CFLAGS) -o $@ $^

%.o: %.c utils.h
	gcc $(CFLAGS) -c $<

clean:
	rm -f *.o lisp

.PHONY: all clean
