SRC=lisp.c
OBJ=$(SRC:.c=.o)
CFLAGS=-std=gnu2x -Og -ggdb3 -Wall -Wextra

all: lisp

lisp: $(OBJ)
	gcc $(CFLAGS) -o $@ $<

%.o: %.c
	gcc $(CFLAGS) -c $<

clean:
	rm -f *.o lisp

.PHONY: all clean
