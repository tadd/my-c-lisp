SRC=main.c lisp.c utils.c
OBJ=$(SRC:.c=.o)
CFLAGS=-std=gnu2x -O0 -ggdb3 -Wall -Wextra $(XCFLAGS)

all: lisp

lisp: $(OBJ)
	gcc $(CFLAGS) -o $@ $^

%.o: %.c lisp.h utils.h
	gcc $(CFLAGS) -c $<

clean:
	rm -f *.o lisp

.PHONY: all clean
