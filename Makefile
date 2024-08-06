CC=gcc
CFLAGS=-std=gnu17 -O0 -ggdb3 -Wall -Wextra $(XCFLAGS)
LIBS=-lm
ANALYZER=-fanalyzer
SANITIZER=-fsanitize=undefined #,address

SRC_COMMON=lisp.c utils.c
SRC=$(SRC_COMMON) main.c
SRC_TEST=$(SRC_COMMON) test-lisp.c
OBJ=$(SRC:.c=.o)
OBJ_TEST=$(SRC_TEST:.c=.o)

all: lisp test

mymemcpy.s: mymemcpy.c
	$(CC) $(CFLAGS) -fno-builtin -O3 -march=native -S -fverbose-asm -o $@ $<

lisp: $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS)

test-lisp: $(OBJ_TEST)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS) -lcriterion

test: test-lisp
	./$<

clean:
	rm -f *.o lisp test-lisp *-san *.s

analyze: $(OBJ:.o=.analyzer)

sanitize: test-lisp-san lisp-san
	./$<

lisp-san: $(OBJ:.o=.san.o)
	$(CC) $(CFLAGS) $(SANITIZER) -o $@ $^ $(LIBS)

test-lisp-san: $(OBJ_TEST:.o=.san.o)
	$(CC) $(CFLAGS) $(SANITIZER) -o $@ $^ $(LIBS) -lcriterion

%.o: %.c
	$(CC) $(CFLAGS) -c $<

%.analyzer: %.c
	$(CC) $(CFLAGS) $(ANALYZER) -c $< -o /dev/null

%.san.o: %.c
	$(CC) $(CFLAGS) $(SANITIZER) -c $< -o $@

utils.o: utils.h
lisp.o main.o: lisp.h utils.h
test-lisp.o: lisp.h

.PHONY: all clean test analyze sanitize
