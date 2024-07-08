#include <stdio.h>

#include "lisp.h"
#include "utils.h"

int main(int argc, char **argv)
{
    FILE *in = stdin;
    if (argc > 1) {
        in = fopen(argv[1], "r");
        if (in == NULL)
            error("file %s not found", argv[1]);
    }
    Value v = parse(in);
    do {
        print(car(v));
        printf("\n");
        v = cdr(v);
    } while (!value_is_nil(v));
    return 0;
}