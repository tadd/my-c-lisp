#include <stdio.h>
#include <string.h>

#include "lisp.h"
#include "utils.h"

int main(int argc, char **argv)
{
    FILE *in = stdin;
    switch (argc) {
    case 3:
        if (strcmp(argv[1], "-e") != 0)
            break;
        char *src = argv[2];
        in = fmemopen(src, sizeof(src), "r");
        break;
    case 2:
        in = fopen(argv[1], "r");
        if (in == NULL)
            error("file %s not found", argv[1]);
        break;
    default:
        break;
    }
    Value v = parse(in);
    if (value_is_nil(v)) {
        return 0;
    }
    do {
        print(car(v));
        printf("\n");
        v = cdr(v);
    } while (!value_is_nil(v));

    fclose(in);
    return 0;
}
