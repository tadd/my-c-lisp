#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "lisp.h"
#include "utils.h"

int main(int argc, char **argv)
{
    FILE *in = stdin;
    int opt;
    while ((opt = getopt(argc, argv, "e:")) != -1) {
        switch (opt) {
        case 'e':
            in = fmemopen(optarg, strlen(optarg), "r");
            break;
        case '?':
            return 2;
        }
    }
    if (argv[optind]) {
        in = fopen(argv[1], "r");
        if (in == NULL)
            error("file %s not found", argv[1]);
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
