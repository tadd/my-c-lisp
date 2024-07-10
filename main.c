#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "lisp.h"
#include "utils.h"

static FILE *parse_opt(int argc, char *const *argv)
{
    FILE *in = stdin;
    int opt;
    while ((opt = getopt(argc, argv, "e:")) != -1) {
        switch (opt) {
        case 'e':
            in = fmemopen(optarg, strlen(optarg), "r");
            break;
        case '?':
            exit(2);
        }
    }
    if (argv[optind]) {
        in = fopen(argv[1], "r");
        if (in == NULL)
            error("file %s not found", argv[1]);
    }
    return in;
}

int main(int argc, char **argv)
{
    FILE *in = parse_opt(argc, argv);
    Value v = parse(in);
    while (!value_is_nil(v)) {
        print(car(v));
        printf("\n");
        v = cdr(v);
    }
    fclose(in);
    return 0;
}
