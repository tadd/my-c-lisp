#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "lisp.h"
#include "utils.h"

static FILE *parse_opt(int argc, char *const *argv)
{
    FILE *in = NULL;
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
        const char *f = argv[optind];
        if (in != NULL)
            error("filename %s given while option '-e' passed", f);
        in = fopen(f, "r");
        if (in == NULL)
            error("file %s not found", f);
    }
    return in ? in : stdin;
}

int main(int argc, char **argv)
{
    FILE *in = parse_opt(argc, argv);
    print(load(in));
    printf("\n");
    fclose(in);
    return 0;
}
