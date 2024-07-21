#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "lisp.h"
#include "utils.h"

typedef struct {
    FILE *in;
    bool print;
} Option;

static Option parse_opt(int argc, char *const *argv)
{
    Option o = { .in = NULL, .print = false };
    int opt;
    while ((opt = getopt(argc, argv, "e:p")) != -1) {
        switch (opt) {
        case 'e':
            o.in = fmemopen(optarg, strlen(optarg), "r");
            break;
        case 'p':
            o.print = true;
            break;
        case '?':
            exit(2);
        }
    }
    const char *f = argv[optind];
    if (f) {
        if (o.in != NULL)
            error("filename %s given while option '-e' passed", f);
        o.in = fopen(f, "r");
        if (o.in == NULL)
            error("can't read file %s", f);
    }
    return o;
}
int main(int argc, char **argv)
{
    Option opt = parse_opt(argc, argv);
    Value v = load(opt.in);
    fclose(opt.in);
    if (v == Qundef)
        error("%s", error_message());
    if (opt.print) {
        display(v);
        printf("\n");
    }
    return 0;
}
