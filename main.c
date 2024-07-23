#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "lisp.h"
#include "utils.h"

ATTR_NORETURN
static void usage(FILE *out)
{
    fprintf(out, "Usage: lisp <file.scm>\n");
    fprintf(out, "   or: lisp -e '(direct (program (here)))'\n");
    exit(out == stdout ? 0 : 2);
}

static void opt_error(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    fprintf(stderr, "error: ");
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    usage(stderr);
}

typedef struct {
    FILE *in;
    bool print;
} Option;

static Option parse_opt(int argc, char *const *argv)
{
    Option o = { .in = NULL, .print = false };
    int opt;
    while ((opt = getopt(argc, argv, "e:hp")) != -1) {
        switch (opt) {
        case 'e':
            o.in = fmemopen(optarg, strlen(optarg), "r");
            break;
        case 'h':
            usage(stdout);
        case 'p':
            o.print = true;
            break;
        case '?':
            usage(stderr);
        }
    }
    const char *f = argv[optind];
    if (f == NULL && o.in == NULL)
        opt_error("no program provided");
    if (f != NULL && o.in != NULL)
        opt_error("filename %s given while option '-e' passed", f);
    if (f != NULL) {
        o.in = fopen(f, "r");
        if (o.in == NULL)
            opt_error("can't read file %s", f);
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
