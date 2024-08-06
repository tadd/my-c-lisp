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
    fprintf(out, "Usage: lisp [-hepP] <file>\n");
    fprintf(out, "  -h\t\tprint this help\n");
    fprintf(out, "  -e <source>\tevaluate <source> directly instead of <file>\n");
    fprintf(out, "  -p\t\tprint last expression in the input\n");
    fprintf(out, "  -P\t\tonly parse then exit before evaluation\n");
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
    bool parse_only;
} Option;

static Option parse_opt(int argc, char *const *argv)
{
    Option o = { .in = NULL, .print = false, .parse_only = false };
    int opt;
    while ((opt = getopt(argc, argv, "e:hPp")) != -1) {
        switch (opt) {
        case 'e':
            o.in = fmemopen(optarg, strlen(optarg), "r");
            break;
        case 'h':
            usage(stdout);
        case 'P':
            o.parse_only = o.print = true;
            break;
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
    Value v;
    if (opt.parse_only)
        v = parse(opt.in);
    else
        v = load(opt.in);
    fclose(opt.in);
    if (v == Qundef)
        error("%s", error_message());
    if (opt.print) {
        display(v);
        printf("\n");
    }
    return 0;
}
