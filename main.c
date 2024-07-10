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

static const char *const OPTION_EXISTS = (void *)1U;
typedef struct {
    const char *tbl[127]; // non-extended ASCII
    int index;
} GetOption;

static GetOption getoption(int argc, char *const *argv, const char *optstr)
{
    GetOption o;
    int opt;
    while ((opt = getopt(argc, argv, optstr)) != -1) {
        o.tbl[opt] = optarg ? optarg : OPTION_EXISTS;
    }
    if (o.tbl['?'])
        exit(2);
    o.index = optind;
    return o;
}

typedef struct {
    FILE *in;
    bool print;
    bool parse_only;
} Option;

static Option parse_opt(int argc, char *const *argv)
{
    Option o = { .in = NULL, .print = false, .parse_only = false };
    GetOption opt = getoption(argc, argv, "e:hPp");
    if (opt.tbl['e']) {
        const char *s = opt.tbl['e'];
        o.in = fmemopen((char *) s, strlen(s), "r");
    }
    if (opt.tbl['h'])
        usage(stdout);
    if (opt.tbl['P'])
        o.parse_only = o.print = true;
    if (opt.tbl['p'])
        o.print = true;
    if (opt.tbl['?'])
        usage(stderr);
    const char *f = argv[opt.index];
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
