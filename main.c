#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include "lisp.h"
#include "utils.h"

ATTR(noreturn)
static void usage(FILE *out)
{
    fprintf(out, "Usage: lisp [-e <source>] [-pPTh] <file>\n");
    fprintf(out, "  -e <source>\tevaluate <source> directly instead of <file>\n");
    fprintf(out, "  -p\t\tprint last expression in the input\n");
    fprintf(out, "  -P\t\tonly parse then exit before evaluation. implies -p\n");
    fprintf(out, "  -T\t\tprint consumed CPU time at exit\n");
    fprintf(out, "  -h\t\tprint this help\n");
    exit(out == stdout ? 0 : 2);
}

ATTR(format(printf, 1, 2))
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
    const char *path;
    const char *script;
    bool print;
    bool parse_only;
    bool cputime;
} Option;

static Option parse_opt(int argc, char *const *argv)
{
    Option o = {
        .path = NULL,
        .script = NULL,
        .print = false,
        .parse_only = false,
        .cputime = false,
    };
    int opt;
    while ((opt = getopt(argc, argv, "e:hPpT")) != -1) {
        switch (opt) {
        case 'e':
            o.script = optarg;
            break;
        case 'h':
            usage(stdout);
        case 'P':
            o.parse_only = o.print = true;
            break;
        case 'p':
            o.print = true;
            break;
        case 'T':
            o.cputime = true;
            break;
        case '?':
            usage(stderr);
        }
    }
    o.path = argv[optind];
    if (o.path == NULL && o.script == NULL)
        opt_error("no program provided");
    if (o.path != NULL && o.script != NULL)
        opt_error("filename %s given while option '-e' passed", o.path);
    return o;
}

static double cputime_ms(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts);
    return ts.tv_sec * 1000.0 + ts.tv_nsec / (1000.0*1000.0);
}

int main(int argc, char **argv)
{
    Option o = parse_opt(argc, argv);
    Value v;
    if (o.parse_only)
        v = o.script ? parse_string(o.script) : parse(o.path);
    else
        v = o.script ? eval_string(o.script) : load(o.path);
    if (v == Qundef)
        error("%s", error_message());
    if (o.print) {
        display(v);
        printf("\n");
    }
    if (o.cputime)
        fprintf(stderr, "CPU: %.3lf ms\n", cputime_ms());
    return 0;
}
