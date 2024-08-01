def highlight(s):
    return "\033[36m" + s + "\033[m"

def cfuncall(name, *args):
    func = gdb.lookup_global_symbol(name).value()
    return func(*args)

class ValuePrinter:
    TYPE = gdb.lookup_type('Value')
    TAG_LABELS = '''
    TAG_PAIR
    TAG_STR
    TAG_CFUNC
    TAG_SPECIAL
    TAG_CLOSURE
    '''.split()
    TAGS = dict([[l, int(gdb.lookup_static_symbol(l).value())] for l in TAG_LABELS])

    def __init__(self, val):
        self.val = val
        #self.tag = int(cfuncall('VALUE_TAG', val));

    def to_string(self):
        return cfuncall('stringify', self.val)

class PP (gdb.Command):
    def __init__(self):
        super(type(self), self).__init__('pp', gdb.COMMAND_DATA)
    def invoke(self, arg, from_tty):
        val = gdb.parse_and_eval(arg)
        pp = my_c_lisp_pp(val)
        if pp:
            print(pp.to_string())
        else:
            print(str(val))
PP()

def my_c_lisp_pp(val):
    if val.type == ValuePrinter.TYPE:
        return ValuePrinter(val)
    return None

gdb.pretty_printers.append(my_c_lisp_pp)
