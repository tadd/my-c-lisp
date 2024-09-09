def cfuncall(name, *args):
    func = gdb.lookup_global_symbol(name).value()
    return func(*args)

class ValuePrinter:
    TYPE = gdb.lookup_type('Value')

    def __init__(self, val):
        self.val = val

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
