from model import ASTNodeVisitor


class PrettyPrinter(ASTNodeVisitor):
    def __init__(self):
        self.result = ''
        self.offset = 0

    def add_new_line(self):
        self.result += '\n'
        self.result += '    ' * self.offset

    def visit_number(self, number):
        self.result += str(number.value)

    def visit_function(self, function):
        raise TypeError("PrettyPrinter must not visit Function object")

    def visit_block(self, block):
        self.result += '{'
        self.offset += 1
        for stmt in block or []:
            self.add_new_line()
            stmt.accept(self)
            if not self.result.endswith('}'):
                self.result += ';'
        self.offset -= 1
        self.add_new_line()
        self.result += '}'

    def visit_function_definition(self, function_definition):
        self.result += 'def ' + function_definition.name + '('
        self.result += ', '.join(function_definition.function.args) + ') '
        self.visit_block(function_definition.function.body)

    def visit_conditional(self, conditional):
        self.result += 'if ('
        conditional.condition.accept(self)
        self.result += ') '
        self.visit_block(conditional.if_true)
        if conditional.if_false:
            self.result += ' else '
            self.visit_block(conditional.if_false)

    def visit_print(self, _print):
        self.result += 'print '
        _print.expr.accept(self)

    def visit_read(self, read):
        self.result += 'read ' + read.name

    def visit_function_call(self, function_call):
        function_call.fun_expr.accept(self)
        self.result += '('
        for i, arg in enumerate(function_call.args):
            if i:
                self.result += ', '
            arg.accept(self)
        self.result += ')'

    def visit_reference(self, reference):
        self.result += reference.name

    def visit_binary_operation(self, binary_operation):
        self.result += '('
        binary_operation.lhs.accept(self)
        self.result += ' ' + binary_operation.op + ' '
        binary_operation.rhs.accept(self)
        self.result += ')'

    def visit_unary_operation(self, unary_operation):
        self.result += '(' + unary_operation.op + '('
        unary_operation.expr.accept(self)
        self.result += 2 * ')'


def pretty_print(stmt):
    printer = PrettyPrinter()
    stmt.accept(printer)
    result = printer.result
    if not result.endswith('}'):
        result += ';'
    print(result)
