from model import *


class ConstantFolder(ASTNodeVisitor):
    def visit_number(self, number):
        return number

    def visit_function(self, function):
        body = [stmt.accept(self) for stmt in function.body]
        return Function(function.args, body)

    def visit_function_definition(self, function_definition):
        return FunctionDefinition(
            function_definition.name,
            function_definition.function.accept(self))

    def visit_conditional(self, conditional):
        condition = conditional.condition.accept(self)
        if_true = [stmt.accept(self) for stmt in conditional.if_true or []]
        if_false = [stmt.accept(self) for stmt in conditional.if_false or []]
        return Conditional(condition, if_true, if_false)

    def visit_print(self, _print):
        return Print(_print.expr.accept(self))

    def visit_read(self, read):
        return read

    def visit_function_call(self, function_call):
        fun_expr = function_call.fun_expr.accept(self)
        args = [arg.accept(self) for arg in function_call.args]
        return FunctionCall(fun_expr, args)

    def visit_reference(self, reference):
        return reference

    def visit_binary_operation(self, binary_operation):
        lhs = binary_operation.lhs.accept(self)
        op = binary_operation.op
        rhs = binary_operation.rhs.accept(self)

        if isinstance(lhs, Number) and isinstance(rhs, Number):
            return BinaryOperation(lhs, op, rhs).evaluate(None)

        if lhs == Number(0) and isinstance(rhs, Reference) and op == '*':
            return Number(0)

        if isinstance(lhs, Reference) and rhs == Number(0) and op == '*':
            return Number(0)

        if isinstance(lhs, Reference) and lhs == rhs and op == '-':
            return Number(0)

        return BinaryOperation(lhs, op, rhs)

    def visit_unary_operation(self, unary_operation):
        expr = unary_operation.expr.accept(self)
        op = unary_operation.op

        if isinstance(expr, Number):
            return UnaryOperation(op, expr).evaluate(None)

        return UnaryOperation(op, expr)


def fold_constants(program):
    folder = ConstantFolder()
    return program.accept(folder)
