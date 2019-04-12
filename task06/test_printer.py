#!/usr/bin/env python3
import pytest

from model import *
from printer import *


def _pretty_print(stmt):
    printer = PrettyPrinter()
    stmt.accept(printer)
    return printer.get_result()


def test_conditional():
    assert _pretty_print(Conditional(Number(42), [], [])) == '''\
if (42) {
}'''


def test_function_definition():
    assert _pretty_print(FunctionDefinition('foo', Function([], []))) == '''\
def foo() {
}'''


def test_print():
    assert _pretty_print(Print(Number(42))) == 'print 42;'


def test_read():
    assert _pretty_print(Read('x')) == 'read x;'


def test_number():
    assert _pretty_print(Number(10)) == '10;'


def test_reference():
    assert _pretty_print(Reference('x')) == 'x;'


def test_binary_operation():
    add = BinaryOperation(Number(2), '+', Number(3))
    mul = BinaryOperation(Number(1), '*', add)
    assert _pretty_print(mul) == '(1 * (2 + 3));'


def test_unary_operation():
    assert _pretty_print(UnaryOperation('-', Number(42))) == '(-(42));'


def test_function_call():
    stmt = FunctionCall(
        Reference('foo'),
        [
            Number(1),
            Number(2),
            Number(3)
        ]
    )
    assert _pretty_print(stmt) == 'foo(1, 2, 3);'


def test_end_to_end(capsys):
    stmt = FunctionDefinition('main', Function(['arg1'], [
        Read('x'),
        Print(Reference('x')),
        Conditional(
            BinaryOperation(Number(2), '==', Number(3)),
            [
                Conditional(Number(1), [], [])
            ],
            [
                FunctionCall(Reference('exit'), [
                    UnaryOperation('-', Reference('arg1'))
                ])
            ],
        ),
    ]))
    pretty_print(stmt)
    assert capsys.readouterr().out == '''\
def main(arg1) {
    read x;
    print x;
    if ((2 == 3)) {
        if (1) {
        }
    } else {
        exit((-(arg1)));
    }
}\n'''


if __name__ == '__main__':
    pytest.main()
