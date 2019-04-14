#!/usr/bin/env python3
import pytest

from model import *
from printer import *
from textwrap import dedent


def test_conditional():
    pretty_printer = PrettyPrinter()
    Conditional(Number(42), [], []).accept(pretty_printer)
    assert pretty_printer.result == dedent('''\
    if (42) {
    }''')


def test_function_definition():
    pretty_printer = PrettyPrinter()
    FunctionDefinition('foo', Function([], [])).accept(pretty_printer)
    assert pretty_printer.result == dedent('''\
    def foo() {
    }''')


def test_print():
    pretty_printer = PrettyPrinter()
    Print(Number(42)).accept(pretty_printer)
    assert pretty_printer.result == 'print 42'


def test_read():
    pretty_printer = PrettyPrinter()
    Read('x').accept(pretty_printer)
    assert pretty_printer.result == 'read x'


def test_number():
    pretty_printer = PrettyPrinter()
    Number(10).accept(pretty_printer)
    assert pretty_printer.result == '10'


def test_reference():
    pretty_printer = PrettyPrinter()
    Reference('x').accept(pretty_printer)
    assert pretty_printer.result == 'x'


def test_binary_operation():
    pretty_printer = PrettyPrinter()
    add = BinaryOperation(Number(2), '+', Number(3))
    BinaryOperation(Number(1), '*', add).accept(pretty_printer)
    assert pretty_printer.result == '(1 * (2 + 3))'


def test_unary_operation():
    pretty_printer = PrettyPrinter()
    UnaryOperation('-', Number(42)).accept(pretty_printer)
    assert pretty_printer.result == '(-(42))'


def test_function_call():
    pretty_printer = PrettyPrinter()
    FunctionCall(
        Reference('foo'),
        [
            Number(1),
            Number(2),
            Number(3)
        ]
    ).accept(pretty_printer)
    assert pretty_printer.result == 'foo(1, 2, 3)'


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
    assert capsys.readouterr().out == dedent('''\
    def main(arg1) {
        read x;
        print x;
        if ((2 == 3)) {
            if (1) {
            }
        } else {
            exit((-(arg1)));
        }
    }
    ''')


if __name__ == '__main__':
    pytest.main()
