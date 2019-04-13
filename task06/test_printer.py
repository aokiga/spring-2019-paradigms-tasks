#!/usr/bin/env python3
import pytest

from model import *
from printer import *
from textwrap import dedent


def test_conditional(capsys):
    stmt = Conditional(Number(42), [], [])
    pretty_print(stmt)
    assert capsys.readouterr().out == dedent('''\
    if (42) {
    }
    ''')


def test_function_definition(capsys):
    stmt = FunctionDefinition('foo', Function([], []))
    pretty_print(stmt)
    assert capsys.readouterr().out == dedent('''\
    def foo() {
    }
    ''')


def test_print(capsys):
    stmt = Print(Number(42))
    pretty_print(stmt)
    assert capsys.readouterr().out == 'print 42;\n'


def test_read(capsys):
    stmt = Read('x')
    pretty_print(stmt)
    assert capsys.readouterr().out == 'read x;\n'


def test_number(capsys):
    stmt = Number(10)
    pretty_print(stmt)
    assert capsys.readouterr().out == '10;\n'


def test_reference(capsys):
    stmt = Reference('x')
    pretty_print(stmt)
    assert capsys.readouterr().out == 'x;\n'


def test_binary_operation(capsys):
    add = BinaryOperation(Number(2), '+', Number(3))
    mul = BinaryOperation(Number(1), '*', add)
    pretty_print(mul)
    assert capsys.readouterr().out == '(1 * (2 + 3));\n'


def test_unary_operation(capsys):
    stmt = UnaryOperation('-', Number(42))
    pretty_print(stmt)
    assert capsys.readouterr().out == '(-(42));\n'


def test_function_call(capsys):
    stmt = FunctionCall(
        Reference('foo'),
        [
            Number(1),
            Number(2),
            Number(3)
        ]
    )
    pretty_print(stmt)
    assert capsys.readouterr().out == 'foo(1, 2, 3);\n'


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
