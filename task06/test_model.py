#!/usr/bin/env python3
import pytest
from model import *


def test_scope_parentless():
    scope = Scope()
    scope[17] = 17
    scope['word'] = 'word'
    assert scope[17] == 17
    assert scope['word'] == 'word'


def test_scope_parent():
    parent = Scope()
    scope = Scope(parent)
    parent['car'] = 'volga'
    assert scope['car'] == 'volga'
    scope['car'] = 'lada'
    assert scope['car'] == 'lada'


def test_scope_grandparent():
    grandparent = Scope()
    parent = Scope(grandparent)
    scope = Scope(parent)
    grandparent['name'] = 'ded'
    assert scope['name'] == 'ded'
    scope['name'] = 'insert your text'
    assert scope['name'] == 'insert your text'


def test_scope_error():
    parent = Scope()
    scope = Scope(parent)
    scope['name'] = 'name'
    parent['car'] = None
    with pytest.raises(KeyError):
        age = scope['age']


def test_function_definition():
    scope = Scope()
    func = Function(['args'], ['something'])
    FunctionDefinition('func', func).evaluate(scope)
    assert scope['func'] is func


def test_conditional():
    scope = Scope()
    block_true = [Number(17)]
    block_false = [Number(1709)]
    assert Conditional(Number(1), block_true, block_false).evaluate(scope)\
        == block_true[-1]
    assert Conditional(Number(0), block_true, block_false).evaluate(scope)\
        == block_false[-1]


def test_conditional_empty():
    scope = Scope()
    block_true = []
    block_false = None
    assert Conditional(Number(1), block_true, block_false).evaluate(scope)\
        is None
    assert Conditional(Number(0), block_true, block_false).evaluate(scope)\
        is None


def test_print(capsys):
    scope = Scope()
    assert Print(Number(17)).evaluate(scope) == Number(17)
    assert capsys.readouterr().out == '17\n'


def test_read(monkeypatch):
    monkeypatch.setattr('builtins.input', lambda: '17')
    scope = Scope()
    assert Read('x').evaluate(scope) == Number(17)
    assert scope['x'] == Number(17)


def test_function_call():
    scope = Scope()
    func = Function(['x'], [Reference('x')])
    assert FunctionCall(func, [Number(17)]).evaluate(scope) == Number(17)


def test_reference():
    scope = Scope()
    scope['17'] = Number(1709)
    assert Reference('17').evaluate(scope) == Number(1709)


def test_binary_operation():
    scope = Scope()
    assert BinaryOperation(Number(1709), '+', Number(17)).evaluate(scope)\
        == Number(1726)
    assert BinaryOperation(Number(1709), '-', Number(17)).evaluate(scope)\
        == Number(1692)
    assert BinaryOperation(Number(1709), '/', Number(17)).evaluate(scope)\
        == Number(100)
    assert BinaryOperation(Number(1709), '%', Number(17)).evaluate(scope)\
        == Number(9)
    assert BinaryOperation(Number(1), '==', Number(0)).evaluate(scope)\
        == Number(0)
    assert BinaryOperation(Number(1709), '>=', Number(17)).evaluate(scope)\
        == Number(1)
    assert BinaryOperation(Number(0), '||', Number(1)).evaluate(scope)\
        == Number(1)
    assert BinaryOperation(Number(0), '&&', Number(1)).evaluate(scope)\
        == Number(0)


def test_unary_operation():
    scope = Scope()
    assert UnaryOperation('-', Number(17)).evaluate(scope).value == -17
    assert UnaryOperation('!', Number(1)).evaluate(scope).value == 0


def test_factorial():
    scope = Scope()
    fact = Function(
        ['x'],
        [Conditional(
            BinaryOperation(
                Reference('x'),
                '==',
                Number(1)
            ),
            [Number(1)],
            [BinaryOperation(
                FunctionCall(
                    Reference('f'),
                    [BinaryOperation(
                        Reference('x'),
                        '-',
                        Number(1)
                    )]
                ),
                '*',
                Reference('x')
            )]
        )]
    )
    FunctionDefinition('f', fact).evaluate(scope)
    assert FunctionCall(Reference('f'), [Number(17)]).evaluate(scope)\
        == Number(355687428096000)


if __name__ == "__main__":
    pytest.main()
