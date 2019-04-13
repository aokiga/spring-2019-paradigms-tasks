#!/usr/bin/env python3
import pytest

from model import *
from folder import fold_constants


def test_bin_op_two_num():
    assert fold_constants(
        BinaryOperation(
            Number(1),
            '+',
            Number(2)
        )
    ) == Number(3)


def test_un_op_num():
    assert fold_constants(
        UnaryOperation(
            '-',
            Number(2)
        )
    ) == Number(-2)


def test_multiply_by_zero_right():
    assert fold_constants(
        BinaryOperation(
            Reference('x'),
            '*',
            Number(0)
        )
    ) == Number(0)


def test_multiply_by_zero_left():
    assert fold_constants(
        BinaryOperation(
            Number(0),
            '*',
            Reference('x')
        )
    ) == Number(0)


def test_subtract_same_refs():
    assert fold_constants(
        BinaryOperation(
            Reference('x'),
            '-',
            Reference('x')
        )
    ) == Number(0)


def test_nonfoldable_add_same_refs():
    assert fold_constants(
        BinaryOperation(
            Reference('x'),
            '+',
            Reference('x')
        )
    ) == BinaryOperation(
        Reference('x'),
        '+',
        Reference('x')
    )


def test_nonfoldable_subtract_diff_refs():
    assert fold_constants(
        BinaryOperation(
            Reference('x'),
            '-',
            Reference('y')
        )
    ) == BinaryOperation(
        Reference('x'),
        '-',
        Reference('y')
    )


def test_end_to_end():
    assert fold_constants(
        BinaryOperation(
            Number(10),
            '-',
            UnaryOperation(
                '-',
                BinaryOperation(
                    Number(3),
                    '+',
                    BinaryOperation(
                        Reference('x'),
                        '-',
                        Reference('x')
                    )
                )
            )
        )
    ) == Number(13)


if __name__ == '__main__':
    pytest.main()
