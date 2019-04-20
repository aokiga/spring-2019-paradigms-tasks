//! В модуле `field` расположена основная логика головоломки "Судоку"
//! Слово `pub` обозначает те константы, структуры и поля, которые будут
//! видны вне модуля `field`, т.е. в модуле `main`.

use std::fmt;

/// Ширина и высота одной ячейки поля, а также количество ячеек в поле по вертикали и горизонтали
pub const K: usize = 3;

/// Ширина и высота всего поля в клетках
pub const N: usize = K * K;

/// Описание состояния одной клетки в головоломке в процессе перебора.
///
/// Клетка может быть либо пустой (незаполненной), либо в ней может находиться
/// число от 1 до `K`.
///
/// Для состояния клетки естественным образом выведены реализации типажей (traits), аналогов интерфейсов:
/// * Copy: значение клетки можно копировать побитово и это действие по умолчанию для оператора `=` вместо перемещения
/// * Clone: доступен метод `.clone()` для явного клонирования
/// * PartialEq и Eq: отношение эквивалентности через `==` и `!=` (рефлексивное, симметричное, транзитивное)
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Cell {
    /// Клетка пуста, т.е. в ней может находиться что угодно и мы не знаем, что именно.
    Empty,
    /// В клетке находится цифра от 1 до K — единственный параметр Digit()
    Digit(usize),
}

// Чтобы постоянно не писать Cell::Empty, сделаем внутренности Cell видимыми в этом файле.
use Cell::*;

/// Реализация типажа `fmt::Debug` для `Cell`.
/// Он требуется для отладочной печати, например, при помощи `println!("{:?}", cell);`
impl fmt::Debug for Cell {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Empty => '.',
                Digit(x) => std::char::from_digit(*x as u32, (N + 1) as u32).unwrap(),
            }
        )
    }
}

/// Описание состояния головоломки в процессе перебора.
///
/// Головоломка представляет собой поле из клеток размера `N*N`,
/// каждая клетка может быть либо известна, либо неизвестна (см. `Cell`).
///
/// Поле обёрнуто в структуру `Field` с единственным полем `0`, которое
/// является двумерным массивом из `Cell`.
/// Первое измерение массива соответствует номеру строки, а второе — номеру столбца.
///
/// Для более удобной работы для структуры `Field` также реализованы
/// оператор `[]` и другие вспомогательные методы.
///
/// Для поля автоматически выведена реализация типажа `Clone`, чтобы
/// можно было вызывать `.clone()`.
/// Аналогичным образом выведена реализация `PartialEq` и `Eq` для оператора `==`.
/// Типаж `Copy` тоже можно было бы вывести автоматически, но поле — слишком
/// большой объект, чтобы разрешать неявное копирование при помощи `=`.
#[derive(Clone, PartialEq, Eq)]
pub struct Field(pub [[Cell; N]; N]);

/// Реализация типажа `Index<usize>` для структуры `Field`,
/// позволяющая получать доступ к строчкам поля на чтение при помощи оператора `[]`.
/// Например: `field[row][col]` вместо `field.0[row][col]`.
impl std::ops::Index<usize> for Field {
    type Output = [Cell; N];
    fn index(&self, row: usize) -> &[Cell; N] {
        &self.0[row]
    }
}

/// Реализация типажа `IndexMut<usize>` для структуры `Field`,
/// позволяющая получать доступ к строчкам поля на запись при помощи оператора `[]`.
/// Например: `field[row][col] = Empty` вместо `field.0[row][col] = Empty`.
impl std::ops::IndexMut<usize> for Field {
    fn index_mut(&mut self, row: usize) -> &mut [Cell; N] {
        &mut self.0[row]
    }
}

impl Field {
    /// Создаёт новое пустое поле, т.е. в котором может находиться что угодно.
    pub fn empty() -> Field {
        Field([[Empty; N]; N])
    }

    /// Возвращает `true`, если все клетки поля заполнены, `false` иначе.
    /// Время работы: `O(n^2)`.
    pub fn full(&self) -> bool {
        self.0.iter().all(|cells| cells.iter().all(|c| *c != Empty))
    }

    /// Возвращает `true`, если среди заполненных клеток поля существует противоречие,
    /// т.е. в какой-нибудь строке/столбце/ячейке поля имеются дублирующиеся значения.
    /// Возвращает `false` при отсутствии противоречий.
    ///
    /// В частности, если поле заполнено целиком, то значение `false` возвращается для всех
    /// корректных решений головоломки и только для них.
    pub fn contradictory(&self) -> bool {
        // Проверка всех строк.
        for row in 0..N {
            let mut was = [false; N + 1];
            for col in 0..N {
                if let Digit(val) = self[row][col] {
                    if was[val] {
                        return true;
                    }
                    was[val] = true;
                }
            }
        }
        // Проверка всех столбцов.
        for col in 0..N {
            let mut was = [false; N + 1];
            for row in 0..N {
                if let Digit(val) = self[row][col] {
                    if was[val] {
                        return true;
                    }
                    was[val] = true;
                }
            }
        }
        // Проверка всех ячеек.
        for row_0 in 0..K {
            for col_0 in 0..K {
                let mut was = [false; N + 1];
                for row_d in 0..K {
                    for col_d in 0..K {
                        if let Digit(val) = self[row_0 * K + row_d][col_0 * K + col_d] {
                            if was[val] {
                                return true;
                            }
                            was[val] = true;
                        }
                    }
                }
            }
        }
        false
    }
}

/// Юнит-тест для функции `Field::contradictory()`.
#[test]
fn test_contradictory() {
    assert_eq!(
        false,
        Field([
            [Digit(1), Digit(2), Digit(3), Empty, Empty, Empty, Empty, Empty, Empty],
            [Digit(4), Digit(5), Digit(6), Empty, Empty, Empty, Empty, Empty, Empty],
            [Digit(7), Digit(8), Digit(9), Empty, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        ])
        .contradictory()
    );
    assert_eq!(
        true,
        Field([
            [Digit(1), Digit(2), Digit(3), Digit(1), Empty, Empty, Empty, Empty, Empty],
            [Digit(4), Digit(5), Digit(6), Empty, Empty, Empty, Empty, Empty, Empty],
            [Digit(7), Digit(8), Digit(9), Empty, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        ])
        .contradictory()
    );
    assert_eq!(
        true,
        Field([
            [Digit(1), Digit(2), Digit(3), Empty, Empty, Empty, Empty, Empty, Empty],
            [Digit(4), Digit(5), Digit(6), Empty, Empty, Empty, Empty, Empty, Empty],
            [Digit(7), Digit(8), Digit(9), Empty, Empty, Empty, Empty, Empty, Empty],
            [Digit(1), Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        ])
        .contradictory()
    );
    assert_eq!(
        true,
        Field([
            [Digit(1), Digit(2), Digit(3), Empty, Empty, Empty, Empty, Empty, Empty],
            [Digit(4), Digit(5), Digit(6), Empty, Empty, Empty, Empty, Empty, Empty],
            [Digit(7), Digit(8), Digit(1), Empty, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        ])
        .contradictory()
    );
}

/// Реализация типажа `fmt::Debug` для `Field`.
/// Он требуется для отладочной печати, например, при помощи `println!("{:?}", cell);`
///
/// Эта реализация выводит поле в `N` строк, по `N` символов в каждой.
///
/// Обратное преобразование может быть выполнено функцией `parse_field`.
impl fmt::Debug for Field {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (row_id, cells) in self.0.iter().enumerate() {
            for cell in cells.iter() {
                write!(f, "{:?}", cell)?;
            }
            if row_id + 1 < N {
                write!(f, "\n")?;
            }
        }
        Ok(())
    }
}

/// Эта функция читает первые `N` строк из итератора `reader` и интерпретирует
/// их как поле головоломки.
/// Возвращает новую структуру `Field` с заполненными клетками.
///
/// В `reader` ожидается `N` строк, в каждой — ровно `N` символов.
/// Каждый символ должен представлять собой либо `.` (пустая клетка),
/// либо цифру от 1 до 9 (заполненная клетка).
///
/// При нарушении формата происходит "паника" (критическая ошибка).
///
/// Для упрощения кода требуется, что итератор `reader` передавал `parse_field`
/// владение всеми прочитанными `String`. Можно было бы написать то же самое,
/// но с итератором, возвращающим кусочки строк в `&str`, но тогда в `main()`
/// было бы неудобно пользоваться `.lines()`. Можно было бы написать более общую
/// версию `parse_field`, но это потребовало бы [дополнительных указаний](https://stackoverflow.com/a/35626785/767632).
pub fn parse_field<I>(reader: I) -> Field
where
    I: IntoIterator<Item = String>,
{
    let mut result = Field::empty();
    let mut reader = reader.into_iter();
    for row in 0..N {
        let line = reader.next().expect("Not enough lines provided");
        assert_eq!(line.len(), N);

        for (col, ch) in line.chars().enumerate() {
            result[row][col] = match ch.to_digit((N + 1) as u32) {
                Some(x) => Digit(x as usize),
                _ if ch == '.' => Empty,
                _ => panic!(format!("Unknown character: {}", ch)),
            }
        }
    }
    result
}

/// Подмодуль, отдельно содержащий юнит-тесты для ввода-вывода.
#[cfg(test)]
mod io_test {
    // Пространства имён модулей независимы, поэтому надо заново писать `use`.
    use super::Cell::*;
    use super::{parse_field, Field};

    const FIELD: Field = Field([
        [Digit(1), Digit(2), Digit(3), Empty, Empty, Empty, Empty, Empty, Empty],
        [Digit(4), Digit(5), Digit(6), Empty, Empty, Empty, Empty, Empty, Empty],
        [Digit(7), Digit(8), Digit(9), Empty, Empty, Empty, Empty, Empty, Empty],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
    ]);
    const FORMATTED: &str = "\
123......
456......
789......
.........
.........
.........
.........
.........
.........";

    #[test]
    fn test_debug_format() {
        assert_eq!(FORMATTED, format!("{:?}", FIELD));
    }

    #[test]
    fn test_parse_field() {
        // Несмотря на то, что `.split()` возвращает итератор по кусочками строки,
        // `parse_field()` требует владеющий `String`, поэтому требуется вызвать `.to_string()`.
        assert_eq!(FIELD, parse_field(FORMATTED.split('\n').map(|x| x.to_string())));
    }
}
