//! Это основной файл с домашним заданием по языку Rust (task08).
//! Кроме него есть еще модуль `field.rs`, лежащий в этой же папке.
//!
//! Чтобы вам было легче ориентироваться, мы постарались прокомментировать весь код, насколько это возможно.
//! Если у вас остаются вопросы или непонятные места — пишите нам, поможем разобраться.

// Намек компилятору, что мы также хотим использовать наш модуль из файла `field.rs`.
mod field;

extern crate threadpool;

// Чтобы не писать `field::Cell:Empty`, можно "заимпортировать" нужные вещи из модуля.
use field::Cell::*;
use field::{parse_field, Field, N};
use std::sync::mpsc::channel;
use threadpool::ThreadPool;

/// Эта функция выполняет один шаг перебора в поисках решения головоломки.
/// Она перебирает значение какой-нибудь пустой клетки на поле всеми непротиворечивыми способами.
/// Что делать после фиксации значения, задаётся параметрами функции.
/// Эта функция написана в обобщённом стиле: результатом перебора может быть любое значение произвольного типа `T`.
///
/// В качестве результата она возвращает `Option<T>` — это тип, который говорит нам о том, что
/// вычисление могло завершиться неудачей и результат не известен. По такому поводу у него два
/// конструктора с разной семантикой:
/// 1. `Some(x)` — вычисление завершилось успешно (решение найдено), где `x` — его результат.
/// 2. `None` — вычисление завершилось неудачно (решения нет), результата нет, увы.
///
/// Она принимает на вход один произвольный типовой параметр `T` и три обычных параметра:
/// 1. `f: &mut Field` — мутабельная ссылку на поле (поле приходится менять и передавать туда-сюда).
///    Гарантируется, что при возврате `None` состояние поля не изменилось,
///    а при возврате `Some` состояние поля соответствует тому варианту, при котором перебор остановился.
/// 2. `solved_cb: impl Fn(&mut Field) -> T` — замыкание, которые вызывается, если мы нашли решение.
///    Это замыкание должно вернуть какое-то значение `x: T`, которое станет результатом работы `try_extend_field`.
/// 3. `next_step_cb: impl Fn(&mut Field) -> Option<T>` — замыкание, которое вызывается, когда мы не
///    до конца заполнили поле и нам требуется продолжить перебор. Это замыкание должно
///    вернуть `None`, если требуется попробовать ещё одно значение клетки, либо `Some(x)`,
///    если перебор следует остановить и вернуть `Some(x)`.
///
/// Замыкание — это просто анонимная функция, котрая может захватить какие-нибудь переменные из
/// объемлющей области видимости. Они бывают трёх видов (Rust автоматически его назначает, вам не надо беспокоиться):
/// 1. `Fn`. Самая строгое, захватывает переменные по иммутабельной ссылке. Может вызываться как угодно.
/// 2. `FnMut`. Захватывает переменные по мутабельным ссылкам. Может вызываться только по мутабельной ссылке,
///     т.к. надо гарантировать, что к захваченным переменным доступ возможен только одним способом.
/// 3. `FnOnce`. Захватывает владение переменными. Может его кому-нибудь передавать при вызове. Как следствие,
///     может быть вызвано не большо одного раза, потому что иначе получится передача владения дважды.
/// Разумеется, если какое-то замыкание является `Fn`, то его можно использовать и как будто оно `FnMut` или `FnOnce`.
///
/// Таким образом, тут для каждого параметра надо выбрать самый нестрогий тип замыкания из возможных:
/// * Так как `solved_cb` вызывается не более одного раза, можно выбрать самый общий тип — `FnOnce`.
/// * Так как `next_step_cb` может вызваться несколько раз, `FnOnce` не выбрать. А вот выбрать `FnMut` можно.
fn try_extend_field<T>(
    f: &mut Field,
    solved_cb: impl FnOnce(&mut Field) -> T,
    mut next_step_cb: impl FnMut(&mut Field) -> Option<T>,
) -> Option<T> {
    // Проверяем простые случаи:
    // 1. Поле противоречиво — решения нет.
    if f.contradictory() {
        return None;
    }
    // 2. Поле непротиворечиво, все заполнено => мы нашли решение.
    //    Надо вызвать наш колбэк-второй параметр и сообщить об успешной находке.
    if f.full() {
        return Some(solved_cb(f));
    }
    // Ищем первую непустую клетку.
    for row in 0..N {
        for col in 0..N {
            // Нашли пустую -- начинем перебирать все, что можно туда поставить
            if f[row][col] == Empty {
                for d in 1..=N {
                    f[row][col] = Digit(d);
                    // поставили -- вызвали колбэк(третий параметр)
                    // Здесь мы смотрим, если он вернул Some(x) -- значит мы нашли решение
                    // Надо его вернуть и дело с концом
                    // Иначе -- перебираем дальше
                    if let Some(x) = next_step_cb(f) {
                        return Some(x);
                    }
                    // И возвращаем все как было
                    f[row][col] = Empty;
                }
                return None;
            }
        }
    }
    // Эта строчка никогда не должна вызываться: поле непустое, а при нахождении
    // пустой клетке мы завершаем функцию. Если строчка-таки вызвалась, то это
    // ошибка программиста и программа "паникует" (валится с критической ошибкой).
    panic!("Field should've been non-full");
}

/// Юнит-тест на один шаг `try_extend_field`.
#[test]
fn test_try_extend_field_all_steps() {
    let mut f: Field = Field::empty();
    f[0][0] = Digit(1);

    let f_initial = f.clone();
    let mut called_with = Vec::new();

    assert!(try_extend_field(
        &mut f,
        |_| panic!("Field is not solved"),
        |f_next| {
            called_with.push(f_next.clone());
            None // Продолжить перебор.
        }
    )
    .is_none());
    assert_eq!(f, f_initial); // Поле не изменилось.
    assert_eq!(called_with.len(), 9); // Было перебрано 9 значений.

    let mut f = Field::empty();
    f[0][0] = Digit(1);

    f[0][1] = Digit(1);
    assert_eq!(f, called_with[0]);
    f[0][1] = Digit(2);
    assert_eq!(f, called_with[1]);
    f[0][1] = Digit(9);
    assert_eq!(f, called_with[8]);
}

/// Юнит-тест, проверяющий, что `try_extend_field` останавливается при получении `Some(x)`.
#[test]
fn test_try_extend_field_first_steps() {
    let mut f: Field = Field::empty();
    f[0][0] = Digit(1);

    let mut called_with = Vec::new();

    assert_eq!(
        try_extend_field(
            &mut f,
            |_| panic!("Field is not solved"),
            |f_next| {
                called_with.push(f_next.clone());
                if called_with.len() < 3 {
                    None // Продолжить перебор
                } else {
                    Some(12345) // Остановить перебор
                }
            }
        ),
        Some(12345)
    );
    assert_eq!(called_with.len(), 3);
    assert_eq!(f, called_with[2]); // Поле оставлено в состоянии, когда перебор остановился.

    let mut f = Field::empty();
    f[0][0] = Digit(1);

    f[0][1] = Digit(1);
    assert_eq!(f, called_with[0]);
    f[0][1] = Digit(2);
    assert_eq!(f, called_with[1]);
    f[0][1] = Digit(3);
    assert_eq!(f, called_with[2]);
}

/// Перебирает все возможные решения головоломки, заданной параметром `f`.
/// Если хотя бы одно решение `s` существует, `f` оказывается равным `s`,
/// а функция возвращает `Some(f)`.
/// Если решений нет, `f` остаётся неизменным, а функция возвращает `None`.
fn find_solution(f: &mut Field) -> Option<Field> {
    try_extend_field(f, |f_solved| f_solved.clone(), find_solution)
}

/// Перебирает все возможные решения головоломки, заданной параметром `f`, в несколько потоков.
/// Если хотя бы одно решение `s` существует, возвращает `Some(s)`,
/// в противном случае возвращает `None`.
fn find_solution_parallel(mut f: Field) -> Option<Field> {
    let n_threads = 8;
    let (sender, receiver) = channel();
    let pool = ThreadPool::new(n_threads);
    try_extend_field(
        &mut f,
        |f| {
            sender.send(Some(f.clone())).unwrap_or(());
            f.clone()
        },
        |f| {
            let sender = sender.clone();
            let mut f = f.clone();
            pool.execute(move || {
                sender.send(find_solution(&mut f)).unwrap_or(());
            });
            None
        }
    );
    std::mem::drop(sender);
    receiver.into_iter().find_map(|x| x)
}

/// Юнит-тест, проверяющий, что `find_solution()` находит лексикографически минимальное решение на пустом поле.
#[test]
fn test_find_solution_empty() {
    let expected = parse_field(
        // r#"" — это многострочный строковой литерал.
        r#"
123456789
456789123
789123456
214365897
365897214
897214365
531642978
642978531
978531642
"#
        .split("\n")
        .skip(1) // Так как первая строчка в литерале пустая, надо её пропустить.
        .map(|x| x.to_string()),
    );
    assert!(expected.full());
    assert!(!expected.contradictory());

    let found = find_solution(&mut Field::empty()).unwrap();
    assert!(found.full());
    assert!(!found.contradictory());
    assert_eq!(found, expected);
}

/// Юнит-тест, проверяющий, что `find_solution()` не находит ответ, когда его не существует.
#[test]
fn test_find_solution_no_solution() {
    let mut f = Field::empty();
    f[0][0] = Digit(1);
    f[0][1] = Digit(1);
    assert!(find_solution(&mut f).is_none());
}

/// Юнит-тест, проверяющий, что `find_solution()` находит ответ, когда остаётся всего один шаг.
#[test]
fn test_find_solution_one_step_solution() {
    let mut f = Field::empty();
    assert!(find_solution(&mut f).is_some());
    assert!(f.full());

    f[3][3] = Empty;
    assert!(find_solution(&mut f).is_some());
}

/// Юнит-тест, проверяющий, что `find_solution_parallel()` находит хоть какое-то решение на пустом поле.
/// Мы не можем гарантировать, какое именно это будет решение, так как потоки выполняются недетерминировано.
#[test]
fn test_find_solution_parallel_empty() {
    let found = find_solution_parallel(Field::empty()).unwrap();
    assert!(found.full());
    assert!(!found.contradictory());
}

/// Юнит-тест, проверяющий, что `find_solution_parallel()` не находит ответ, когда его не существует.
#[test]
fn test_find_solution_parallel_no_solution() {
    let mut f = Field::empty();
    f[0][0] = Digit(1);
    f[0][1] = Digit(1);
    assert!(find_solution_parallel(f).is_none());
}

/// Юнит-тест, проверяющий, что `find_solution_parallel()` находит ответ, когда остаётся всего один шаг.
#[test]
fn test_find_solution_parallel_one_step_solution() {
    let mut f = Field::empty();
    assert!(find_solution(&mut f).is_some());
    assert!(f.full());

    f[3][3] = Empty;
    assert!(find_solution_parallel(f).is_some());
}

/// Точка входа в нашу программу.
fn main() {
    // По подсказке компилятора: для корректного чтения строк через итератор
    // требуется, чтобы `BufRead` был "виден" в текущей функции.
    use std::io::BufRead;

    let stdin = std::io::stdin();
    // Читаем поле из stdin, заводим под него мутабельную переменную.
    // Так как `stdin.lock().lines()` возвращает итератор по `Result<String>`
    // (попыткам прочитать строчку), а не `String`, нам требуется "достать"
    // из каждой попытки реальную строчку. Это делает метод `.unwrap()`,
    // который при неудачной попытке чтения вызывает "панику" (критическую ошибку).
    let field = parse_field(stdin.lock().lines().map(|l| l.unwrap()));
    // stdin перестал быть нужен, избавимся от соответствующей переменной,
    // чтобы случайно не заиспользовать её потом.
    std::mem::drop(stdin);

    // Отладочный вывод поля, чтобы проверить корректность чтения.
    println!("\nGot field:\n{:?}", field);

    let now = std::time::Instant::now();

    // Запускаем поиск решения.
    // Если оно есть, то печатаем его, в противном случае печатаем `No solution`.
    println!("\nLooking for a solution...");
    match find_solution_parallel(field) {
        Some(solution) => println!("{:?}", solution),
        None => println!("No solution"),
    };
    println!("Found in {} ms", now.elapsed().as_millis());
}
