# Вариант P2V1

## Задача 1

Опишите предикат, преобразующий арифметическое выражение в список операций и операндов в постфиксной записи.
Пример: `expr2postfix(1+2*3,[1,2,3,*,+]).`

### Решение

```Prolog
expr2postfix(N, [N]) :- integer(N), !.
expr2postfix(E, R) :- E=..[Op, A, B], expr2postfix(A, X), expr2postfix(B, Y), append(X, Y, L), append(L, [Op], R).
```

## Задача 2

Пусть у нас есть двоичное дерево, в листьях которого находятся числа, а узлы не содержат полезной информации.

1. Предложите способ описания такого дерева на Прологе, приведите пример.
2. Опишите предикат генерации по дереву всех возможных арифметических выражений с такой же структурой (т.е. подставляя в узлы дерева все возможные арифметические операции)
3. Из всех сгенерированных выражений выражение с максимальным значением.

### Решение

```Prolog
%% 1. Представляем бинарное дерево, используя оператор '#'
:-op(700, xfy, #).

%% 2. Генерируем выражения через обход по дереву. Каждый узел (это оператор '#') меняем на математический оператор
gen(N,_,N) :- integer(N), !.
gen(E,O,R) :- E=..[_, A, B], gen(A,O,X), gen(B,O,Y), member(NOp, O),R=..[NOp, X, Y].
gen(E, N) :- gen(E, [+,-,/,*], R), N is R.

%% 3. Находим максимальное значение, используя св-во упорядоченности результирующего списка предиката setof
max_value(E, X) :- setof(N, gen(E, N), L), append(_, [X], L).
```

# Вариант P2V2

## Задание 1

Опишите предикат, преобразующий арифметическое выражение в список операций и операндов в префиксной записи.
Пример: `expr2prefix(1+2*3,[+,1,*,2,3]).`

### Решение

```Prolog
expr2prefix(N, [N]) :- integer(N), !.
expr2prefix(E, [Op|L]) :- E=..[Op, A, B], expr2prefix(A, X), expr2prefix(B, Y), append(X, Y, L).
```

## Задание 2

Номер автобусного билета содержит 6 цифр. Назовём билет обобщенно счастливым, если расстановкой между его первыми тремя цифрами и последними тремя цифрами всевозможных арифметических операций и скобок можно получить два выражения с одинаковыми значениями.

1. Описать предикат, проверяющий, является ли заданный билет обобщенно счастливым
2. Для билета, являющегося обобщенно счастливым, вывести все значения соответствующих выражений. 

### Решение

```Prolog
prefix2expr([N], N) :- integer(N), !.
prefix2expr([Op|T], E) :- append(X, Y, T), prefix2expr(X, A), prefix2expr(Y, B), E=..[Op, A, B].

expr_permutation([X, Y, Z], O, E) :-
    member(A, O), member(B, O),
    prefix2expr([A, X, B, Y, Z], E).

lucky([X, Y, Z, X2, Y2, Z2], E1, E2) :-
    expr_permutation([X, Y, Z], [+, -, /, *], E1),
    expr_permutation([X2, Y2, Z2], [+, -, /, *], E2),
    catch(
        (R is E1, R is E2),
        error(evaluation_error(zero_divisor), _),
        fail
    ), !.

lucky(T) :- lucky(T, _, _).
```

# Вариант P2V3

## Задача 1

Опишите предикат, совершающий замену переменных в некотором арифметическом выражении. Пример: `subst(1+x*(x+1),x,10,R) => R=1+10*(10+1)`

### Решение

```Prolog
substr(N, _, _, N) :- integer(N), !.
substr(X, X, N, N) :- !.
substr(E, X, N, R) :- E=..[Op, A, B], substr(A, X, N, R1), substr(B, X, N, R2), R=..[Op, R1, R2].
```

## Задача 2

При помощи Пролог-программы расшифруйте числовой ребус: `ОДИН + ОДИН = МНОГО`. В ребусе каждой букве соответствует какая-то цифра, разным буквам соответствуют разные цифры.

### Решение

```Prolog
make_number([X], X, 0, L) :- member(X, L).

make_number([X|T], N, C, L) :-
    member(X, L), make_number(T, N1, C1, L),
    C is C1 + 1, N is N1 + X * 10 ^ C.

rebus([O, D, I, N, +, O, D, I, N, =, M, N, O, G, O]) :-
    make_number([O, D, I, N], N1, _, [9,8,7,6,5,4,3,2,1,0]),
    make_number([M, N, O, G, O], N3, _, [9,8,7,6,5,4,3,2,1,0]),
    N3 is 2 * N1, !.
```

### Решение (обобщенный случай)

```Prolog
expr_letter(A, C) :- atom(A), atom_chars(A, CL), member(C, CL).
expr_letter(E, C) :- E=..[_, L, R], (expr_letter(L, C); expr_letter(R, C)).

substr([], _, []) :- !.
substr([X | T], S, [Y | R]) :- member([X, Y], S), substr(T, S, R).
substr(W, S, R) :- atom(W), atom_chars(W, WL), substr(WL, S, RL), atom_chars(R, RL).

substr_expr(W, S, R) :-
	atom(W), substr(W, S, R2),
	atom_number(R2, R).

substr_expr(E, S, R) :-
	E=..[Op, A, B],
	substr_expr(A, S, A1), substr_expr(B, S, B1), R=..[Op, A1, B1].

substractions([], _, []) :- !.
substractions([S | T], L, [[S, X] | R]) :- select(X, L, L1), substractions(T, L1, R).

solve(E, R) :-
	setof(X, expr_letter(E, X), L), atom_chars('1234567890', N),
	substractions(L, N, A), substr_expr(E, A, R),
	R=..['=', B, C], BR is B, CR is C, BR = CR.

rebus :-
    solve('ОДИН' + 'ОДИН' = 'МНОГО', X),
    write(X), nl.
```

# Вариант P2V4

## Задание 1

Опишите оператор `bis`, который позволяет вычислять на Прологе логические выражения, где `+` соответствует дизъюнкции, а `*` - конъюнкции. Пример:

```Prolog
?- X bis t * (t + f).
X = t.
```

### Решение

```Prolog
:- op(700, xfx, bis).

bis(t, E) :-
    bexpr(E, A), N is A, N = 1.
bis(f, E) :-
    bexpr(E, A), N is A, N = 0. 

bexpr(t, 1).
bexpr(f, 0).
bexpr(E, R) :- E=..[Op, A, B], bexpr(A, X), bexpr(B, Y), R=..[Op, X, Y].
```

## Задание 2

В IQ-тестах любят задачки на поиски недостающей фигуры, как приведено на рисунке.

![O8o_9gd8Nr8](https://user-images.githubusercontent.com/95340036/208153226-cc537baa-21f6-4051-86ae-c439d1c3a471.jpg)

1. Предложите способ представления фигур и их матрицы на Прологе
2. Решите задачу поиска недостающей фигуры для поля $3 \times 3$
3. Решите задачу для произвольного размера поля

### Решение

> Решение с самого экзамена. Относиться с осторожностью, может быть неправильным.

```Prolog
index(Index, List, Element) :-
    append(A, [Element | _], List), length(A, Index).


filter_element(Index, Element, [L | T], [L | R]) :-
    index(Index, L, X), X \= Element, !,
    filter_element(Index, Element, T, R).

filter_element(Index, Element, [_ | T], R) :-
    filter_element(Index, Element, T, R).

filter_element(_, _, [], []).


filter(A, Lists, Result) :-
    length(A, N), N2 is N - 1, filter(N2, A, Lists, Result).

filter(N, A, L, Result) :-
    N > -1,
    index(N, A, E), filter_element(N, E, L, L1),
    N1 is N - 1, filter(N1, A, L1, Result).

filter(-1, _, L, L).

zip([[X, Y] | T], [X | A], [Y | B]) :- zip(T, A, B).
zip([], [], []) :- !.

filter_mask([R | M], L1, L2) :-
    zip(R, A, B),
    member(A, L1), member(B, L2),
    filter(A, L1, L11), filter(B, L2, L22),
    filter_mask(M, L11, L22).

filter_mask([], _, _) :- !.

mask(Mask, P1, P2) :-
    findall(X, permutation(X, P1), L1),
    findall(X, permutation(X, P2), L2),
    filter_mask(Mask, L1, L2).
```
