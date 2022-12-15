my_setof(Var, Expr, _) :- call(Expr), assertz(found_result(Var)), fail.
my_setof(_, _, List) :- collect_uniq_results(List).


insert(X, T, R) :-
    append(A, B, T),
    not((member(Y, A), Y >= X)), not((member(Y, B), Y =< X)),
    append(A, [X | B], R), !.
insert(_, T, T).


collect_uniq_results(T) :- retract(found_result(X)), collect_uniq_results(T1), insert(X, T1, T), !.
collect_uniq_results([]).