https://tutorcs.com
WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
-module(hw1_tests).
-include_lib("eunit/include/eunit.hrl").

-import(hw1, [sum/1, sum_hr/1, sum_tr/1, sum_iii/1]).
-import(hw1, [sq/1, sq_i/0, sq_ii/1, sq_iii/1]).
-import(hw1, [gcd/2, p/1, pi/1, p/2, pi/2, gen_test/1]).

sum_args() -> [1, 3, 11, 3978, 0, -4, 1.5, cow, [this, is, a, list], {}, self()].

sum_test_() ->
  Is_hr =
    try
      sum_hr(1) == sum_is_already_head_recursive
    catch _:_ -> false
    end,
  Is_tr =
    try
      sum_tr(1) == sum_is_already_tail_recursive
    catch _:_ -> false
    end,
  %io:format("Is_hr = ~w, Is_tr = ~w~n", [Is_hr, Is_tr]),
  HR_tests = case Is_hr of
    false -> % sum is not head recursive, compare sum_hr with sum
      [    try
	     Ref = sum(N),
	     ?_assertEqual(Ref, sum_hr(N))
	   catch _:_ -> ?_assertError(_, sum_hr(N))
	   end
	|| N <- sum_args() ];
    true -> []
  end,
  TR_tests = case Is_tr of
    false -> % sum is not head recursive, compare sum_hr with sum
      [    try
	     Ref = sum(N),
	     ?_assertEqual(Ref, sum_tr(N))
	   catch _:_ -> ?_assertError(_, sum_tr(N))
	   end
	|| N <- sum_args() ];
    true -> []
  end,
  III_tests =
    [    try
	   Ref = sum(N),
	   ?_assertEqual(Ref, sum_iii(N))
	 catch _:_ -> ?_assertError(_, sum_iii(N))
	 end
      || N <- sum_args() ],
  [HR_tests, TR_tests, ?_assert(not (Is_hr and Is_tr)),
   III_tests].


sq_test() ->
  ?assert(lists:member(sq_i(), [head_recursive, tail_recursive])),
  TestList0 = [],
  TestList1 = [12],
  TestList2 = [1, 3, -2, 1.2, 22/7, 0, 123456789],
  ?assertEqual(sq(TestList0), sq_ii(TestList0)),
  ?assertEqual(sq(TestList1), sq_ii(TestList1)),
  ?assertEqual(sq(TestList2), sq_ii(TestList2)),
  ?assertError(_, sq_ii(bad)),
  ?assertError(_, sq_ii({1, 2, 3})),
  ?assertEqual(sq(TestList0), sq_iii(TestList0)),
  ?assertEqual(sq(TestList1), sq_iii(TestList1)),
  ?assertEqual(sq(TestList2), sq_iii(TestList2)),
  ?assertError(_, sq_iii(bad)),
  ?assertError(_, sq_iii({1, 2, 3})).


gcd_test() ->
  ?assertEqual(3, gcd(12, 21)),
  ?assertEqual(3, gcd(-12, -21)),
  ?assertEqual(12, gcd(-12, 0)),
  ?assertEqual(1, gcd(1, 2239284229266)),
  ?assertEqual(42, gcd(2239284229266, 3623237993424)),
  ?assertError(_, gcd(rabbit, 2023)),
  ?assertError(_, gcd(0, 0)),
  ?assertError(_, gcd(1.2, 3)),
  ?assertError(_, gcd(12, [21])).

p_test() ->
  ?assertEqual(3, p(2)),
  ?assertEqual(23, p(6)),
  ?assertEqual(6087, p(100)),
  % as formulated in the problem statement, p(N) = 0 when N =< 0, but I'll
  % also accept it if your implementation throws an error for values of N.
  try
    X = p(-4),
    ?assertEqual(0, X)
  catch _:_ -> ok
  end,
  ?assertError(_, p(bad)).


close(X, Y, Epsilon) when is_float(X), is_number(Y) ->
  abs(X-Y) =< Epsilon.
close(X, Y) when is_float(X), is_number(Y) ->
  close(X, Y, 1.0e-12 * lists:max([abs(X), abs(Y), 1])).

pi_test() ->
  ?assert(close(3.0860669992418384, pi(10))),
  ?assert(close(3.1395974980055170, pi(100))),
  ?assertError(_, pi(3.14159265359)).

p2_test() ->
  X = p(100, 100000),
  % You could be *really* unlucky, but these bounds should be wide enough.
  ?assert((59370 < X) and (X < 62370)).

pi2_test() ->
  Pi = pi(1000000000, 100000),
  % Again, you could be *really* unlucky, but ...
  ?assert((3.10 < Pi) and (Pi < 3.18)).

gen_test() ->
  ?assertEqual(6, gen_test(3)),
  ?assertEqual(500500, gen_test(1000)),
  ?assertEqual(0, gen_test(0)),
  ?assertEqual(0, gen_test(-3)),
  ?assertError(_, gen_test(2.3)),
  ?assertError(_, gen_test([])).
