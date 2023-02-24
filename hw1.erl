https://tutorcs.com
WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
-module(hw1).  % template code for CPSC 521 hw1.

-export([sum/1, sum_i/0, sum_hr/1, sum_tr/1, sum_iii/1,  % functions for Question 1
	 sq/1, sq_i/0, sq_ii/1, sq_iii/1]).
-export([gcd/2]).                                        % function for Question 2
-export([p/1, p/2, pi/1, pi/2]).    % functions for Question 3.
-export([gen_fold/3, gen_test/1]).  % functions for Question 4.

%
% -export([...]).  % When writing your implementation, you will almost
%                  % certainly define some helper functions.  Feel free to
%                  % export them.  I find that debugging is *way* easier
%                  % when I can test my functions one at a time.

% Anywhere in this template code that you see a term of the form:
%     missing_implementation([function_name, Arg1, Arg2, ...])
% that means that for your solution, you need to replace that call to
% missing_implementation/1 with your own code.
% We export missing_implementation/1 so your solution can compile without
% warnings even after you've replaced all calls to missing_implemenation(...)
% with your own code.
-export([missing_implementation/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                      %
% functions for Q1                                                     %
%                                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Q1.a:  sum(N)
sum(0) -> 0;
sum(N) when is_integer(N), 0 < N ->
  N + sum(N-1).

% Q1.a.i  is sum/1 head-recursive or tail_recursive?
% If sum/1 defined above is head-recursive,
%   then
%     replace missing_implementation([sum_i]) in the template below
%     with the atom head_recursive;
%   otherwise  (this means sum/1 is tail-recursive)
%     replace missing_implementation([sum_i]) with the atom
%     with the atom tail_recursive;
sum_i() -> missing_implementation([sum_i]).

% Q1.a.ii  write the other version of sum.
% If sum/1 defined above is tail-recursive,
%   then
%     replace missing_implementation([sum_hr, N]) in the template below
%     with a head-recursive version;
%   otherwise % sum/1 is head-recursive
%     replace missing_implementation([sum_hr, N]) with the atom
%     sum_is_already_head_recursive.
sum_hr(N) ->
  missing_implementation([sum_hr, N]).

% if sum/1 defined above is head-recursive,
%   then
%     replace missing_implementation([sum_tr, N]) in the template below
%     with a tail-recursive version;
%   otherwise % sum/1 is tail-recursive
%     replace missing_implementation([sum_tr, N]) with the atom
%     sum_is_already_tail_recursive.
sum_tr(N) ->
  missing_implementation([sum_tr, N]).

% implement sum using lists:foldl, lists:foldr, or lists:map
% Replace missing_implementation([sum_iii, N]) in the template below
%   with your implementation that uses foldl, foldr, or map.
sum_iii(N) ->
  missing_implementation([sum_iii, N]).


% Q1.b:  sq(N)

% sq(List) -> square each element of list, and return the resulting list
sq([]) -> [];
sq([Hd | Tl]) -> [Hd*Hd | sq(Tl)].

% Q1.b.i:  sq_i() -> head_recursive, if sq is head-recursive
%                 -> tail_recursive, if sq is tail-recursive
sq_i() -> missing_implementation([sq_i]).

% Q1.b.ii: implement sq_ii(List) using lists:foldl, lists:foldr, or lists:map.
%   sq_ii(List) should return the same result as sq(List) to within floating point
%     roundoff error (if List has elements that are floating point numbers).  The
%     two functions return identical results if every element of List is an Erlang
%     integer.
sq_ii(List) ->
  missing_implementation([sq_ii, List]).

% Q1.b.iii: implement sq_iii(N) using a list comprehension.
%   Same remarks as above about matching sq(List) to within round-off errors,
%   and identical for lists of integers.
sq_iii(List) ->
  missing_implementation([sq_iii, List]).

% Q1.c: Mind reading
% Please provide your answer in the hw1.pdf file that you submit with your solution.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                      %
% function for Q2                                                      %
%                                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% gcd(A, B)
gcd(A, B) -> missing_implementation([gcd, A, B]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                      %
% functions for Q3                                                     %
%                                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Q3.a:  p(N) -> The number of pairs of integers {A, B}, with 1 =< A, B =< N
%   such that A and B are co-prime.
p(N) -> missing_implementation([p, N]).

% Q3.b:  pi(N) -> an estimate of pi using p(N).
%   Hint: lim_{N->infinity} p(N)/(N^2) = 6/(pi^2).
pi(N) -> missing_implementation([pi, N]).

% Q3.c:  p(N, M) ->
%   generate M pseudo-random pairs of integers {A, B} with 1 =< A, B =< N
%   and return the number of these pairs where A and B are co-prime.
%   Hint: use rand:uniform to obtain uniformly distributed pseudo-random integers.
p(N, M) -> missing_implementation([p, N, M]).

% Q3.d:  pi(N, M) -> an estimate of pi using p(N, M).
pi(N, M) -> missing_implementation([pi, N, M]).

% Q3.e:  compare pi(N) and pi(N, M).
% Please provide your answer in the hw1.pdf file that you submit with your solution.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                      %
% functions for Q4                                                     %
%                                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Q4.a: implement gen_produce/2, gen_fold/3, and gen_fold/4

% Q4.a.i: gen_produce(GenFun, GenArg) ->
%   Wait to receive a request, Req, from our parent process.
%   For each request of the form {next, Pid}, call GenFun(GenArg) to
%   get a {V, NewArg} pair.  Send {gen, self(), {ok, V}} to Pid, and use
%   NewArg for the next call to GenFun.  If GenFun returns the atom done,
%   send {gen, self(), done} to Pid, and exit.
%   We can also receive a message that is just the atom exit.  In that
%   case, exit right away.
gen_produce(GenFun, GenArg) ->
  receive
    {next, Pid} ->
      missing_implementation([gen_produce, GenFun, GenArg], Pid);
    exit -> ok   % allows the consumer to signal an early termination
  end.

% Q4.a.ii: gen_fold(AccFun, Acc, GenProc) ->
%   send {next, self()} requests to GenProc and receive the responses.
%   Each time a new value is received, combine it with Acc using AccFun.
%   When that atom 'done' is received, return Acc.
gen_fold(AccFun, Acc, GenProc) ->
  GenProc ! {next, self()}, % request the next value
  receive
    {gen, GenProc, done} ->
      missing_implementation([gen_fold, AccFun, Acc, GenProc]);
    {gen, GenProc, {ok, V}} ->
      missing_implementation([gen_fold, AccFun, Acc, GenProc], V)
  end,
  % delete the next line when you've implemented your solution.
  % It's a bogus call to gen_produce to avoid a "function unused" warning.
  gen_produce(42, cows).

% Q4.a.iii: gen_fold(AccFun, Acc0, GenProc) ->
%   Spawn a generator function, and then call gen_fold/3 to combine the values
%   from the generator to get the total result.
gen_fold(AccFun, Acc0, GenFun, GenArg) ->
  GenProc = spawn(missing_implementation([gen_fold, AccFun, Acc0, GenFun, GenArg])),
  gen_fold(AccFun, Acc0, GenProc).


% gen_test(N) -> sum_{I=0}^N I, computed using gen_fold.
gen_test(N) when is_integer(N) ->
  gen_fold(fun(X, Acc) -> X + Acc end, 0,
	     fun(I) when I =< N -> {I, I+1};
	        (_) -> done
	     end, 1).

% Q4.b: Please provide your written answer in hw1.pdf


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                      %
% Utility functions                                                    %
%                                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The next three functions make the code in this module compile without
% errors or warnings, even though there are pieces you need to implement.
% If you try running one of the functions above without implementing your
% solution, then these functions will print an error message.

missing_implementation(FunArgs) ->
  io:format("missing implementation for "),
  print_fncall(FunArgs).

missing_implementation(FunArgs, _IgnoreTheseLocals) ->
  io:format("missing implementation for "),
  print_fncall(FunArgs).


print_fncall([FunAtom | Args]) ->
  io:format("~w(", [FunAtom]),
  case Args of
    [Arg1 | ArgTl] ->
      io:format("~p", [Arg1]),
      [ io:format(", ~p", [Arg]) || Arg <- ArgTl ];
    [] -> ok
  end,
  io:format(")~n"),
  error([missing_implementation, FunAtom, Args]).
