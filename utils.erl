-module(utils).

-import(lists, [member/2]).

-export([empty_list/1, or_list/1]).

% empty_list returns true if the list is empty, false otherwise
empty_list([]) -> true;
empty_list(_) -> false.

% or_list returns the disjunction of a list of bools
or_list(L) -> member(true, L).
