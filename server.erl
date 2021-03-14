-module(server).

-include_lib("stdlib/include/qlc.hrl").
-import(lists, [foreach/2]).

-export([setup/0, start/0, loop/0, reset/0,
         show_table/1,
         add_person/4, remove_person/1
         ]).

% person's primary key: nrCC
-record(person, {nrCC, name, address, phone}).
% book's primary key: id
-record(book, {id, name, authors}).
% request's primary key: {nrCC(person), id(book)}
-record(request, {id, nrCC}).


bookshelf() -> [
  {book, 1, "Programming Erlang: software for a concurrent world", ["Joe Armstrong"]},
  {book, 2, "The little book of Semaphores", ["Allen B. Downey"]}
  %{book, 3, "Numerical Otimization", ["Jorge Nocedal", "Stephen J. Wrigth"]},
  %{book, 4, "A first course in mathematical modeling", ["Frank R. Giordano", "William P. Fox", "Steven B. Horton"]},
  %{book, 5, "Convex Optimization", ["Stephen Boyd", "Lieven Vandenberghe"]}
].

setup() ->
    mnesia:create_schema([self()]),
    mnesia:start(),
    mnesia:create_table(person, [{attributes, record_info(fields, person)}]),
    mnesia:create_table(book, [{attributes, record_info(fields, book)}]),
    mnesia:create_table(request, [{attributes, record_info(fields, request)}]),
    mnesia:wait_for_tables([person, book, request], 20000),
    mnesia:transaction(fun() -> foreach(fun mnesia:write/1, bookshelf()) end).

start() -> spawn(fun() -> setup(), loop() end).

loop() ->
  receive
    {show_table, From, Table} -> From ! show_table(Table), loop();

    {add_person, _, NrCC, Name, Address, Phone} -> add_person(NrCC, Name, Address, Phone), loop();
    {remove_person, _, NrCC} -> remove_person(NrCC), loop();

    {add_request, _, ID, NrCC} -> add_request(ID, NrCC), loop();
    {remove_request, _, ID, NrCC} -> remove_request(ID, NrCC), loop()
  end.

reset() ->
  mnesia:clear_table(person),
  mnesia:clear_table(book),
  mnesia:clear_table(bookshelf),
  mnesia:transaction(fun() -> foreach(fun mnesia:write/1, bookshelf()) end).

show_table(Table) ->
  do(qlc:q([X || X <- mnesia:table(Table)])).

% Entry :: {tableName, attributes}
add_entry(Entry) ->
  mnesia:transaction(fun() -> mnesia:write(Entry) end).

% OId :: {tableName, PK}
remove_entry(OId) ->
  mnesia:transaction(fun() -> mnesia:delete(OId) end).

do(Q) ->
  F = fun() -> qlc:e(Q) end,
  case mnesia:transaction(F) of
  {atomic, Val} -> Val;
  {aborted, Reason} -> Reason
  end.

add_person(NrCC, Name, Address, Phone) ->
  add_entry(#person{nrCC=NrCC, name=Name, address=Address, phone=Phone}).

remove_person(NrCC) ->
  OId = {person, NrCC},
  remove_entry(OId).

%
%add_book({ID, Name, Authors}) ->
%  add_entry(#book{id=ID, name=Name, authors=Authors}).

%remove_book(ID) ->
%  OId = {book, ID},
%  remove_entry(OId).

add_request(ID, NrCC) ->
  add_entry(#request{id=ID, nrCC=NrCC}).

remove_request(ID, NrCC) ->
  OId = {request, {ID, NrCC}},
  remove_entry(OId).
