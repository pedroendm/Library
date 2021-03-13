-module(server).

-include_lib("stdlib/include/qlc.hrl").
-import(lists, [foreach/2]).

-export([start/0, reset/0, show_table/1,
         add_person/4, remove_person/1,
         add_book/3, remove_book/1]).

% person's primary key: nrCC
-record(person, {nrCC :: integer(), name :: string(), address :: string(), phone :: integer()}).
% book's primary key: id
-record(book, {id :: integer(), name :: string(), authors :: [string()]}).

%% DB related
start() ->
    mnesia:create_schema([self()]),
    mnesia:start(),
    mnesia:create_table(person, [{attributes, record_info(fields, person)}]),
    mnesia:create_table(book, [{attributes, record_info(fields, book)}]),
    %mnesia:create_table(request, [{attributes, record}])
    mnesia:wait_for_tables([person, book], 20000).

reset() ->
  F = fun() -> [mnesia:delete(person, Key) || Key <- mnesia:all_keys(person)] end,
  mnesia:transaction(F).

show_table(Table) ->
  do(qlc:q([X || X <- mnesia:table(Table)])).

% Entry :: {tableName, attributes}
add_entry(Entry) ->
  mnesia:transaction(fun() -> mnesia:write(Entry) end).

% OId :: {tableName, PK}
remove_entry(OId) ->
  mnesia:transaction(fun() -> mnesia:delete(OId) end).

do(Q) ->
  {atomic, Result} = mnesia:transaction(fun() -> qlc:e(Q) end),
  Result.

add_person(NrCC, Name, Address, Phone) ->
  add_entry(#person{nrCC=NrCC, name=Name, address=Address, phone=Phone}).

remove_person(NrCC) ->
  OId = {person, NrCC},
  remove_entry(OId).

add_book(ID, Name, Authors) ->
  add_entry(#book{id=ID, name=Name, authors=Authors}).

remove_book(ID) ->
  OId = {book, ID},
  remove_entry(OId).
