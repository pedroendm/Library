-module(server).

-include_lib("stdlib/include/qlc.hrl").

-import(lists, [member/2, foreach/2]).
-import(utils, [empty_list/1, or_list/1]).

-export([start/0, setup/0, valid/2]).

-record(book, {id, title, authors}).
-record(person, {cc, name, address, phone}).
-record(request, {id, cc}).

bd() -> [
  % Books
  {book, 1, "Programming Erlang: software for a concurrent world", {"Joe Armstrong"}},
  {book, 2, "The little book of Semaphores", {"Allen B. Downey"}},
  {book, 3, "Programming Erlang: software for a concurrent world", {"Joe Armstrong"}},
  {book, 4, "Numerical Otimization", {"Jorge Nocedal", "Stephen J. Wrigth"}},
  {book, 5, "A first course in mathematical modeling", {"Frank R. Giordano", "William P. Fox", "Steven B. Horton"}},
  {book, 6, "Convex Optimization", {"Stephen Boyd", "Lieven Vandenberghe"}},

  % People
  {person, 10, "Tommy Shelby", "Birmingham", 1231812},
  {person, 20, "Tyrion Lannister", "Casterly Rock", 2312321},
  {person, 30, "The Professor", "Spain", 8978221},
  {person, 40, "Dolores", "WestWorld", 7481262}
].

setup() ->
    mnesia:create_schema([self()]),
    mnesia:start(),
    mnesia:create_table(person, [{attributes, record_info(fields, person)}]),
    mnesia:create_table(book, [{attributes, record_info(fields, book)}]),
    mnesia:create_table(request, [{attributes, record_info(fields, request)}]),
    mnesia:wait_for_tables([person, book, request], 20000),
    mnesia:transaction(fun() -> foreach(fun mnesia:write/1, bd()) end).

loop() ->
  receive
    % Lookup
    {person_requests, From, CC}     -> From ! person_requests(CC), loop();
    {book_requests, From, Title}    -> From ! book_requests(Title), loop();
    {book_is_requested, From, ID}   -> From ! book_is_requested(ID), loop();
    {book_ids, From, Title}         -> From ! book_ids(Title), loop();
    {person_num_requests, From, CC} -> From ! person_num_requests(CC), loop();

    % Update
    {add_request, From, ID, CC}     -> From ! add_request(ID, CC), loop();
    {remove_request, From, ID, CC}  -> From ! remove_request(ID, CC), loop()
  end.

start() -> spawn(fun() -> setup(), loop() end).


person_requests(CC) ->
  case valid(person, CC) of
    false -> {aborted, invalid_person_cc};
    true -> Requests = do(qlc:q([element(1, X#request.id) || X <- mnesia:table(request), element(2, X#request.id) == CC])), {atomic, Requests}
  end.

book_requests(Title) ->
  case valid(book, Title) of
    false -> {aborted, invalid_book_title};
    true -> IDs = do(qlc:q([X#book.id || X <- mnesia:table(book), X#book.title == Title])), % Get possible codes for the book with title 'Title'
            CCs = do(qlc:q([X#request.cc || X <- mnesia:table(request), member(X#request.id, IDs)])),  % Get every person that request one of this codes
            {atomic, CCs}
  end.

book_is_requested(ID) ->
  case valid(book, ID) of
    false -> {aborted, invalid_book_id};
    true -> Q = do(qlc:q([X || X <- mnesia:table(request), X#request.id == ID])), {atomic, not(Q == [])}
  end.

book_ids(Title) ->
  case valid(book, Title) of
    false -> {aborted, invalid_book_title};
    true -> IDs = do(qlc:q([X#book.id || X <- mnesia:table(book), X#book.title == Title])), {atomic, IDs}
  end.

person_num_requests(CC) ->
  case valid(person, CC) of
    false -> {aborted, invalid_person_cc};
    true -> Qt = length(person_requests(CC)), {atomic, Qt}
  end.

add_request(ID, CC) ->
  case valid(book, ID) of
    false -> {aborted, invalid_book_id};
    true -> case valid(person, CC) of
              false -> {aborted, invalid_person_cc};
              true -> case book_is_requested(ID) of
                        {atomic, false} -> add_entry(#request{id=ID, cc=CC}), {atomic};
                        {atomic, true}  -> {aborted, invalid_request}
                      end
            end
  end.

remove_request(ID, CC) ->
  case valid(book, ID) of
    false -> {aborted, invalid_book_id};
    true -> case valid(person, CC) of
              false -> {aborted, invalid_person_cc};
              true -> case valid(request, {ID, CC}) of 
                        false -> {aborted, invalid_return};
                        true -> remove_entry(#request{id=ID, cc=CC}), {atomic}
                      end
            end
  end.


do(Q) ->
  F = fun() -> qlc:e(Q) end,
  case mnesia:transaction(F) of
    {atomic, Result} -> Result;
    {aborted, Reason} -> Reason
  end.

add_entry(Entry) -> % Entry :: {tableName, attributes}
  mnesia:transaction(fun() -> mnesia:write(Entry) end).

remove_entry(OId) -> % OId :: {tableName, PK}
  mnesia:transaction(fun() -> mnesia:delete(OId) end).

valid(Entity, Value) ->
  NotEmptyList = fun([]) -> false; (_) -> true end,
  Or = fun(false, false) -> false; (_, _) -> true end,
  case Entity of
    book    -> NotEmptyList(do(qlc:q([X || X <- mnesia:table(book), Or(X#book.id == Value, X#book.title == Value)])));
    person  -> NotEmptyList(do(qlc:q([X || X <- mnesia:table(person), X#person.cc == Value])));
    request -> NotEmptyList(do(qlc:q([X || X <- mnesia:table(request), X#request.id == element(1, Value), X#request.cc == element(2, Value)])))
  end.
