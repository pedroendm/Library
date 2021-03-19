-module(server).

-include_lib("stdlib/include/qlc.hrl").

-import(lists, [member/2, foreach/2]).
-import(utils, [empty_list/1, or_list/1]).

-export([setup/0, start/0, loop/0,
         show_table/1, person_requests/1, book_requests/1,
         add_person/4, remove_person/1]).

% person's primary key: nrCC
-record(person, {cc, name, address, phone}).
% book's primary key: id
-record(book, {id, title, authors}).
% request's primary key: {id(book), nrCC(person)}
-record(request, {id, valid}).


bookshelf() -> [
  {book, 1, "Programming Erlang: software for a concurrent world", {"Joe Armstrong"}},
  {book, 2, "The little book of Semaphores", {"Allen B. Downey"}},
  {book, 3, "Programming Erlang: software for a concurrent world", {"Joe Armstrong"}}

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

loop() ->
  receive
    {show_table, From, Table} -> From ! show_table(Table), loop();
    {add_person, From, CC, Name, Address, Phone} -> From ! add_person(CC, Name, Address, Phone), loop();
    {remove_person, From, CC} -> From ! remove_person(CC), loop();

    % Lookup
    {person_requests, From, CC} -> From ! person_requests(CC), loop();
    {book_requests, From, Title} -> From ! book_requests(Title), loop();
    {book_is_requested, From, ID} -> From ! book_is_requested(ID), loop();
    {book_ids, From, Title} -> From ! book_ids(Title), loop();
    {person_num_requests, From, CC} -> From ! person_num_requests(CC), loop();

    % Update
    {add_request, From, ID, CC} -> From ! add_request(ID, CC), loop();
    {remove_request, From, ID, CC} -> From ! remove_request(ID, CC), loop()
  end.

start() -> spawn(fun() -> setup(), loop() end).

% Entry :: {tableName, attributes}
add_entry(Entry) ->
  mnesia:transaction(fun() -> mnesia:write(Entry) end).

% OId :: {tableName, PK}
remove_entry(OId) ->
  mnesia:transaction(fun() -> mnesia:delete(OId) end).

do(Q) ->
  F = fun() -> qlc:e(Q) end,
  case mnesia:transaction(F) of
    {atomic, Result} -> Result;
    {aborted, Reason} -> Reason
  end.

show_table(Table) ->
  do(qlc:q([X || X <- mnesia:table(Table)])).


add_person(CC, Name, Address, Phone) ->
  case exist_person_cc(CC) of
     false -> add_entry(#person{cc=CC, name=Name, address=Address, phone=Phone}), {atomic};
     true -> {aborted, duplicate_person_cc}
  end.

remove_person(CC) ->
  case exist_person_cc(CC) of
    false -> {aborted, invalid_person_cc};
    true ->  OId = {person, CC}, remove_entry(OId), {atomic}
  end.


% Returns a list with the books' id requested by the person with CC number 'NrCC'
person_requests(CC) ->
  case exist_person_cc(CC) of
    false -> {aborted, invalid_person_cc};
    true -> Requests = do(qlc:q([element(1, X#request.id) || X <- mnesia:table(request), element(2, X#request.id) == CC])),
            {atomic, Requests}
  end.

% Returns a list with the people that request the book with title "Title"
book_requests(Title) ->
  case exist_book_title(Title) of
    false -> {aborted, invalid_book_title};
    true ->
      % Get possible codes for the book with title Title
      IDs = do(qlc:q([X#book.id || X <- mnesia:table(book), X#book.title == Title])),
      % Get every person that request one of this codes
      CCs = do(qlc:q([element(2, X#request.id) || X <- mnesia:table(request), member(element(1, X#request.id), IDs)])),
      {atomic, CCs}
  end.

% Returns true if the book is already requested, false otherwise.
book_is_requested(ID) ->
  case exist_book_id(ID) of
    false -> {aborted, invalid_book_id};
    true -> Flags = do(qlc:q([X#request.valid || X <- mnesia:table(request), element(1, X#request.id) == ID])),
            Is_Requested = utils:or_list(Flags),
            {atomic, Is_Requested}
  end.

% Returns the ids of the book with title 'Title'
book_ids(Title) ->
  case exist_book_title(Title) of
    false -> {aborted, invalid_book_title};
    true -> IDs = do(qlc:q([X#book.id || X <- mnesia:table(book), X#book.title == Title])),
            {atomic, IDs}
  end.

% Returns the number of requests made by the person with CC number 'CC'
person_num_requests(CC) ->
  case exist_person_cc(CC) of
    false -> {aborted, invalid_person_cc};
    true -> Qt = length(person_requests(CC)),
            {atomic, Qt}
  end.

add_request(ID, CC) ->
  case exist_book_id(ID) of
    false -> {aborted, invalid_book_id};
    true -> case exist_person_cc(CC) of
              false -> {aborted, invalid_person_cc};
              true -> case book_is_requested(ID) of % See if already requested, doesn't matter by who.
                        false -> add_entry(#request{id={ID, CC}, valid=true}), {atomic};
                        true  -> {aborted, invalid_request}
                      end
            end
  end.

remove_request(ID, CC) ->
  case exist_book_id(ID) of
    false -> {aborted, invalid_book_id};
    true -> case exist_person_cc(CC) of
              false -> {aborted, invalid_person_cc};
              % BUG HERE: if theres no such entry, add one false returned
              %           Care can be requested by other
              true -> case member(ID, person_requests(CC)) of % If the book is requested by the person
                        false -> {aborted, invalid_return};
                        true -> add_entry(#request{id={ID, CC}, valid=false}), {atomic}
                      end
            end
  end.

exist_person_cc(CC) ->
  People = do(qlc:q([X || X <- mnesia:table(person), X#person.cc == CC])),
  not(utils:empty_list(People)).

exist_book_title(Title) ->
  Books = do(qlc:q([X || X <- mnesia:table(book), X#book.title == Title])),
  not(utils:empty_list(Books)).

exist_book_id(ID) ->
  Books = do(qlc:q([X || X <- mnesia:table(book), X#book.id == ID])),
  not(utils:empty_list(Books)).
