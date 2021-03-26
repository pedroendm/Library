-module(server).

-include_lib("stdlib/include/qlc.hrl").

-import(lists, [member/2, foreach/2, sort/1]).
-import(utils, [empty_list/1, or_list/1]).

-export([start/0]).

-record(book,    {id, title, authors}).
-record(person,  {cc, name, address, phone}).
-record(request, {id, cc}).

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

bd() -> [
  % Books
  {book, 1, "Programming Erlang: software for a concurrent world", {"Joe Armstrong"}},
  {book, 2, "The little book of Semaphores", {"Allen B. Downey"}},
  {book, 3, "Programming Erlang: software for a concurrent world", {"Joe Armstrong"}},
  {book, 4, "Numerical Otimization", {"Jorge Nocedal", "Stephen J. Wrigth"}},
  {book, 5, "A first course in mathematical modeling", {"Frank R. Giordano", "William P. Fox", "Steven B. Horton"}},
  {book, 6, "Convex Optimization", {"Stephen Boyd", "Lieven Vandenberghe"}},
  {book, 7, "Introduction to Automata Theory, Languages and Computations", {"John E. Hopcroft", "Rajeev Motwani", "Jeffrey D. Ullman"}},
  {book, 8, "Introduction to Automata Theory, Languages and Computations", {"John E. Hopcroft", "Rajeev Motwani", "Jeffrey D. Ullman"}},
  {book, 9, "Introduction to the Theory of Computation", {"Michael Sipser"}},
  {book, 10, "Introduction to Operations Research", {"Frederich S. Hillier", "Gerald J. Lieberman"}},
  {book, 11, "Convex Optimization", {"Stephen Boyd", "Lieven Vandenberghe"}},
  {book, 12, "Convex Optimization", {"Stephen Boyd", "Lieven Vandenberghe"}},
  {book, 13, "Deep Learning", {"Ian Goodfellow", "Yoshua Bengio", "Aaron Courville"}},
  {book, 14, "A Computational Introduction to Number Theory and Algebra", {"Victor Shoup"}},
  {book, 15, "Introduction to Automata Theory, Languages and Computations", {"John E. Hopcroft", "Rajeev Motwani", "Jeffrey D. Ullman"}},

  % People
  {person, 1000, "Tommy Shelby", "Birmingham", 1231812},
  {person, 2000, "Tyrion Lannister", "Casterly Rock", 2312321},
  {person, 3000, "Sergio Marquina", "Spain", 8978221},
  {person, 4000, "Dolores", "WestWorld", 7481262},
  {person, 5000, "Bernard Lowe", "WestWorld", 7481262},
  {person, 6000, "John Snow", "Tower of Joy", 7481262},
  {person, 7000, "Andrés de Fonollosa", "Spain", 7481262}
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
    {person_requests, From, CC}     -> From ! {self(), person_requests(CC)}, loop();
    {book_requests, From, Title}    -> From ! {self(), book_requests(Title)}, loop();
    {book_is_requested, From, ID}   -> From ! {self(), book_is_requested(ID)}, loop();
    {book_ids, From, Title}         -> From ! {self(), book_ids(Title)}, loop();
    {person_num_requests, From, CC} -> From ! {self(), person_num_requests(CC)}, loop();

    % Update
    {add_request, From, ID, CC}     -> From ! {self(), add_request(ID, CC)}, loop();
    {remove_request, From, ID, CC}  -> From ! {self(), remove_request(ID, CC)}, loop()
  end.

start() -> spawn(fun() -> setup(), loop() end).

person_requests(CC) ->
  case valid(person, CC) of
    false -> {aborted, invalid_person_cc};
    true -> Requests = do(qlc:q([X#request.id || X <- mnesia:table(request), X#request.cc == CC])), {atomic, sort(Requests)}
  end.

book_requests(Title) ->
  case valid(book, Title) of
    false -> {aborted, invalid_book_title};
    true -> IDs = do(qlc:q([X#book.id || X <- mnesia:table(book), X#book.title == Title])),            % Get possible codes for the book with title 'Title'
            CCs = do(qlc:q([X#request.cc || X <- mnesia:table(request), member(X#request.id, IDs)])),  % Get every person that request one of this codes
            {atomic, sort(CCs)}
  end.

book_is_requested(ID) ->
  case valid(book, ID) of
    false -> {aborted, invalid_book_id};
    true -> Q = do(qlc:q([X || X <- mnesia:table(request), X#request.id == ID])), {atomic, not(Q == [])}
  end.

book_ids(Title) ->
  case valid(book, Title) of
    false -> {aborted, invalid_book_title};
    true -> IDs = do(qlc:q([X#book.id || X <- mnesia:table(book), X#book.title == Title])), {atomic, sort(IDs)}
  end.

person_num_requests(CC) ->
  case valid(person, CC) of
    false -> {aborted, invalid_person_cc};
    true -> {atomic, Requests} = person_requests(CC), Qt = length(Requests), {atomic, Qt}
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
                        true -> remove_entry({request, ID}), {atomic}
                      end
            end
  end.

valid(Entity, Value) ->
  NotEmptyList = fun([]) -> false; (_) -> true end,
  Or = fun(false, false) -> false; (_, _) -> true end,
  case Entity of
    book    -> NotEmptyList(do(qlc:q([X || X <- mnesia:table(book), Or(X#book.id == Value, X#book.title == Value)])));
    person  -> NotEmptyList(do(qlc:q([X || X <- mnesia:table(person), X#person.cc == Value])));
    request -> NotEmptyList(do(qlc:q([X || X <- mnesia:table(request), X#request.id == element(1, Value), X#request.cc == element(2, Value)])))
  end.
