-module(server).

-include_lib("stdlib/include/qlc.hrl").

-import(lists, [member/2, foreach/2, sort/1]).

-export([start/0]).

-record(book,    {id, title, authors}).
-record(person,  {cc, name, address, phone}).
-record(request, {id, cc}).

% Auxiliar function to make a querie.
do(Q) ->
  F = fun() -> qlc:e(Q) end,
  case mnesia:transaction(F) of
    {atomic, Result} -> Result;
    {aborted, Reason} -> Reason
  end.

% Auxiliar function to add an entry in the database.
% Entry = {tableName, field1Value, ..., fieldNValue}
add_entry(Entry) ->
  mnesia:transaction(fun() -> mnesia:write(Entry) end).

% Auxiliar function to remove an entry in the database.
% OId = {tableName, PrimKeyValue}
remove_entry(OId) ->
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
  {person, 1000, "Tommy Shelby", "Birmingham", 1231819},
  {person, 2000, "Tyrion Lannister", "Casterly Rock", 2318321},
  {person, 3000, "Sergio Marquina", "Spain", 8978221},
  {person, 4000, "Dolores", "WestWorld", 7481562},
  {person, 5000, "Bernard Lowe", "WestWorld", 799962},
  {person, 6000, "John Snow", "Tower of Joy", 3471222},
  {person, 7000, "Andrés de Fonollosa", "Spain", 2421222}
].

% Setting up the server, which is, basically, starting the database.
setup() ->
  mnesia:create_schema([self()]),
  mnesia:start(),
  mnesia:create_table(person, [{attributes, record_info(fields, person)}]),
  mnesia:create_table(book, [{attributes, record_info(fields, book)}]),
  mnesia:create_table(request, [{attributes, record_info(fields, request)}]),
  mnesia:wait_for_tables([person, book, request], 20000),
  mnesia:transaction(fun() -> foreach(fun mnesia:write/1, bd()) end).

% Server's main loop
loop() ->
  receive
    % All of these functions used here, return a tuple like {aborted, Reason} or {atomic, ...}.
    % Read the comments next to everyone of them, for more particular info.
    % Lookup services
    {person_requests, From, CC}     -> From ! {server_pid, person_requests(CC)};
    {book_requests, From, Title}    -> From ! {server_pid, book_requests(Title)};
    {book_is_requested, From, ID}   -> From ! {server_pid, book_is_requested(ID)};
    {book_ids, From, Title}         -> From ! {server_pid, book_ids(Title)};
    {person_num_requests, From, CC} -> From ! {server_pid, person_num_requests(CC)};

    % Update services
    {add_request, From, ID, CC}     -> From ! {server_pid, add_request(ID, CC)};
    {remove_request, From, ID, CC}  -> From ! {server_pid, remove_request(ID, CC)}
  end,
  loop().

% Starts the server
start() -> register(server_pid, spawn(fun() -> setup(), loop() end)). % register so we can use it on module 'client'

% Given a person CC number, if isn't valid, returns {aborted, invalid_person_cc},
% otherwise returns {atomic, Requests}, where Requests is the sorted list of the books' IDs that the person with that CC number requested.
person_requests(CC) ->
  case valid(person, CC) of
    false -> {aborted, invalid_person_cc};
    true -> Requests = do(qlc:q([X#request.id || X <- mnesia:table(request), X#request.cc == CC])),
            {atomic, sort(Requests)}
  end.

% Given a book title, if that title isn't valid, returns {aborted, invalid_book_title},
% otherwise returns {atomic, CCs}, where CCs is the sorted list of the persons CCs that requested such book.
book_requests(Title) ->
  case valid(book, Title) of
    false -> {aborted, invalid_book_title};
    true -> {atomic, IDs} = book_ids(Title),                                                           % Get possible codes for the book with title 'Title'
            CCs = do(qlc:q([X#request.cc || X <- mnesia:table(request), member(X#request.id, IDs)])),  % Get every person that request one of this codes
            {atomic, sort(CCs)}
  end.

% Given a book ID, if that ID isn't valid, returns {aborted, invalid_book_id},
% otherwise returns {atomic, B}, where B is a boolean value.
% If B is true, then the book with that ID is requested, if it's false, then it isn't requested.
book_is_requested(ID) ->
  case valid(book, ID) of
    false -> {aborted, invalid_book_id};
    true -> Q = do(qlc:q([X || X <- mnesia:table(request), X#request.id == ID])), {atomic, not(Q == [])}
  end.

% Given a book title, if that title isn't valid, returns {aborted, invalid_book_title},
% otherwise returns {atomic, IDs}, where IDs is the sorted list of the books' IDs with such title.
book_ids(Title) ->
  case valid(book, Title) of
    false -> {aborted, invalid_book_title};
    true -> IDs = do(qlc:q([X#book.id || X <- mnesia:table(book), X#book.title == Title])), {atomic, sort(IDs)}
  end.

% Given a person CC, if that CC isn't valid, returns {aborted, invalid_person_cc},
% otherwise returns {atomic, Qt}, where Qt is the number of books requested by the person with that CC number.
person_num_requests(CC) ->
  case valid(person, CC) of
    false -> {aborted, invalid_person_cc};
    true -> {atomic, Requests} = person_requests(CC), Qt = length(Requests), {atomic, Qt}
  end.

% Given a book ID and a person CC, if one of them is invalid, returns {aborted, invalid_book_id}, if the one invalid is the ID, or {aborted, invalid_person_cc}, if is the person CC invalid,
% otherwise returns {atomic}, if the operation went well, that is, the server register a request of the book ID in the name of that person,
% or returns {aborted, invalid_request}, if that the book was already requested, so the person can't request the book now.
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

% Given a book ID and a person CC, if one of them is invalid, returns {aborted, invalid_book_id}, if the one invalid is the ID, or {aborted, invalid_person_cc}, if is the person CC invalid,
% otherwise returns {aborted, invalid_request}, if the book isn't requested by that person, so she can't return it,
% or returns {atomic}, if the operation went well, that is, the server register that the person returned the book id.
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

% Simple function to validate the data, that is, confirm in the database if some value of some field of an entity (book, person, request) exists.
% Returns true if such value exists for an entity, false, otherwise.
% For instance: valid(person, 1231), would return true if that CC number exists in the database.
% Only implemented the validation for the fields that we needed, which are: for the book -> id, title; for the person: cc; for the request: {id, cc} (validating an entry)
% Care with extension!
% Since we are only looking for one or two attributes in each entity, with them being with different types, when it's the case of two, we don't need to worry about for what field the value is.
% But adding more attributes to look for, could lead to bugs!
% For instance, if we now needed to search for the phone number of a person, extending that in the line for the person entity, following what we did, without any precaution,
% could false validate phone numbers or CC numbers, since a CC number could have the same value as the phone number!
valid(Entity, Value) ->
  NotEmptyList = fun([]) -> false; (_) -> true end,
  Or = fun(false, false) -> false; (_, _) -> true end,
  case Entity of
    book    -> NotEmptyList(do(qlc:q([X || X <- mnesia:table(book), Or(X#book.id == Value, X#book.title == Value)])));
    person  -> NotEmptyList(do(qlc:q([X || X <- mnesia:table(person), X#person.cc == Value])));
    request -> NotEmptyList(do(qlc:q([X || X <- mnesia:table(request), X#request.id == element(1, Value), X#request.cc == element(2, Value)])))
  end.
