-module(client).

-export([show_table/2, create_account/5, delete_account/2,
         person_requests/2, book_requests/2, book_is_requested/2, book_ids/2, person_num_requests/2, % Lookup
         request_book/3, return_book/3 % Update
        ]).

show_table(Server, TableName) ->
  Server ! {show_table, self(), TableName},
  receive
    Response -> Response
  end.

create_account(Server, NrCC, Name, Address, Phone) ->
  Server ! {add_person, self(), NrCC, Name, Address, Phone},
  receive
    {aborted, Reason} -> error_message(Reason);
    {atomic} -> io:fwrite("Account created with sucess.~n")
  end.

delete_account(Server, NrCC) ->
  Server ! {remove_person, self(), NrCC},
  receive
    {aborted, Reason} -> error_message(Reason);
    {atomic} -> io:fwrite("Account removed with sucess.~n")
  end.

% Lookup methods
person_requests(Server, NrCC) ->
  Server ! {person_requests, self(), NrCC},
  receive
    {aborted, Reason} -> error_message(Reason);
    {atomic, Requests} -> io:fwrite("Books requested by the person with CC number ~w: ~w~n", [NrCC, Requests])
  end.

book_requests(Server, Title) ->
  Server ! {book_requests, self(), Title},
  receive
    {aborted, Reason} -> error_message(Reason);
    {atomic, CCs} -> io:fwrite("Citizen card numbers of the people who requested the book ~p: ~w~n", [Title, CCs])
  end.

book_is_requested(Server, ID) ->
  Server ! {book_is_requested, self(), ID},
  receive
    {aborted, Reason} -> error_message(Reason);
    {atomic, true} -> io:fwrite("The book is requested.~n");
    {atomic, false} -> io:fwrite("The book isn't requested.~n")
  end.

book_ids(Server, Title) ->
  Server ! {book_ids, self(), Title},
  receive
    {aborted, Reason} -> error_message(Reason);
    {atomic, IDs} -> io:fwrite("IDs of the book with title \"~w\": ~w~n", [Title, IDs])
  end.

person_num_requests(Server, CC) ->
  Server ! {person_num_requests, self(), CC},
  receive
    {aborted, Reason} -> error_message(Reason);
    {atomic, Qt} -> io:fwrite("The person with CC number ~w has requested ~w books.~n", [CC, Qt])
  end.

% Update methods
request_book(Server, ID, NrCC) ->
  Server ! {add_request, self(), ID, NrCC},
  receive
    {aborted, Reason} -> error_message(Reason);
    {atomic} -> io:fwrite("Book requested with sucess.~n")
  end.

return_book(Server, ID, NrCC) ->
  Server ! {remove_request, self(), ID, NrCC},
  receive
    {aborted, Reason} -> error_message(Reason);
    {atomic} -> io:fwrite("Returned book with sucess.~n")
  end.

error_message(Reason) ->
  Map = #{invalid_book_title  => "there's no book in this library with that title.",
          invalid_book_id     => "there's no book in this library with that ID.",
          invalid_person_cc   => "there's no person in the system with that CC number.",
          duplicate_person_cc => "there's already a person in the system with that CC number.",
          invalid_request     => "the book is already requested by someone.",
          invalid_return      => "you don't possess the book to return it."},
  "[System] Operation failed: " ++ maps:get(Reason, Map).
