-module(client).

-export([create_account/5, delete_account/2,
         request_book/3, return_book/3,
         person_requests/2, book_requests/2, book_is_requested/2,
         show_table/2]).

create_account(Server, NrCC, Name, Address, Phone) ->
  Server ! {add_person, self(), NrCC, Name, Address, Phone},
  receive
    {aborted, duplicate_person_cc} -> io:fwrite("There's already a person in the system with that CC number. Account not created.~n");
    {atomic} -> io:fwrite("Account created with sucess.~n")
  end.

% Return all books hold by the person.
delete_account(Server, NrCC) ->
  Server ! {remove_person, self(), NrCC},
  receive
    {aborted, invalid_person_cc} -> io:fwrite("There's already no person in the system with that CC number.~n");
    {atomic} -> io:fwrite("Account removed with sucess.~n")
  end.

request_book(Server, ID, NrCC) ->
  Server ! {add_request, self(), ID, NrCC},
  receive
    {aborted, invalid_book_id} -> io:fwrite("There's no book in this library with that ID.~n");
    {aborted, invalid_person_cc} -> io:fwrite("There's no person in the system with that CC number.~n");
    {aborted, book_is_requested} -> io:fwrite("Book is already requested.~n");
    {atomic} -> io:fwrite("Book requested with sucess.~n");
  end.

return_book(Server, ID, NrCC) ->
  Server ! {remove_request, self(), ID, NrCC},
  receive
    
  end.

person_requests(Server, NrCC) ->
  Server ! {person_requests, self(), NrCC},
  receive
    {aborted, invalid_person_cc} -> io:fwrite("There's no person in the system with that CC number.~n");
    {atomic, Requests} -> io:fwrite("Books requested by the person with CC number ~w: ~w~n", [NrCC, Requests])
  end.

book_requests(Server, Title) ->
  Server ! {book_requests, self(), Title},
  receive
    {aborted, invalid_book_title} -> io:fwrite("There's no book in this library with that title.~n");
    {atomic, CCs} -> io:fwrite("Citizen card numbers of the people who requested the book ~p: ~w~n", [Title, CCs])
  end.

book_is_requested(Server, ID) ->
  Server ! {book_is_requested, self(), ID},
  receive
    {aborted, invalid_book_id} -> io:fwrite("There's no book in this library with that ID.~n");
    {atomic, true} -> io:fwrite("The book is requested.~n");
    {atomic, false} -> io:fwrite("The book isn't requested.~n")
  end.

show_table(Server, TableName) ->
  Server ! {show_table, self(), TableName},
  receive
    Response -> Response
  end.
