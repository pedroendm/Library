-module(client).

-export([person_requests/2, book_requests/2, book_is_requested/2, book_ids/2, person_num_requests/2, % Lookup
         request_book/3, return_book/3                                                               % Update
        ]).

% Lookup methods
person_requests(Server, CC) ->
  Server ! {person_requests, self(), CC},
  receive
    {aborted, Reason} -> error_message(Reason);
    {atomic, Requests} -> "[System] IDs of the books requested by the person with CC number " ++ CC ++ ": " ++ lists:concat(lists:join(", ", Requests)) ++ "."
  end.

book_requests(Server, Title) ->
  Server ! {book_requests, self(), Title},
  receive
    {aborted, Reason} -> error_message(Reason);
    {atomic, CCs} -> "[System] CC number of the people who requested the book \"" ++ Title ++ "\": " ++ lists:concat(lists:join(", ", CCs)) ++ "."
  end.

book_is_requested(Server, ID) ->
  Server ! {book_is_requested, self(), ID},
  receive
    {aborted, Reason} -> error_message(Reason);
    {atomic, true} -> "[System] The book is requested.~n";
    {atomic, false} -> "[System] The book isn't requested.~n"
  end.

book_ids(Server, Title) ->
  Server ! {book_ids, self(), Title},
  receive
    {aborted, Reason} -> error_message(Reason);
    {atomic, IDs} -> "[System] Available IDs of the book \"" ++ Title ++ "\": " ++ lists:concat(lists:join(", ", IDs)) ++ "."
  end.

person_num_requests(Server, CC) ->
  Server ! {person_num_requests, self(), CC},
  receive
    {aborted, Reason} -> error_message(Reason);
    {atomic, Qt} -> "[System] The person with CC number " ++ CC ++ " has requested " ++ Qt ++ " books."
  end.

% Update methods
request_book(Server, ID, CC) ->
  Server ! {add_request, self(), ID, CC},
  receive
    {aborted, Reason} -> error_message(Reason);
    {atomic} -> "[System] The book was requested with sucess."
  end.

return_book(Server, ID, CC) ->
  Server ! {remove_request, self(), ID, CC},
  receive
    {aborted, Reason} -> error_message(Reason);
    {atomic} -> "[System] The book was returned with sucess."
  end.

error_message(Reason) ->
  Map = #{invalid_book_title  => "there's no book in this library with that title.",
          invalid_book_id     => "there's no book in this library with that ID.",
          invalid_person_cc   => "there's no person in the system with that CC number.",
          invalid_request     => "the book is already requested.",
          invalid_return      => "you don't possess the book to return it."},
  "[System] Operation failed: " ++ maps:get(Reason, Map).
