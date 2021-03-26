-module(client).

-export([person_requests/2, book_requests/2, book_is_requested/2, book_ids/2, person_num_requests/2, % Lookup
         request_book/3, return_book/3                                                               % Update
        ]).

% Lookup methods
person_requests(Server, CC) ->
  Server ! {person_requests, self(), CC},
  receive
    {Server, {aborted, Reason}} -> io:format(error_message(Reason));
    {Server, {atomic, Requests}} -> case Requests of
                                      [] -> io:format("[System] The person with CC number ~w has no requests made.~n", [CC]);
                                      _  -> io:format("[System] IDs of the books requested by the person with CC number ~w: ~s.~n", [CC, lists:concat(lists:join(", ", Requests))])
                                    end
  end.

book_requests(Server, Title) ->
  Server ! {book_requests, self(), Title},
  receive
    {Server, {aborted, Reason}} -> io:format(error_message(Reason));
    {Server, {atomic, CCs}} -> case CCs of
                                [] -> io:format("[System] No one requested the book \"~s\".~n", [Title]);
                                _  -> io:format("[System] CC numbers of the people who requested the book \"~s\": ~s.~n", [Title, lists:concat(lists:join(", ", CCs))])
                               end
  end.

book_is_requested(Server, ID) ->
  Server ! {book_is_requested, self(), ID},
  receive
    {Server, {aborted, Reason}} -> io:format(error_message(Reason));
    {Server, {atomic, true}} -> io:format("[System] The book is requested.~n");
    {Server, {atomic, false}} -> io:format("[System] The book isn't requested.~n")
  end.

book_ids(Server, Title) ->
  Server ! {book_ids, self(), Title},
  receive
    {Server, {aborted, Reason}} -> io:format(error_message(Reason));
    {Server, {atomic, IDs}} -> io:format("[System] Available IDs of the book \"~s\": ~s.~n", [Title, lists:concat(lists:join(", ", IDs))])
  end.

person_num_requests(Server, CC) ->
  Server ! {person_num_requests, self(), CC},
  receive
    {Server, {aborted, Reason}} -> io:format(error_message(Reason));
    {Server, {atomic, Qt}} -> case Qt of
                                1 -> io:format("[System] The person with CC number ~w has requested 1 book.~n", [CC]);
                                _ -> io:format("[System] The person with CC number ~w has requested ~w books.~n", [CC, Qt])
                              end
  end.

% Update methods
request_book(Server, ID, CC) ->
  Server ! {add_request, self(), ID, CC},
  receive
    {Server, {aborted, Reason}} -> io:format(error_message(Reason));
    {Server, {atomic}} -> io:format("[System] The book was requested with sucess.~n")
  end.

return_book(Server, ID, CC) ->
  Server ! {remove_request, self(), ID, CC},
  receive
    {Server, {aborted, Reason}} -> io:format(error_message(Reason));
    {Server, {atomic}} -> io:format("[System] The book was returned with sucess.~n")
  end.

error_message(Reason) ->
  Map = #{invalid_book_title  => "There's no book in this library with that title.",
          invalid_book_id     => "There's no book in this library with that ID.",
          invalid_person_cc   => "There's no person in the system with that CC number.",
          invalid_request     => "The book is already requested.",
          invalid_return      => "You don't possess the book to return it."},
  "[System] " ++ maps:get(Reason, Map) ++ "~n".
