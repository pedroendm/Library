-module(client).

-export([person_requests/1, book_requests/1, book_is_requested/1, book_ids/1, person_num_requests/1,
         request_book/2, return_book/2
        ]).

% All functions follow the same form.
% Simply, send a message to the server, identifying the "type of service", the client (so we can get an answer back) and the values significant to the message, which are self-explanatory by their names.
% Then, print, in a nice way, the answer that we got back.

% Lookup methods
person_requests(CC) ->
  server_pid ! {person_requests, self(), CC},
  receive
    {server_pid, {aborted, Reason}} -> io:format(error_message(Reason));
    {server_pid, {atomic, Requests}} -> case Requests of
                                          [] -> io:format("The person with CC number ~w has no requests made.~n", [CC]);
                                          _  -> io:format("IDs of the books requested by the person with CC number ~w: ~s.~n", [CC, lists:concat(lists:join(", ", Requests))])
                                        end
  end.

book_requests(Title) ->
  server_pid ! {book_requests, self(), Title},
  receive
    {server_pid, {aborted, Reason}} -> io:format(error_message(Reason));
    {server_pid, {atomic, CCs}} -> case CCs of
                                [] -> io:format("No one requested the book \"~s\".~n", [Title]);
                                _  -> io:format("CC numbers of the people who requested the book \"~s\": ~s.~n", [Title, lists:concat(lists:join(", ", CCs))])
                               end
  end.

book_is_requested(ID) ->
  server_pid ! {book_is_requested, self(), ID},
  receive
    {server_pid, {aborted, Reason}} -> io:format(error_message(Reason));
    {server_pid, {atomic, true}} -> io:format("The book is requested.~n");
    {server_pid, {atomic, false}} -> io:format("The book isn't requested.~n")
  end.

book_ids(Title) ->
  server_pid ! {book_ids, self(), Title},
  receive
    {server_pid, {aborted, Reason}} -> io:format(error_message(Reason));
    {server_pid, {atomic, IDs}} -> io:format("Available IDs of the book \"~s\": ~s.~n", [Title, lists:concat(lists:join(", ", IDs))])
  end.

person_num_requests(CC) ->
  server_pid ! {person_num_requests, self(), CC},
  receive
    {server_pid, {aborted, Reason}} -> io:format(error_message(Reason));
    {server_pid, {atomic, Qt}} -> case Qt of
                                1 -> io:format("The person with CC number ~w has requested 1 book.~n", [CC]);
                                _ -> io:format("The person with CC number ~w has requested ~w books.~n", [CC, Qt])
                              end
  end.

% Update methods
request_book(ID, CC) ->
  server_pid ! {add_request, self(), ID, CC},
  receive
    {server_pid, {aborted, Reason}} -> io:format(error_message(Reason));
    {server_pid, {atomic}} -> io:format("The book was requested with sucess.~n")
  end.

return_book(ID, CC) ->
  server_pid ! {remove_request, self(), ID, CC},
  receive
    {server_pid, {aborted, Reason}} -> io:format(error_message(Reason));
    {server_pid, {atomic}} -> io:format("The book was returned with sucess.~n")
  end.

% Utility function
% IO related to reporting errors
error_message(Reason) ->
  Map = #{invalid_book_title  => "There's no book in this library with that title.",
          invalid_book_id     => "There's no book in this library with that ID.",
          invalid_person_cc   => "There's no person in the system with that CC number.",
          invalid_request     => "The book is already requested.",
          invalid_return      => "You don't possess the book to return it."},
  maps:get(Reason, Map) ++ "~n".
