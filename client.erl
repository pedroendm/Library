-module(client).

-export([create_account/5, delete_account/2,
         request_book/3, return_book/3,
         show_table/2, person_requests/2]).

create_account(Server, NrCC, Name, Address, Phone) ->
  Server ! {add_person, self(), NrCC, Name, Address, Phone}.

% Return all books hold by the person.
delete_account(Server, NrCC) ->
  Server ! {remove_person, self(), NrCC}.

request_book(Server, ID, NrCC) ->
  Server ! {add_request, self(), ID,  NrCC}.

return_book(Server, ID, NrCC) ->
  Server ! {remove_request, self(), ID,  NrCC}.

person_requests(Server, NrCC) ->
  Server ! {person_requests, self(), NrCC},
  receive
    Response -> Response
  end.

show_table(Server, TableName) ->
  Server ! {show_table, self(), TableName},
  receive
    Response -> Response
  end.
