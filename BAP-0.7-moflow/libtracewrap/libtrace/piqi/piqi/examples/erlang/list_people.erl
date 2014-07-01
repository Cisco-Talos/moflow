-module(list_people).
-compile(export_all).

-include("addressbook_piqi.hrl").


% Main function:  Reads the entire address book from a file and prints all
% the information inside.
run([Filename]) ->
    % Read the existing address book.
    {ok, Bytes} = file:read_file(Filename),
    AddressBook = addressbook_piqi:parse_address_book(Bytes),
    list_people(AddressBook),
    ok;

run(_) ->
    erlang:halt(1).
    

% Iterates though all people in the AddressBook and prints info about them.
list_people(X) ->
    #address_book{person = People} = X,
    lists:foreach(fun list_person/1, People).


list_person(X) ->
    #person{
        name = Name,
        id = Id,
        email = Email,
        phone = Phones } = X,
    io:format("Person ID: ~w~n", [Id]),
    io:format("  Name: ~s~n", [Name]),
    print_email(Email),
    lists:foreach(fun print_phone_number/1, Phones).


print_email('undefined') -> ok;
print_email(Email) ->
    io:format("  E-mail address: ~s~n", [Email]).


print_phone_number(X) ->
    #person_phone_number{
        number = Number,
        type = PhoneType } = X,
    case PhoneType of
        mobile ->
            io:format("  Mobile phone #: ");
        home ->
            io:format("  Home phone #: ");
        work ->
            io:format("  Work phone #: ")
    end,
    io:format("~s~n", [Number]).

