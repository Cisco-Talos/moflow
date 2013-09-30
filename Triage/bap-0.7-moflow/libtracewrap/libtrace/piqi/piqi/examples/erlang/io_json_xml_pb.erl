-module(io_json_xml_pb).
-compile(export_all).

-include("addressbook_piqi.hrl").


run([Filename]) ->
    % Read the existing address book in Protobuf format
    {ok, Bytes} = file:read_file(Filename),

    AddressBook = addressbook_piqi_ext:parse_address_book(Bytes, 'pb'),
    io:format("~n~nErlang: ~n~n~p~n", [AddressBook]),


    Json = addressbook_piqi_ext:gen_address_book(AddressBook, 'json'),
    io:format("~n~nJSON: ~n~n~s~n", [Json]),
    AddressBook = addressbook_piqi_ext:parse_address_book(Json, 'json'),


    JsonPretty = addressbook_piqi_ext:gen_address_book(AddressBook, 'json_pretty'),
    io:format("~n~nJSON pretty: ~n~n~s~n", [JsonPretty]),
    AddressBook = addressbook_piqi_ext:parse_address_book(JsonPretty, 'json'),


    % true by default
    JsonWithNullFields = addressbook_piqi_ext:gen_address_book(AddressBook, 'json', [{'json_omit_null_fields', false}]),
    io:format("~n~nJSON with null fields: ~n~n~s~n", [JsonWithNullFields]),
    AddressBook = addressbook_piqi_ext:parse_address_book(JsonWithNullFields, 'json'),


    % true by default
    JsonNoPretty = addressbook_piqi_ext:gen_address_book(AddressBook, 'json', [{'pretty_print', false}]),
    io:format("~n~nJSON w/o pretty-printing: ~n~n~s~n", [JsonNoPretty]),
    AddressBook = addressbook_piqi_ext:parse_address_book(JsonNoPretty, 'json'),


    Xml = addressbook_piqi_ext:gen_address_book(AddressBook, 'xml'),
    io:format("~n~nXML: ~n~n~s~n", [Xml]),
    AddressBook = addressbook_piqi_ext:parse_address_book(Xml, 'xml'),


    XmlPretty = addressbook_piqi_ext:gen_address_book(AddressBook, 'xml_pretty'),
    io:format("~n~nXML pretty: ~n~n~s~n", [XmlPretty]),
    AddressBook = addressbook_piqi_ext:parse_address_book(XmlPretty, 'xml'),


    XmlNoPretty = addressbook_piqi_ext:gen_address_book(AddressBook, 'xml', [{'pretty_print', false}]),
    io:format("~n~nXML w/o pretty-printing: ~n~n~s~n", [XmlNoPretty]),
    AddressBook = addressbook_piqi_ext:parse_address_book(XmlNoPretty, 'xml'),


    Piq = addressbook_piqi_ext:gen_address_book(AddressBook, 'piq'),
    io:format("~n~nPiq: ~n~n~s~n", [Piq]),
    AddressBook = addressbook_piqi_ext:parse_address_book(Piq, 'piq'),
    ok;

run(_) ->
    erlang:halt(1).
 
