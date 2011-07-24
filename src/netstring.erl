-module(netstring).

-export([encode/1,
         decode/1]).

%% @doc build a netstring given its content
-spec encode(Content::binary()|list()) -> iolist().
encode(Content) when is_binary(Content) ->
    Length = case is_binary(Content) of
                 true  -> size(Content);
                 false -> length(Content)
             end,
    [integer_to_list(Length), $:, Content, $,].

-spec decode(Data::binary()) -> {Parsed::binary(), Rest::binary()}.
decode(Data) ->
    [Size, ToParse] = binary:split(Data, <<$:>>),
    SizeI = list_to_integer(binary_to_list(Size)),
    <<Parsed:SizeI/binary, $,, Rest/binary>> = ToParse,
    {Parsed, Rest}.
