-module(db).
%%%
%%% Mnesia DB backend routines module.
%%% Put your backend code here
%%%

-compile([export_all]).
-include("db.hrl").

-define(V(Response), verify_create_table(Response)).
-define(CREATE_TABLE(Record, Type, Indexes), 
        ?V(mnesia:create_table(Record,
                               [
                                {type, Type},
                                {disc_copies, [node()]},
                                {index, Indexes},
                                {attributes, record_info(fields, Record)}
                               ]))).

%% Don't remove! This is is used to install your Mnesia DB backend  from CLI tool
install([])-> % {{{1
    ?CREATE_TABLE(cms_mfa, bag, []),
    ?CREATE_TABLE(cms_template, set, []),
    %?CREATE_TABLE(cms_block_template, bag, [template_id]),
    ?CREATE_TABLE(cms_asset, bag, [type, name]),
    %?CREATE_TABLE(cms_block_asset, bag, [asset_id]),
    ?CREATE_TABLE(cms_page, set, []),
    %?CREATE_TABLE(cms_user, bag, []),
    %?CREATE_TABLE(cms_account, bag, []),
    mnesia:transaction(
      fun() ->
              maps:map(
                fun(_K, V) ->
                        lists:foreach(
                          fun(R) ->
                                  mnesia:write(R)
                          end,
                          V)
                end,
                admin:default_data())
      end).

%% Don't remove! This is is used to update your Mnesia DB backend  from CLI tool
update([]) -> % {{{1
    ok.
                        
get_mfa(Page, Block) -> % {{{1
    Funs = transaction(fun() ->
                        G = mnesia:read(cms_mfa, {"*", Block}),
                        T = mnesia:read(cms_mfa, {Page, Block}),
                        lists:filter(fun(#cms_mfa{sort=S}) -> 
                                             not lists:keymember(S, #cms_mfa.sort, T)
                                             end, G) ++ T
                end),
    lists:keysort(#cms_mfa.sort, Funs).

get_template(TID) -> % {{{1
    transaction(fun() ->
                        mnesia:read(cms_template, TID)
                end).

get_template(PID, Block) -> % {{{1
    transaction(fun() ->
                        [#cms_block_template{template_id=TID}] = mnesia:read(cms_block_template, {PID, Block}),
                        mnesia:read(cms_template, TID)
                end).

get_asset(AID) -> % {{{1
    transaction(fun() ->
                        mnesia:read(cms_asset, AID)
                end).

get_assets(Type) -> % {{{1
    transaction(fun() ->
                        Assets = mnesia:select(cms_asset, [{#cms_asset{type=Type, _='_'}, [], ['$_']}]),
                        [record_to_map(A) || A <- Assets]
                end).
get_assets(Page, Block) -> % {{{1
    transaction(fun() ->
                        AIDs = mnesia:read(cms_block_asset, {Page, Block}),
                        [mnesia:read(cms_asset, AID) || 
                         #cms_block_asset{asset_id=AID} <- AIDs]
                end).

update(Record, Field, Value) -> % {{{1
    transaction(fun() ->
                        mnesia:delete_object(Record),
                        R1 = update_record_field(Record, Field, Value),
                        mnesia:write(R1),
                        Value
                end).

update_asset(Map) -> % {{{1
    save(map_to_record(Map)).


                        
get_page(PID) -> % {{{1
    transaction(fun() ->
                        mnesia:read(cms_page, PID)
                end).

save([]) -> % {{{1
    ok;
save([Record|T]) -> % {{{1
    [save(Record) | save(T)];
save(Record) -> % {{{1
    transaction(fun() ->
                        mnesia:write(Record)
                end).

delete(#{}=Map) -> % {{{1
    io:format("Delete map: ~p~n", [Map]),
    delete(map_to_record(Map));
delete(Record) -> % {{{1
    io:format("Delete: ~p~n", [Record]),
    transaction(fun() ->
                        mnesia:delete_object(Record)
                end).
%% For convenience in install and update process
verify_create_table({atomic, ok}) -> ok; % {{{1
verify_create_table({aborted, {already_exists, _Table}}) -> ok. % {{{1

% Wrapper around Mnesia transaction which returns empty list on errors
transaction(F) -> % {{{1
    case mnesia:transaction(F) of
        {atomic, R} -> R;
        _ -> []
    end.
                              
update_record_field(Record, Field, Value) -> % {{{1
    [Rec|RecList] = tuple_to_list(Record),
    Fields = mnesia:table_info(Rec, attributes),
    Map = maps:from_list(lists:zip(Fields, RecList)),
    NewMap = maps:update(Field, Value, Map),
    NewList = [maps:get(K, NewMap, undefined) || K <- Fields],
    list_to_tuple([Rec|NewList]).

record_to_map(Record) -> % {{{1
    RecList = tuple_to_list(Record),
    Fields = fields(hd(RecList)),
    RFields = [record | Fields],
    Map = maps:from_list(lists:zip(RFields, RecList)),
    Type = mnesia:table_info(maps:get(record, Map), type),
    Map#{table_type => Type}.

map_to_record(#{record := Rec}=Map) -> % {{{1
    Fields = fields(Rec),
    RFields = [record | Fields],
    NewList = [maps:get(K, Map, undefined) || K <- RFields],
    list_to_tuple(NewList).

fields(cms_asset) -> % {{{1
    record_info(fields, cms_asset);
fields(cms_page) -> % {{{1
    record_info(fields, cms_page);
fields(cms_mfa) -> % {{{1
    record_info(fields, cms_mfa);
fields(cms_template) -> % {{{1
    record_info(fields, cms_template).
