-module(db).
%%%
%%% Mnesia DB backend routines module.
%%% Put your backend code here
%%%

-compile([export_all]).
-include("db.hrl").


%% Don't remove! This is is used to install your Mnesia DB backend  from CLI tool
install([])-> % {{{1
    ?CREATE_TABLE(cms_mfa, bag, []),
    ?CREATE_TABLE(cms_template, set, []),
    ?CREATE_TABLE(cms_asset, bag, [type, name]),
    ?CREATE_TABLE(cms_page, set, []),
    ?CREATE_TABLE(cms_user, bag, []),
    ?CREATE_TABLE(cms_role, set, []),
    DataModules = common:module_by_function({default_data, 0}),
    mnesia:transaction(
      fun() ->
              [maps:map(
                 fun(_K, V) ->
                         lists:foreach(
                           fun(R) ->
                                   mnesia:write(R)
                           end,
                           V)
                 end,
                (M):default_data()) ||
               M <- DataModules]
      end).

%% Don't remove! This is is used to update your Mnesia DB backend  from CLI tool
update([]) -> % {{{1
    transaction(fun() ->
                        A = mnesia:match_object(#cms_mfa{mfa={index, maybe_redirect_to_login, '_'}, _ = '_'}),
                        B = mnesia:match_object(#cms_mfa{mfa={index, maybe_change_module, '_'}, _ = '_'}),
                        [mnesia:delete_object(O) || O <- A ++ B]
                end).
                        
%% Getters
login(Email, Password) -> % {{{1
    transaction(fun() ->
                        mnesia:match_object(#cms_user{email=Email,
                                                      password=Password,
                                                      _='_'})
                end).

register(Email, Password, Role) -> % {{{1
    transaction(fun() ->
                        case mnesia:match_object(#cms_user{email=Email,
                                                           _='_'}) of
                            [] -> 
                                User = #cms_user{email=Email,
                                                 password=Password,
                                                 role=Role},
                                mnesia:write(User),
                                User;
                            _ -> {error, "User already exist"}
                        end
                end).
get_mfa(Page, Block) -> % {{{1
    get_mfa(Page, Block, false).
get_mfa(Page, Block, Replaced) -> % {{{1
    Funs = transaction(fun() ->
                        G = mnesia:read(cms_mfa, {"*", Block}),
                        T = mnesia:read(cms_mfa, {Page, Block}),
                        lists:filter(fun(#cms_mfa{sort=S}) -> 
                                             not lists:keymember(S, #cms_mfa.sort, T)
                                             end, G) ++ 
                        case Replaced of
                            true ->
                                T;
                            _ ->
                                [TE || #cms_mfa{mfa=MFA}=TE <- T, MFA /= undefined]
                        end
                            
                end),
    lists:keysort(#cms_mfa.sort, Funs).
get_all_blocks(PID) -> % {{{1
    Blocks = transaction(fun() ->
                                 mnesia:select(cms_mfa, [{#cms_mfa{id={'$1', '$2'}, _='_'}, [{'or', {'==', '$1', PID}, {'==', '$1', "*"}}], ['$2']}])
                         end),
    sets:to_list(sets:from_list(Blocks)).
                                 
get_users() -> % {{{1
    transaction(fun() ->
                        Users = mnesia:match_object(#cms_user{_='_'}),
                        [record_to_map(A) || A <- Users]
                end).

get_roles() -> % {{{1
    transaction(fun() ->
                        Role = mnesia:match_object(#cms_role{_='_'}),
                        [record_to_map(A) || A <- Role]
                end).
get_templates() -> % {{{1
    transaction(fun() ->
                        Templates = mnesia:match_object(#cms_template{_='_'}),
                        [record_to_map(A) || A <- Templates]
                end).

get_template(TID) -> % {{{1
    transaction(fun() ->
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

fix_sort(#cms_mfa{id={PID, Block}}=Rec) -> % {{{1
    Sort = case get_mfa(PID, Block) of
               [] -> 1;
               MFAs ->
                   #cms_mfa{sort=S} = lists:last(MFAs),
                   S
           end,
    Rec#cms_mfa{sort=Sort+1}.

update(OldRecord, NewRecord) -> % {{{1
    transaction(fun() ->
                        mnesia:delete_object(OldRecord),
                        mnesia:write(NewRecord)
                end).

update(Record, Field, Value) -> % {{{1
    transaction(fun() ->
                        mnesia:delete_object(Record),
                        R1 = update_record_field(Record, Field, Value),
                        mnesia:write(R1),
                        Value
                end).

update_map(Map) -> % {{{1
    save(map_to_record(Map)).


                        
get_pages() -> % {{{1
    transaction(fun() ->
                        Pages = mnesia:match_object(#cms_page{_='_'}),
                         [record_to_map(A) || A <- Pages]
                end).

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
                        mnesia:write(Record),
                        Record
                end).

maybe_delete(#cms_mfa{id={PID, Block}, sort=Sort}=B) -> % {{{1
    transaction(fun() ->
                        case mnesia:wread({cms_mfa, {PID, Block}}) of
                            [] ->
                                mnesia:write(empty_mfa(PID, Block, Sort));
                            L when is_list(L) -> 
                                
                                [mnesia:delete_object(B1) || B1 <- L, B1#cms_mfa.sort == Sort]
                        end
                end).
maybe_update(#cms_mfa{id={PID, Block}, sort=Sort}=B) -> % {{{1
    transaction(fun() ->
                        case mnesia:wread({cms_mfa, {PID, Block}}) of
                            [] ->
                                ok;
                            L when is_list(L) -> 
                                
                                [mnesia:delete_object(B1) || B1 <- L, B1#cms_mfa.sort == Sort]
                        end,
                        mnesia:write(B)
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
fields(cms_user) -> % {{{1
    record_info(fields, cms_user);
fields(cms_role) -> % {{{1
    record_info(fields, cms_role);
fields(cms_template) -> % {{{1
    record_info(fields, cms_template).

empty_mfa(PID, Block, Sort) -> % {{{1
    #cms_mfa{
       id={PID, Block},
       sort=Sort,
       mfa=udefined}.


