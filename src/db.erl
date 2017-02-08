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
install([])->
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
update([]) ->
    ok.
                        
get_mfa(Page, Block) ->
    Funs = transaction(fun() ->
                        G = mnesia:read(cms_mfa, {"*", Block}),
                        T = mnesia:read(cms_mfa, {Page, Block}),
                        lists:filter(fun(#cms_mfa{sort=S}) -> 
                                             not lists:keymember(S, #cms_mfa.sort, T)
                                             end, G) ++ T
                end),
    lists:keysort(#cms_mfa.sort, Funs).

get_template(TID) ->
    transaction(fun() ->
                        mnesia:read(cms_template, TID)
                end).

get_template(PID, Block) ->
    transaction(fun() ->
                        [#cms_block_template{template_id=TID}] = mnesia:read(cms_block_template, {PID, Block}),
                        mnesia:read(cms_template, TID)
                end).

get_assets(Page, Block) ->
    transaction(fun() ->
                        AIDs = mnesia:read(cms_block_asset, {Page, Block}),
                        [mnesia:read(cms_asset, AID) || 
                         #cms_block_asset{asset_id=AID} <- AIDs]
                end).
get_page(PID) ->
    transaction(fun() ->
                        mnesia:read(cms_page, PID)
                end).

save([]) ->
    ok;
save([Record|T]) ->
    [save(Record) | save(T)];
save(Record) ->
    transaction(fun() ->
                        mnesia:write(Record)
                end).
%% For convenience in install and update process
verify_create_table({atomic, ok}) -> ok;
verify_create_table({aborted, {already_exists, _Table}}) -> ok.

% Wrapper around Mnesia transaction which returns empty list on errors
transaction(F) ->
    case mnesia:transaction(F) of
        {atomic, R} -> R;
        _ -> []
    end.
                              

