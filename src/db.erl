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
    mnesia:transaction(fun() ->
                               lists:foreach(fun(R) ->
                                                     mnesia:write(R)
                                             end,
                                             maps:get(cms_mfa, admin:default_data()))
                       end),
    %?CREATE_TABLE(cms_template, bag, []),
    %?CREATE_TABLE(cms_page, bag, []),
    %?CREATE_TABLE(cms_user, bag, []),
    %?CREATE_TABLE(cms_account, bag, []),
    ok.

%% Don't remove! This is is used to update your Mnesia DB backend  from CLI tool
update([]) ->
    ok.
                        
get_mfa(Page, Block) ->
    transaction(fun() ->
                        mnesia:read(cms_mfa, {Page, Block})
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
                              

