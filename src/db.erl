-module(db).
%%%
%%% Mnesia DB backend routines module.
%%% Put your backend code here
%%%

-compile([export_all]).
-include("db.hrl").

-define(V(Response), verify_create_table(Response)).

%% Don't remove! This is is used to install your Mnesia DB backend  from CLI tool
install([])->
    ok.

%% Don't remove! This is is used to update your Mnesia DB backend  from CLI tool
update([]) ->
    ok.
                        
%% For convenience in install and update process
verify_create_table({atomic, ok}) -> ok;
verify_create_table({aborted, {already_exists, _Table}}) -> ok.

% Wrapper around Mnesia transaction which returns empty list on errors
transaction(F) ->
    case mnesia:transaction(F) of
        {atomic, R} -> R;
        _ -> []
    end.
                              

