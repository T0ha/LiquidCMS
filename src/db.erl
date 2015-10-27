-module(db).
-compile([export_all]).
-include("db.hrl").

-define(V(Response), verify_create_table(Response)).

install([])->
    ok.

update([]) ->
    ok.
                        

verify_create_table({atomic, ok}) -> ok;
verify_create_table({aborted, {already_exists, _Table}}) -> ok.

next_id(Table) ->
    transaction(fun() ->
                        try 
                            mnesia:last(Table) + 1
                        catch
                            _:_ ->
                                1      
                        end
                end).

remove(Type, Id) -> 
    transaction(fun() ->
                         mnesia:delete(Type, Id, write)
                end).
write(Rec) ->
    transaction(fun() ->
                        mnesia:write(Rec)
                end).

transaction(F) ->
    case mnesia:transaction(F) of
        {atomic, R} -> R;
        _ -> []
    end.
                              

