%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_crud).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1,
    event/1,
    inplace_textbox_event/2,
    inplace_textarea_event/2
]).

-define(POSTBACK(P), #event{
                        type=click,
                        delegate=?MODULE,
                        postback=P
                       }).

%% Move the following record definition to records.hrl:

-spec reflect() -> [atom()].
reflect() -> record_info(fields, crud).

-spec render_element(#crud{}) -> body(). % {{{1
render_element(Record = #crud{
                           id=CRUDID,
                           class=Class,
                           table_class=TableClass,
                           button_class=ButtonClass,
                           cols=Cols,
                           funs=Funs,
                           count=Count,
                           start=Start
                          }) ->
    List = maps:get(list, Funs, fun() -> [] end),

    Data = List(),
    Length = length(Data),
    Pagination = pagination(Record, Length),
    Table = table(Record, Data),
    #panel{
       id=CRUDID,
       class=Class,
       body=[
             %CreateButton,
             Table,
             Pagination
            ]
      }.

pagination(#crud{ % {{{1
              start=Start, 
              count=Count,
              pagination_class=Class
             } = Rec,
           Num) ->
    case Num div Count of
        0 -> [];
        N ->
            lists:map(
              fun(P) ->
                      Active = case P * Count of
                                   Start -> "active";
                                   _ -> ""
                               end,
                      #link{
                         class=[Active | Class],
                         text=(P + 1),
                         actions=?POSTBACK({show, Rec#crud{start=P * Count}})}
              end,
              lists:seq(0, N))
    end.

table(#crud{ % {{{1
         start=Start,
         count=Count,
         table_class=TableClass,
         cols=Cols,
         funs=Funs
        } = Rec,
      Data) when is_list(Data), 
                 is_list(Cols),
                 length(Cols) > 0 ->
    NumberedData = lists:zip(lists:seq(1, length(Data)), Data),
    #table{
       class=TableClass,
       rows=[
             #tablerow{
                cells=[
                       #tableheader{text="N"} |
                       [#tableheader{text=H} || {_, H, _} <- Cols]
                       ++ [#tableheader{text="",
                                        show_if=maps:is_key(delete, Funs)}]
                      ]} |
             [table_row(Rec, A) || {N, _}=A <- NumberedData,
                                   N > Start,
                                   N =< Start + Count]
            ]}.


table_row(#crud{ % {{{1
             cols=Cols,
             funs=Funs
            } = Rec,
           {N, Data}) ->
    #tablerow{
       cells=[
              #tablecell{text=N} |
              [#tablecell{
                  body=case Type of
                           tb ->
                               #inplace_textbox{tag={update, Rec, Data, Field},
                                                delegate=?MODULE,
                                                text=maps:get(Field, Data, " ")};
                           ta ->
                               #inplace_textarea{tag={update, Rec, Data, Field},
                                                 delegate=?MODULE,
                                                 text=maps:get(Field, Data, " ")};
                           none ->
                               #span{text=maps:get(Field, Data, " ")};
                           {select, Values} ->
                               Id = wf:temp_id(),
                               #dropdown{
                                  id=Id,
                                  value=maps:get(Field, Data),
                                  options=Values,
                                  delegate=?MODULE,
                                  postback={update, Rec, Data, Field, Id}
                                 }
                       end
                 } || {Field, _, Type} <- Cols] ++ 
              [#tablecell{
                  show_if=maps:is_key(delete, Funs),
                  body=#button{
                          text="X",
                          class=Rec#crud.button_class,
                          actions=?POSTBACK({delete, Rec, Data})
                         }}]
                          
             ]}.


inplace_textbox_event({update=Fun, Rec, Data, Field}, Value) ->
    update(Fun, Rec, Data, Field, Value);
inplace_textbox_event(Tag, Value) ->
    wf:info("~p inplace tb event ~p: ~p", [?MODULE, Tag, Value]),
    Value.

inplace_textarea_event({update=Fun, Rec, Data, Field}, Value) ->
    update(Fun, Rec, Data, Field, Value);
inplace_textarea_event(Tag, Value) ->
    wf:info("~p inplace ta event ~p: ~p", [?MODULE, Tag, Value]),
    Value.

event({update=Fun, Rec, Data, Field, ElementId}) -> % {{{1
    Val = wf:q(ElementId),
    update(Fun, Rec, Data, Field, Val),
    wf:replace(Rec#crud.id, Rec);
event({show, Rec}=E) -> % {{{1
    wf:replace(Rec#crud.id, Rec);
event({Fun, Rec, Data}=E) -> % {{{1
    io:format("Event: ~p~n", [E]),
    call(Fun, Rec, Data),
    wf:replace(Rec#crud.id, Rec);
event(Ev) -> % {{{1
    wf:warning("Event ~p in ~p", [Ev, ?MODULE]).
    
update(Fun, Rec, Data, Field, Value) -> % {{{1
    Bag = maps:get(table_type, Data, set),
    ok=case {maps:is_key(delete, Rec#crud.funs), Bag == bag} of
           {true, true} ->
               call(delete, Rec, Data);
           _ -> ok
       end,
    NewData = maps:update(Field, cast(Value, maps:get(Field, Data)), Data),
    call(Fun, Rec, NewData),
    Value.

call(Fun, #crud{funs=Funs}=Rec, Data) -> % {{{1
    F = maps:get(Fun, Funs, fun(D) -> D end),
    F(Data).

cast(Value, Old) when is_atom(Old) -> % {{{1
    wf:to_atom(Value);
cast(Value, Old) when is_list(Old) -> % {{{1
    wf:to_list(Value);
cast(Value, Old) when is_binary(Old) -> % {{{1
    wf:to_binary(Value);
cast(Value, Old) when is_integer(Old) -> % {{{1
    wf:to_integer(Value);
cast(Value, Type) -> % {{{1
    wf:warning("Can't cast ~p to ~p", [Value, Type]),
    Value.
