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
                           table_class=_TableClass,
                           button_class=_ButtonClass,
                           cols=_Cols,
                           funs=Funs,
                           count=_Count,
                           start=_Start
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
                               OldValue = maps:get(Field, Data, " "),
                               #inplace_textbox{tag={update, Rec, Data, Field,OldValue},
                                                delegate=?MODULE,
                                                text=OldValue};
                           ta ->
                               #inplace_textarea{tag={update, Rec, Data, Field},
                                                 delegate=?MODULE,
                                                 text=maps:get(Field, Data, " ")};
                           none ->
                               _V = 
                               #span{text=wf:f("~p", [maps:get(Field, Data, " ")])};
                           {select, Values} ->
                               Id = wf:temp_id(),
                               #dropdown{
                                  id=Id,
                                  value=maps:get(Field, Data),
                                  options=Values,
                                  delegate=?MODULE,
                                  postback={update, Rec, Data, Field, Id, undefined}
                                 };
                           button ->
                               _V = #button{
                                        text="Copy page",
                                        class=Rec#crud.button_class,
                                        body=#span{
                                        class="glyphicon glyphicon-copy"},
                                        actions=?POSTBACK({copy_page, Rec, Data})
                                       }
                       end
                 } || {Field, _, Type} <- Cols] ++
              [#tablecell{
                  show_if=maps:is_key(delete, Funs),
                  body=#button{
                          text="×",
                          class=Rec#crud.button_class++" close ",
                          title="Delete",
                          actions=?POSTBACK({delete, Rec, Data})
                         }}]                           
             ]}.


inplace_textbox_event({update=Fun, Rec, Data, Field, OldValue}, Value) -> % {{{1
  % ?LOG("inplace1 tb event ~p:  ", [Value]),
    update(Fun, Rec, Data, Field, unicode:characters_to_binary(Value), OldValue);
inplace_textbox_event(_Tag, Value) -> % {{{1
    % ?LOG("~p inplace2 tb event ~p: ~p", [?MODULE, Tag, Value]),
    Value.

inplace_textarea_event({update=Fun, Rec, Data, Field}, Value) -> % {{{1
     % ?LOG("inplace textarea event ~p:", [ Value]),
    update(Fun, Rec, Data, Field, Value, undefined);
inplace_textarea_event(_Tag, Value) -> % {{{1
    % ?LOG("~p inplace ta event ~p: ~p", [?MODULE, Tag, Value]),
    Value.

event({update=Fun, Rec, Data, Field, ElementId, OldValue}) -> % {{{1
    Val = wf:q(ElementId),
    % ?LOG("update event ~p: value:~p", [ ElementId, Val]),
    update(Fun, Rec, Data, Field, Val, OldValue),
    wf:replace(Rec#crud.id, Rec);
event({show, Rec}=_E) -> % {{{1
    wf:replace(Rec#crud.id, Rec);
event({Fun, Rec, Data}=_E) -> % {{{1
    % io:format("Event: ~p~n", [E]),
    call(Fun, Rec, Data),
    wf:replace(Rec#crud.id, Rec);
event(Ev) -> % {{{1
    wf:warning("Event ~p in ~p", [Ev, ?MODULE]).

update(Fun, Rec, Data, Field, Value, OldValue) -> % {{{1
    % ?LOG("~nupdate(crud): ~p", [Value]),
    Bag = maps:get(table_type, Data, set),
    ok=case {maps:is_key(delete, Rec#crud.funs), Bag == bag} of
           {true, true} ->
               call(delete, Rec, Data);
           _ -> ok
       end,
    % ?LOG("~nupdate(OldValue): ~p", [OldValue]),
    Data1 = maps:update(Field, cast(Value, maps:get(Field, Data)), Data),
    NewData = maps:put(old_value, OldValue, Data1),
    % ?LOG("~nupdate(NewData): ~p", [NewData]),
    call(Fun, Rec, NewData),
    wf:defer(#event{postback={show, Rec}, delegate=?MODULE} ),
    Value.

call(Fun, #crud{funs=Funs}=_Rec, Data) -> % {{{1
    % ?LOG("~nwill call: ~p", [Fun]),
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
