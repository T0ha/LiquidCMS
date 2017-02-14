%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_crud).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
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
    TableID = wf:temp_id(),
    List = maps:get(list, Funs, fun() -> [] end),

    Data = List(),
    Length = length(Data),
    Pagination = pagination(Funs, Length, Start, Count, ButtonClass),
    Table = table(Data, Start, Count, TableClass, Cols, TableID, Funs),
    #panel{
       id=CRUDID,
       class=Class,
       body=[
             %CreateButton,
             Table,
             Pagination
            ]
      }.

pagination(Funs, Num, Start, Count, Class) -> % {{{1
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
                         text=P,
                         actions=?POSTBACK({show, Funs, Start, Count})}
              end,
              lists:seq(1, N + 1))
    end.

table(Data, Start, Count, TableClass, Cols, TableID, Funs) when is_list(Data), % {{{1
                                                                is_list(Cols),
                                                                length(Cols) > 0 ->
    NumberedData = lists:zip(lists:seq(1, length(Data)), Data),
    #table{id=TableID,
           class=TableClass,
           rows=[
                 #tablerow{
                    cells=[
                           #tableheader{text="N"} |
                           [#tableheader{text=H} || {_, H, _} <- Cols]
                          ]} |
                 [table_row(A, Cols, Funs) || {N, _}=A <- NumberedData,
                                              N >= Start,
                                              N < Start + Count]
                ]}.


table_row({N, Data}, Cols, Funs) -> % {{{1
    Delete = maps:get(delete, Funs, undefined),
    #tablerow{
       cells=[
              #tablecell{text=N} |
              [#tablecell{
                  body=case Type of
                           tb ->
                               #inplace_textbox{tag={update, Data, Field},
                                                text=maps:get(Field, Data, " ")};
                           ta ->
                               #inplace_textarea{tag={update, Data, Field},
                                                 text=maps:get(Field, Data, " ")};
                           none ->
                               #span{text=maps:get(Field, Data, " ")};
                           {select, Values} ->
                               Id = wf:temp_id(),
                               #dropdown{
                                  id=Id,
                                  value=maps:get(Field, Data),
                                  options=Values,
                                  postback=?POSTBACK({update, Data, Field, Id})
                                 }
                       end
                 } || {Field, _, Type} <- Cols] ++ 
              [#tablecell{
                  show_if=(Delete /= undefined),
                  body=#button{
                          text="X",
                          postback=?POSTBACK({Delete, Data})
                         }}]
                          
             ]}.
