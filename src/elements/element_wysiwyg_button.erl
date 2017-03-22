%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_wysiwyg_button).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, wysiwyg_button).

-spec render_element(#wysiwyg_button{}) -> body().
render_element(_Record = #wysiwyg_button{
                            func = Func,
                            body = Body,
                            class = Class
                           }) ->
    #link{data_fields = [
                         {edit, func(Func)}
                        ],
          class = ["btn-toolbar" | Class],
          body = Body}.

func({F, []}) ->
    wf:f("~s", [F]);
func({F, A}) ->
    wf:f("~s ~s", [F, A]);
func(S) when is_list(S); is_binary(S) ->
    S.
