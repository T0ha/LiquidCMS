%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_wysiwyg).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).


-spec reflect() -> [atom()].
reflect() -> record_info(fields, wysiwyg).

-spec render_element(#wysiwyg{}) -> body().
render_element(_Record = #wysiwyg{
                            id=Id,
                            buttons=Buttons,
                            preview=Preview,
                            class=Class,
                            hotkeys=HotKeys
                           }) ->
    wf:wire(#script{script="$('#wysiwyg_editor').wysiwyg();"}),
    wf:wire("#wysiwyg_editor",
            #event{
               type=keyup,
               actions=#script{script="$('#wysiwyg_raw_html').val($('#wysiwyg_editor').html());"}
              }),
    #panel{
       body=[
             buttons(Buttons),
             #textarea{style="display: none;",
                       id=Id,
                       html_id="wysiwyg_raw_html"},
             #panel{html_id="wysiwyg_editor",
                    class=Class},
             preview(Preview)
            ]}.

buttons([]) ->
    [];
buttons(Buttons) ->
    #panel{class = "btn-toolbar",
           body = Buttons}.

preview(_) ->
    [].
