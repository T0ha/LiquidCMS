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
                            html=HTML,
                            class=Class,
                            hotkeys=_HotKeys
                           }) ->
    wf:wire("#wysiwyg_editor",
            #event{
               type=change,
               actions=#script{script="$('#wysiwyg_raw_html').val($('#wysiwyg_editor').html());"}
              }),
    wf:wire(#script{script="$('#wysiwyg_editor').wysiwyg();$('#wysiwyg_editor').trigger('change');"}),
    #panel{
       body=[
             buttons(Buttons),
             #checkbox{id=checkbox_html,
                       label_position='before',
                       text="Show/hide html code",
                       class="pull-left",
                       postback=display_source_code
                       },
             #textarea{style="display: none; width: 100%; height: auto;",
                       id=Id,
                       % readonly=true,
                       html_id="wysiwyg_raw_html"},
             #panel{html_id="wysiwyg_editor",
                    style="height: auto;
                           margin-top: 15px;
                           min-height: 100px;
                           max-height:60vh;
                           overflow-y:auto;
                           ",
                    body=HTML,
                    class=Class},
             preview(Preview)
            ]}.

buttons([]) ->
    [];
buttons(Buttons) ->
    #panel{class = "btn-toolbar",
           data_fields = [
                         {target, "#wysiwyg_editor"},
                         {role, "editor-toolbar"}
                        ],
           body = Buttons}.

preview(_) ->
    [].
