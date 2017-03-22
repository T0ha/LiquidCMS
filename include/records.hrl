%% Include the automatically generated plugins directory
-include("plugins.hrl").

%% Include any application-specific custom elements, actions, or validators below
-record(crud, {?ELEMENT_BASE(element_crud),
               table_class=[],
               button_class=[],
               pagination_class=[],
               cols=[] ::  [{atom(), string(), tb | ta | {select, [{string(), string()}]}}],
               funs=#{},
               start=0,
               count=10
    }).

-record(wysiwyg_button, {?ELEMENT_BASE(element_wysiwyg_button),
        func :: any(),
        body :: any()
    }).

-record(wysiwyg, {?ELEMENT_BASE(element_wysiwyg),
        buttons = [] :: [#wysiwyg_button{}],
        preview = false:: boolean(),
        hotkeys = #{} :: map()
    }).

