%% Include the automatically generated plugins directory
-include("plugins.hrl").

%% Include any application-specific custom elements, actions, or validators below
-record(crud, {?ELEMENT_BASE(element_crud),
               table_class=[],
               button_class=[],
               cols=[] ::  [{atom(), string(), tb | ta | {select, [{string(), string()}]}}],
               funs=#{},
               start=0,
               count=10
    }).
