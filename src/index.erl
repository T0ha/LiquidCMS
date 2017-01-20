%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("db.hrl").

main() -> #template { file="./templates/blank.html" }.

parallel_block(Page, Block) ->
    Functions = db:get_mfa(Page, Block),
    [ apply(M, F, A) || #cms_mfa{mfa={M, F, A}} <- Functions].

css(Path) ->
  "<link href='" ++ wf:to_list(Path) ++ "' rel='stylesheet'>".
    
script(Path) ->
    "<script src='" ++ wf:to_list(Path) ++ "' type='text/javascript' charset='utf-8'></script>".

scripts() -> 
    [parallel_block("admin", "css"),
    parallel_block("admin", "script")].
    %#template { file="./templates/scripts.html" }.

title() -> "LiquidCMS".

body() ->
    #container_12 { body=[
        #grid_8 { alpha=true, prefix=2, suffix=2, omega=true, body=inner_body() }
    ]}.

inner_body() -> 
    [
        #h1 { text="Welcome to Nitrogen" },
        #p{},
        "
        If you can see this page, then your Nitrogen server is up and
        running. Click the button below to test postbacks.
        ",
        #p{}, 	
        #button { id=button, text="Click me!", postback=click },
		#p{},
        "
        Run <b>./bin/dev help</b> to see some useful developer commands.
        ",
		#p{},
		"
		<b>Want to see the ",#link{text="Sample Nitrogen jQuery Mobile Page",url="/mobile"},"?</b>
		"
    ].
	
event(click) ->
    wf:replace(button, #panel { 
        body="You clicked the button!", 
        actions=#effect { effect=highlight }
    }).
