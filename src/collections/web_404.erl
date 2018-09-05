%% vim: ts=4 sw=4 et
-module(web_404).
-compile(export_all).
-export ([main/0]).

main() ->
	% URI = wf:uri(),
    % io:format("~n ~p:check_page ~p",[?MODULE,URI]),
    Page = db:get_page("404"),
    wf:status_code(404),
    common:template(Page,"templates/404.html").
