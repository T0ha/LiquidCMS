%% vim: ts=4 sw=4 et
-module(web_404).
-export ([main/0]).

main() ->
	URI = wf:uri(),
    % io:format("~n ~p:check_page ~p",[?MODULE,URI]),
    wf:redirect('/?page=404').
