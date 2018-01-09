%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (cms_collection_default).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

?DESCRIPTION(CMS Collection Template).

%% CMS Module interface {{{1
functions() -> % {{{2
    [
     {block, "Block (div)"}
     ].

format_block(F, [Block|_]=A) -> % {{{2
    {wf:f("account:~s(~p)", [F, A]), Block}.

form_data(_F, [_, Block, Classes]) -> % {{{2
    {[], [], Block, Classes};
form_data(_F, []) -> % {{{2
    {[], []}.

% save_block(#cms_mfa{id={_PID, _}, mfa={_M, Fun, [Block, Classes,_DataAttr]}}=Rec) -> % {{{2
%     ?LOG("~nsave_block1 ~p", [Rec]),
%     Rec#cms_mfa{mfa={?MODULE, Fun, [Block, Classes]}};
save_block(#cms_mfa{id={_PID, _}, mfa={_M, Fun, [Block, Classes]}}=Rec) -> % {{{2
    Rec#cms_mfa{mfa={?MODULE, Fun, [Block, Classes]}}.

%% Block renderers {{{1
block(Page, Block, Classes) -> % {{{2
    #panel{
       html_id=common:block_to_html_id(Block),
       class=Classes,
       body=common:parallel_block(Page, Block)
      }.
%% Event handlers % {{{1
event(Ev) -> % {{{2
    ?LOG("~p event ~p", [?MODULE, Ev]).

%% Helpers {{{1

%% Dropdown formatters {{{1
