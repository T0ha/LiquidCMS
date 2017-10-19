%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (analytics).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

?DESCRIPTION(Analytics services).

%% CMS Module interface {{{1
functions() -> % {{{2
    [
     {metric_ga, "Google"},
     {metric_ya, "Yandex"},
     {metric_hs, "Hubspot"}
     ].

format_block(F, [Block|_]=A) -> % {{{2
    {wf:f("account:~s(~p)", [F, A]), Block}.

% form_data(F, [_, Block, Classes]) -> % {{{2
%     {[], [], Block, Classes};
% form_data(F, []) -> % {{{2
%     {[], []}.

form_data(F, A) -> % {{{2
    [_, AnalyticsId] = admin:maybe_empty(A, 2),
    [
     {"AnalyticsId", {id, AnalyticsId}}
    ].

save_block(#cms_mfa{id={PID, _}, mfa={M, Fun, [Block, Classes]}}=Rec) -> % {{{2
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

metric_hs(Page, AnalyticsId) -> % {{{2
    common:template(Page,
      "templates/hs_analytics.html",
      [ {'AnalyticsId', AnalyticsId} ]
    ).

metric_ya(Page, AnalyticsId) -> % {{{2
    common:template(Page,
      "templates/ya_analytics.html",
      [ {'AnalyticsId', AnalyticsId} ]
    ).

metric_ga(Page, AnalyticsId) -> % {{{2
    common:template(Page,
      "templates/ga_analytics.html",
      [ {'AnalyticsId', AnalyticsId} ]
    ).
%% Helpers {{{1

%% Dropdown formatters {{{1
