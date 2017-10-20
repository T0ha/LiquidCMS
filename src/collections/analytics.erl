%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (analytics).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

?DESCRIPTION(Analytics services).

default_data() -> % {{{2
    #{cms_template => [
                  #cms_template{
                            file="templates/hs_analytics.html",
                            bindings=[],
                            name="templates/hs_analytics.html"},
                  #cms_template{
                            file="templates/ga_analytics.html",
                            bindings=[],
                            name="templates/ga_analytics.html"},
                  #cms_template{
                            file="templates/ya_analytics.html",
                            bindings=[],
                            name="templates/ya_analytics.html"}
                  ]}.

%% CMS Module interface {{{1
functions() -> % {{{2
    [
     {metric_ga, "Google"},
     {metric_ya, "Yandex"},
     {metric_hs, "Hubspot"}
     ].

format_block(F, [Block|_]=A) -> % {{{2
    {wf:f("~p:~s(analytics_id=~p)", [?MODULE, F, A]), Block}.

form_data(_, A) -> % {{{2
    [_, AnalyticsId] = admin:maybe_empty(A, 2),
    [
     {"AnalyticsId", {id, AnalyticsId}}
    ].

save_block(#cms_mfa{ mfa={?MODULE, Fun, [_Block, AnalyticsId, _Classes]}}=Rec) -> % {{{2
    Rec#cms_mfa{mfa={?MODULE, Fun, [AnalyticsId]}}.

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
