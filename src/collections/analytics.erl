%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (analytics).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").
-include("cms.hrl").

?DESCRIPTION(Analytics services).

%% CMS Module interface {{{1
functions() -> % {{{2
    [
     {metric_ga, "Google"},
     {metric_ya, "Yandex"},
     {metric_hs, "Hubspot"}
    ].

default_data() -> % {{{2
    #{cms_template => [
                  #cms_template{
                            file="templates/hs_analytics.html",
                            bindings=[],
                            name="Hubspot Analytics"},
                  #cms_template{
                            file="templates/ga_analytics.html",
                            bindings=[],
                            name="Google Analytics"},
                  #cms_template{
                            file="templates/ya_analytics.html",
                            bindings=[],
                            name="Yandex Analytics"}
                  ]}.

format_block(F, A) -> % {{{2
     {wf:f("~s(analytics_id:~s)", [F, A]), undefined}.

form_data(_, A) -> % {{{2
    [_, AnalyticsId] = admin:maybe_empty(A, 2),
    [
     {"AnalyticsId", {id, AnalyticsId}}
    ].

save_block(#cms_mfa{ mfa={?MODULE, Fun, [_,AnalyticsId,_,_]}}=Rec) -> % {{{2
    Rec#cms_mfa{mfa={?MODULE, Fun, [AnalyticsId]}}.

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
