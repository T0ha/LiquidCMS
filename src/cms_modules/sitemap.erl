%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module(sitemap).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include("db.hrl").


%% @doc Generate sitemap.xml content
generate_sitemap() ->  % {{{2
    Ns1 = "http://www.sitemaps.org/schemas/sitemap/0.9",
    PagesAll = db:get_pages(),
    OnlyActive = fun(I) -> maps:get(active,I)==true end,
    Pages = lists:filter(OnlyActive, PagesAll),
    Proto = application:get_env(nitrogen, protocol, "http"),
    Host = application:get_env(nitrogen, host, "site.com"),
    BaseUrl = wf:f("~s://~s/", [Proto, Host]),
    RootElem = #xmlElement{name=urlset,
                           namespace=#xmlNamespace{default=Ns1},
                           attributes=[#xmlAttribute{name=xmlns, value=Ns1}],
                           content = 
                              lists:map(
                                fun(I) -> 
                                  case maps:get(sitemap,I) of
                                        none ->
                                          "";
                                        Fr ->
                                          Id = maps:get(id,I),
                                          Url = case wf:to_atom(Id) of
                                                  index -> 
                                                      BaseUrl;
                                                  _ ->
                                                      wf:f("~s?page=~s", [BaseUrl, Id])
                                                end,
                                          {url, [ #xmlElement{ name=loc,
                                                               content=[Url]
                                                  },
                                                  #xmlElement{ name=changefreq,
                                                               content=[atom_to_list(Fr)]
                                                  },
                                                  #xmlElement{ name=lastmod,
                                                               content=[db:datetime_tostr(maps:get(updated_at,I))]
                                                  }
                                                ]
                                          }
                                    end                                        
                                end, Pages
                              )
                          },
    serialize_xml([RootElem]).


%% @doc Helper function to generate XML from a data structure 
serialize_xml(Data) -> % {{{2
    Prolog = ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>"],
    Xml = lists:flatten(xmerl:export_simple(Data, xmerl_xml,[{prolog,Prolog}])),
    % ?LOG("~s~n", [Xml]),
    Xml.


%% changefreq for page
sitemap_frequencies() -> % {{{2
  [{always,"always"},
   {hourly,"hourly"},
   {daily,"daily"},
   {weekly,"weekly"},
   {monthly,"monthly"},
   {yearly,"yearly"},
   {never,"never"},
   {none,"none"}
  ].
