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
    BaseUrl = application:get_env(nitrogen, host, "http://site.com") ++ "/",
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
		                                  	{url, [#xmlElement{ name=loc,
		                                              content=[BaseUrl++maps:get(id,I)]
		                                            },
		                                         #xmlElement{ name=changefreq,
		                                                    content=[atom_to_list(Fr)]
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