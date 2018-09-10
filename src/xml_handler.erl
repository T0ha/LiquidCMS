-module(xml_handler).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").


handle(EntryFun) -> % {{{2
  %% Return sitemap.xml

  wf:content_type("text/xml"),
  EntryFun().

