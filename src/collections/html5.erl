-module(html5).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("db.hrl").
-include("cms.hrl").
-include("records.hrl").

?DESCRIPTION(Html5 elements).

%% CMS Module interface {{{1
functions() -> % {{{2
    [
     {list, "Items List (ul)"},
     {list_item, "List Item (li)"},
     {link_url, "Link (a)"},
     {block, "Block (div)"},
     {header, "Header (h{level})"},
     {paragraph, "Paragraph (p)"},
     {email_link, "Email Link"},
     {span, "Span"},
     {label, "Label"},
     {article, "Article"},
     {nav, "Nav"},
     {aside, "Aside"},
     {header_html, "Header (header)"},
     {footer_html, "Footer (footer)"},
     {mark, "Mark"},
     {main, "Main"},
     {section, "Section"},
     {time, "Time"},
     {iframe, "Iframe"}
     ].

format_block(block, [Block, Classes, DataAttr]) -> % {{{2
    {wf:f("Block (div): ~s(class=~p attr:~p)", [Block, Classes, DataAttr]), Block};
format_block(list, [Block, Numbered, Classes, DataAttr]) -> % {{{2
    {wf:f("List: ~s(numbered=~s, class=~p, attr:~p)", [Block, Numbered, Classes, DataAttr]), Block};
format_block(list_item, [Block, Classes, DataAttr]) -> % {{{2
    {wf:f("List Item: ~s(class=~p, attr:~p)", [Block, Classes, DataAttr]), Block};
format_block(link_url, [Block, URL, Classes, DataAttr]) -> % {{{2
    {wf:f("Link: ~s(href=~s, class=~p, attr:~p)", [Block, URL, Classes, DataAttr]), Block};
format_block(header, [Block, Level, Classes, DataAttr]) -> % {{{2
    {wf:f("Header: ~s(level=~s, class=~p, attr:~p)", [Block, Level, Classes, DataAttr]), Block};

format_block(article, [HeaderBlock, BodyBlock, FooterBlock, Classes, DataAttr]) -> % {{{2
    {wf:f("Article (head_block=~p, body_block=~p, footer_block=~p, classes=~p, attr:~p)",
         [HeaderBlock, BodyBlock, FooterBlock, Classes, DataAttr]), BodyBlock};

format_block(F, [Block, Data, Classes, DataAttr]) -> % {{{2
    {wf:f("html5(~s): ~s(data=~s, class=~p, attr:~p)", [F, Block, Data, Classes, DataAttr]), Block};
format_block(F, [Block, Classes, DataAttr]) -> % {{{2
  {wf:f("html5(~s): (id:~p class=~p attr:~p)", [F, Block, Classes, DataAttr]), Block};
format_block(F, A) -> % {{{2
    % ?LOG("format_block: F:~p, A:~p",[F,A]),
    {wf:f("html5: ~s(~p)", [F, A]), undefined}.


form_data(list, A) -> % {{{2
    [_, Block, Numbered, Classes, DataAttr] = admin:maybe_empty(A, 5),
    {[
       #checkbox{
          text="Numbered",
          value="true",
          label_position=before,
          id=numbered,
          checked=Numbered
         }
     ],
     [], Block, Classes, DataAttr};
form_data(link_url, A) -> % {{{2
    [_, Block, URL, Classes, DataAttr] = admin:maybe_empty(A, 5),
    {[
      {"URL",
       #txtbx{
          id=url,
          text=URL,
          placeholder="http://yoursite.com"
         }}
     ],
     [], Block, Classes, DataAttr};
form_data(iframe, A) -> % {{{2
    [_, Block, URL, Width, Height, AllowFls, Classes, DataAttr] = admin:maybe_empty(A, 8),
    ?LOG("form_data iframe ~p",[AllowFls]),
    {[
      {"URL",
       #txtbx{
          id=iframe_url,
          text=URL,
          placeholder="http://yoursite.com"
         }},
      {"Width",
       #txtbx{
          id=iframe_width,
          text=Width,
          placeholder="560"
         }},
      {"Height",
       #txtbx{
          id=iframe_height,
          text=Height,
          placeholder="400"
         }},
      #checkbox{
         text="Allow fullscreen",
         id=allowfullscreen,
         checked=AllowFls
        }
     ],
     [], Block, Classes, DataAttr};
form_data(time, A) -> % {{{2
    [_, Block, Datetime, Classes, DataAttr] = admin:maybe_empty(A, 5),
    {[
      {"Datetime (opt.)",
       #txtbx{
          id=url,
          text=Datetime,
          placeholder="2018-01-01 19:20"
         }}
     ],
     [], Block, Classes, DataAttr};
form_data(email_link, A) -> % {{{2
    [_, Block, Email, Classes, DataAttr] = admin:maybe_empty(A, 5),
    {[
      {"Email link",
       #txtbx{
          id=email_link,
          text=Email,
          placeholder="example@mail.com"
         }}
     ],
     [], Block, Classes, DataAttr};
form_data(header, A) -> % {{{2
    [_, Block, Level, Classes, DataAttr] = admin:maybe_empty(A, 5),
    {[
      {"Header level (1-5)",
       #txtbx{
          id=level,
          text=Level,
          placeholder="Input number"
         }}
     ],
     [], Block, Classes, DataAttr};
form_data(article, A) -> % {{{2
    [_, HeaderBlock, Block, FooterBlock, Classes, DataAttr] = admin:maybe_empty(A, 6),
    {[
      {"Block for header",
      #txtbx{
         id=article_header_block,
         text=HeaderBlock,
         placeholder="Block name for article header (leave blank for none)"
        }},
      {"Block for footer",
      #txtbx{
         id=article_footer_block,
         text=FooterBlock,
         placeholder="Block name for article footer (leave blank for none)"
        }}
     ],
     [],
     Block,
     Classes,
     DataAttr
    }.

save_block(#cms_mfa{mfa={html5, list, [Block, Classes, DataAttr]}}=Rec) -> % {{{2
    Num = wf:to_atom(common:q(numbered, "false")),
    Rec#cms_mfa{mfa={html5, list, [Block, Num, Classes, DataAttr]}};
save_block(#cms_mfa{mfa={html5, link_url, [Block, URL, Classes, DataAttr]}}=Rec) -> % {{{2
    Rec#cms_mfa{mfa={html5, link_url, [Block, URL, Classes, DataAttr]}};
save_block(#cms_mfa{mfa={html5, iframe, [Block, URL, Width, Height, Classes, DataAttr]}}=Rec) -> % {{{2
    AllowFls = wf:to_atom(common:q(allowfullscreen, false)),
    Rec#cms_mfa{mfa={html5, iframe, [Block, URL, Width, Height, AllowFls, Classes, DataAttr]}};
save_block(#cms_mfa{mfa={html5, email_link, [Block, URL, Classes, DataAttr]}}=Rec) -> % {{{2
    Rec#cms_mfa{mfa={html5, email_link, [Block, URL, Classes, DataAttr]}};
save_block(#cms_mfa{mfa={html5, header, [Block, Level, Classes, DataAttr]}}=Rec) -> % {{{2
  L = try
      case list_to_integer(Level) of
        Num when (Num < 6) and (Num > 0) ->
          Level;
        _ ->
          "5"
      end
    catch  _:_ -> 
      "5"
    end,
    Rec#cms_mfa{mfa={html5, header, [Block, L, Classes, DataAttr]}};
save_block(#cms_mfa{mfa={html5, article, [Block, Header, Footer, Classes, DataAttr]}}=Rec) -> % {{{2
    Rec#cms_mfa{mfa={html5,
                     article,
                     [
                      Header,
                      Block,
                      Footer,
                      Classes,
                      DataAttr
                     ]}}.

%% Block renderers {{{1
list(Page, Block, Classes) -> % {{{2
    list(Page, Block, false, Classes, []).

list(Page, Block, true, Classes) -> % {{{2
    list(Page, Block, true, Classes, []);
list(Page, Block, false, Classes) -> % {{{2
    list(Page, Block, false, Classes, []).

list(Page, Block, Numbered, Classes, DataAttr) -> % {{{2
    Items = common:parallel_block(Page, Block),
    #list{body=Items,
          numbered=Numbered,
          class=Classes,
          html_id=common:block_to_html_id(Block),
          data_fields=DataAttr
    }.

list_item(Page, ItemID) -> % {{{2
    list_item(Page, ItemID, []).

list_item(Page, ItemID, Classes) -> % {{{2
    list_item(Page, ItemID, Classes, []).
list_item(Page, ItemID, Classes, DataAttr) -> % {{{2
    #listitem{
       html_id=common:block_to_html_id(ItemID),
       body=common:parallel_block(Page, ItemID),
       class=Classes,
       data_fields = DataAttr
      }.

link_url(Page, Block, URL) -> % {{{2
    link_url(Page, Block, URL, []).
link_url(Page, Block, URL, Classes) -> % {{{2
    link_url(Page, Block, URL, Classes, []).
link_url(Page, Block, URL, Classes, DataAttr) -> % {{{2
    #link{
       url=URL, 
       html_id=common:block_to_html_id(Block),
       class=Classes,
       body=common:parallel_block(Page, Block),
       data_fields = DataAttr
      }.

link_event(Page, Block, Event) -> % {{{2
    link_event(Page, Block, Event, []).
link_event(Page, Block, Event, Classes) -> % {{{2
    #link{
       html_id=common:block_to_html_id(Block),
       actions=Event,
       class=Classes,
       body=common:parallel_block(Page, Block)
      }.

email_link(Page, Block, Email, Classes, DataAttr) -> % {{{2
  #email_link {
    email=Email,
    html_id=common:block_to_html_id(Block),
    class=Classes,
      body=common:parallel_block(Page, Block),
      data_fields = DataAttr
  }.

block(Page, Block, Classes) -> % {{{2
    block(Page, Block, Classes, []).
block(Page, Block, Classes, DataAttr) -> % {{{2
    #panel{
       html_id=common:block_to_html_id(Block),
       class=Classes,
       body=common:parallel_block(Page, Block),
       data_fields = DataAttr
      }.

header(Page, Block, Level, Classes, DataAttr) ->  % {{{2
  case Level of 
    "1" -> 
      #h1{
        html_id=common:block_to_html_id(Block),
        class=Classes,
        body=common:parallel_block(Page, Block),
            data_fields = DataAttr
      };
    "2" ->
      #h2{
        html_id=common:block_to_html_id(Block),
        class=Classes,
        body=common:parallel_block(Page, Block),
            data_fields = DataAttr
      };
    "3" ->
      #h3{
        html_id=common:block_to_html_id(Block),
        class=Classes,
        body=common:parallel_block(Page, Block),
            data_fields = DataAttr
      };
    "4" ->
      #h4{
        html_id=common:block_to_html_id(Block),
        class=Classes,
        body=common:parallel_block(Page, Block),
            data_fields = DataAttr
      };
    _ ->
      #h5{
        html_id=common:block_to_html_id(Block),
        class=Classes,
        body=common:parallel_block(Page, Block),
            data_fields = DataAttr
      }
  end.

paragraph(Page, Block, Classes, DataAttr) ->  % {{{2
  #p{
    html_id=common:block_to_html_id(Block),
    class=Classes,
    body=common:parallel_block(Page, Block),
        data_fields = DataAttr
  }.

article(Page, HeaderBlock, BodyBlock, FooterBlock, Classes, DataAttr) -> % {{{2
  #article {
    body=[
       #html5_header{ body=index:maybe_block(Page, HeaderBlock, [])},
       index:maybe_block(Page, BodyBlock, []),
       #html5_footer{ body=index:maybe_block(Page, FooterBlock, [])}
    ],
    class=[Classes],
    data_fields = DataAttr
  }.

nav(Page, Block, Classes, DataAttr) ->  % {{{2
  #nav{
    html_id=common:block_to_html_id(Block),
    class=Classes,
    body=common:parallel_block(Page, Block),
        data_fields = DataAttr
  }.

aside(Page, Block, Classes, DataAttr) ->  % {{{2
  #aside{
    html_id=common:block_to_html_id(Block),
    class=Classes,
    body=common:parallel_block(Page, Block),
        data_fields = DataAttr
  }.

header_html(Page, Block, Classes, DataAttr) ->  % {{{2
  #html5_header{
    html_id=common:block_to_html_id(Block),
    class=Classes,
    body=common:parallel_block(Page, Block),
        data_fields = DataAttr
  }.

footer_html(Page, Block, Classes, DataAttr) ->  % {{{2
  #html5_footer{
    html_id=common:block_to_html_id(Block),
    class=Classes,
    body=common:parallel_block(Page, Block),
        data_fields = DataAttr
  }.

mark(Page, Block, Classes, DataAttr) ->  % {{{2
  #mark{
    html_id=common:block_to_html_id(Block),
    class=Classes,
    body=common:parallel_block(Page, Block),
        data_fields = DataAttr
  }.

main(Page, Block, Classes, DataAttr) ->  % {{{2
  #main{
    html_id=common:block_to_html_id(Block),
    class=Classes,
    body=common:parallel_block(Page, Block),
        data_fields = DataAttr
  }.

section(Page, Block, Classes, DataAttr) ->  % {{{2
  #section{
    html_id=common:block_to_html_id(Block),
    class=Classes,
    body=common:parallel_block(Page, Block),
        data_fields = DataAttr
  }.

time(Page, Block, Datetime, Classes, DataAttr) ->  % {{{2
  #time{
    html_id=common:block_to_html_id(Block),
    class=Classes,
    datetime=Datetime,
    body=common:parallel_block(Page, Block),
        data_fields = DataAttr
  }.

span(Page, Block, Classes, DataAttr) ->  % {{{2
  #span{
    html_id=common:block_to_html_id(Block),
    class=Classes,
    body=common:parallel_block(Page, Block),
        data_fields = DataAttr
  }.

label(Page, Block, Classes, DataAttr) ->  % {{{2
  #label{
    html_id=common:block_to_html_id(Block),
    class=Classes,
    body=common:parallel_block(Page, Block),
        data_fields = DataAttr
  }.

iframe(_Page, Block, URL, Width, Height, AllowFls, Classes, DataAttr) -> % {{{2
  #iframe {
    src = URL,
    allowfullscreen = case AllowFls of 
                          on-> "true";
                          _ -> "false"
                      end,
    html_id=common:block_to_html_id(Block),
    class=Classes,
    width=Width,
    height=Height,
    data_fields = DataAttr
}.
