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
     % {img, "Image"},
     {list, "Items List"},
     {list_item, "List Item"},
     {link_url, "Link"},
     {block, "Block (div)"}
     ].

format_block(block, [Block, Classes, DataAttr]) -> % {{{2
    {wf:f("Block (div): ~s(class=~p) (attr:~p)", [Block, Classes, DataAttr]), Block};
format_block(list, [Block, Numbered, Classes, DataAttr]) -> % {{{2
    {wf:f("List: ~s(numbered=~s, class=~p, attr:~p)", [Block, Numbered, Classes, DataAttr]), Block};
format_block(list_item, [Block, Classes, DataAttr]) -> % {{{2
    {wf:f("List Item: ~s(class=~p, attr:~p)", [Block, Classes, DataAttr]), Block};
format_block(link_url, [Block, URL, Classes, DataAttr]) -> % {{{2
    {wf:f("Link: ~s(href=~s, class=~p, attr:~p)", [Block, URL, Classes, DataAttr]), Block};
format_block(F, A) -> % {{{2
    % ?LOG("format_block: F:~p, A:~p",[F,A]),
    {wf:f("html5:~s(~p)", [F, A]), undefined}.


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
     [], Block, Classes, DataAttr}.

save_block(#cms_mfa{mfa={common, list, [Block, Classes, DataAttr]}}=Rec) -> % {{{2
    Num = wf:to_atom(common:q(numbered, "false")),
    Rec#cms_mfa{mfa={common, list, [Block, Num, Classes, DataAttr]}};
save_block(#cms_mfa{mfa={common, link_url, [Block, URL, Classes, DataAttr]}}=Rec) -> % {{{2
    Rec#cms_mfa{mfa={common, link_url, [Block, URL, Classes, DataAttr]}}.

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

block(Page, Block, Classes) -> % {{{2
    block(Page, Block, Classes, []).
block(Page, Block, Classes, DataAttr) -> % {{{2
    #panel{
       html_id=common:block_to_html_id(Block),
       class=Classes,
       body=common:parallel_block(Page, Block),
       data_fields = DataAttr
      }.
