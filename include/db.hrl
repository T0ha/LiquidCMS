-define(DB_PREFIX(), liquid_cms_).
-define(POSTBACK(P), #event{
                        type=click,
                        postback=P
                       }).
-define(POSTBACK(P, Delegate), #event{
                        type=click,
                        postback=P,
                        delegate=Delegate
                       }).

-define(ROLE_ROOT_SORT, 1).
-define(ROLE_ADMIN_SORT, 1000).
-define(ROLE_EDITOR_SORT, 2000).
-define(ROLE_NOBODY_SORT, 10000).

-type role() :: 'admin' | 'editor' | 'nobody' | atom().
-type asset_type() :: 'image' | 'script' | 'css' | 'less' | binary.

%% DB tables records goes here
-record(cms_mfa, {
          id :: {string(), string()},
          sort :: non_neg_integer(),
          mfa :: {module(), fun(), [any()]} | fun(),
          settings :: map()
         }).

-record(cms_template, {
          file :: iodata(), % Path
          bindings=[] :: [proplists:property()],
          name="" :: string(),
          description="" :: string(),
          settings=#{} :: map()
         }).

-record(cms_asset, {
          id :: [string()],
          name :: binary(),
          description = <<>> :: binary(),
          file :: iodata(),
          minified=false :: boolean(),
          type :: asset_type(),
          settings=#{} :: map()
         }).

-record(cms_page, {
          id :: string(),
          description = <<>> :: binary(),
          module = router :: module(),
          accepted_role = nobody :: atom(),
          title = <<"LiquidCMS">> :: binary(),
          settings = #{}
         }).

-record(cms_user, {
          email :: string(),
          password :: binary(),
          role :: role(),
          settings = #{} :: map()
         }).


-record(cms_account, {
          id :: any(),
          user :: binary(),
          settings = #{} :: map()
         }).
          
           
-record(cms_role, {
          role :: role(),
          name :: binary(),
          sort=1 :: integer(),
          settings = #{} :: map()
         }).


