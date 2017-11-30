-define(DB_PREFIX(), liquid_cms_).
-define(DESCRIPTION(Text), description() -> ??Text).

%% DB helpers
-define(TIMESTAMPS, 
        created_at :: calendar:datetime() | undefined,
        updated_at :: calendar:datetime() | undefined).

-define(V(Response), db:verify_create_table(Response)).
-define(CREATE_TABLE(Record, Type, Indexes), 
        ?V(mnesia:create_table(Record,
                               [
                                {type, Type},
                                {disc_copies, [node()]},
                                {index, Indexes},
                                {attributes, record_info(fields, Record)}
                               ]))).
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
-record(cms_settings, {
          key :: atom(),
          value :: term()
         }).

-record(cms_mfa, 
        {
         id :: {string(), string()},
         sort=1 :: non_neg_integer(),
         mfa :: {module(), atom(), [any()]} | fun(),
         ?TIMESTAMPS,
         settings :: map()
         }).

-record(cms_template, {
          file :: iodata(), % Path
          bindings=[] :: [proplists:property()],
          name="" :: string(),
          description="" :: string(),
          ?TIMESTAMPS,
          settings=#{} :: map()
         }).

-record(cms_asset, {
          id :: [string()],
          name :: binary(),
          description = <<>> :: binary(),
          file :: iodata(),
          minified=false :: boolean(),
          type :: asset_type(),
          ?TIMESTAMPS,
          settings=#{} :: map()
         }).

-record(cms_page, {
          id :: string(),
          description = <<>> :: binary(),
          module = router :: module(),
          accepted_role = nobody :: atom(),
          title = <<"LiquidCMS">> :: binary(),
          ?TIMESTAMPS,
          settings = #{} :: map()
         }).

-record(cms_user, {
          email :: string(),
          password :: binary(),
          role :: role(),
          confirm = 0 :: integer(),
          ?TIMESTAMPS,
          settings = #{} :: map()
         }).


% -record(cms_account, {
%           id :: any(),
%           user :: binary(),
%           created_at :: calendar:datetime() | undefined,
%           updated_at :: calendar:datetime() | undefined,
%           settings = #{} :: map()
%          }).
          
           
-record(cms_role, {
          role :: role(),
          name :: binary(),
          sort=1 :: integer(),
          created_at :: calendar:datetime() | undefined,
          updated_at :: calendar:datetime() | undefined,
          settings = #{} :: map()
         }).


