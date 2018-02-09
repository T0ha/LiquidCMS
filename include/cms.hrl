-define(DESCRIPTION(Text), description() -> ??Text).
-define(POSTBACK(P), #event{
                        type=click,
                        postback=P
                       }).
-define(POSTBACK(P, Delegate), #event{
                        type=click,
                        postback=P,
                        delegate=Delegate
                       }).

