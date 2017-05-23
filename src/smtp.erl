-module(smtp).
-compile([export_all]).

-define(HOST, "https://liquid-nitrogen.org").

send_confirm(Email, Verify) ->  % {{{1
    Host = ?HOST,
    Link = Host ++ "/account/verify?email=" ++ Email ++ "\\&verification=" ++ wf:url_encode(Verify),
    {ok, Body} = file:read_file("templates/confirm.tpl"),
    Body1 = re:replace(Body, "\\$LINK\\$", Link, [{return, list}, global]),
    Body2 = re:replace(Body1, "\\$VERIFY\\$", Verify, [{return, list}, global]),
    Body3 = re:replace(Body2, "\\$EMAIL\\$", Email, [{return, list}, global]),
    send_html("veify@liquid-nitrogen.org", Email, "Confirm account registration on liquid-nitrogen.org", Body3).

send_forgot(Email, Passwd) ->  % {{{1
    {ok, Body} = file:read_file("templates/forgot.tpl"),
    Body1 = re:replace(Body, "\\$PASSWD\\$", Passwd, [{return, list}, global]),
    Body2 = re:replace(Body1, "\\$HOST\\$", ?HOST, [{return, list}, global]),
    Body3 = re:replace(Body2, "\\$EMAIL\\$", Email, [{return, list}, global]),
    send_html("no-reply@liquid-nitrogen.org", Email, "Password changed foraccount on liquid-nitrogen.org", Body3).

send_html(From, To, Subject, Message) ->  % {{{1
    Sendmail = open_port({spawn, "/usr/sbin/sendmail -t"}, []),
    MessageWithHeader = "From: " ++ From ++ "\n" ++
	"To: " ++ To ++ "\n" ++
	"MIME-Version: 1.0\n" ++
	"Content-Type: text/html; charset=utf8\n" ++
	"Subject: " ++ Subject ++ "\n\n" ++
	Message ++ "\n\n", 
    Sendmail ! {self(), {command, MessageWithHeader}},
    Sendmail ! {self(), close}.
