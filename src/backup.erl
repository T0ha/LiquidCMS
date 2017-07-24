%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (backup).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

%% TODO: Move to cms_module with role check
main() -> % {{{1
    Path = wf:q(path),
    wf:content_type("octet/binary"),
    wf:download_as("backup.lcms"),
    {ok, Data} = file:read_file(Path),
    Data.

