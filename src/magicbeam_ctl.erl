-module(thunderbeam_ctl).

-export([inject/1, enable/1, disable/2, remove/1]).

inject([Node]) ->
    case thunderbeam_util:inject_app(list_to_atom(Node)) of
        ok -> success("injected thunderbeam into " ++ Node);
        _ -> problems("injecting thunderbeam into " ++ Node)
    end.

enable([Node]) ->
    case thunderbeam_util:rpc(list_to_atom(Node), thunderbeam, enable, []) of
        ok -> success("enabled thunderbeam on " ++ Node);
        _ -> problems("unable to enable thunderbeam on " ++ Node)
    end.
