-module(magicbeam).

-export([all/1]).

main(Args) ->


options() -> 
    [
        {help, $h, "help", undefined, "Receive Help"},
        {node, $n, "node", string, "Remote Node."},
        {cookie, $c, "cookie", string, "Cookie to Use"}
    ].
