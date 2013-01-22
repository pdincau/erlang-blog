-module(ssl_client).

-export([start/0, loop/3, test/1]).

-define(SSLOPTIONS, [{certfile, "/path/to/certificate.pem"},
                     {keyfile, "/path/to/test/key.pem"},
                     {password, "password"},
                     {mode, binary},
                     {packet, 0},
                     {active, false}]).

-define(MAX_CONN, 300).
 

start() ->
    ssl:start(),
    Pid = spawn(?MODULE, loop, [0, 0, now()]),
    [spawn(?MODULE, test, [Pid]) || _ <- lists:seq(1, ?MAX_CONN)],
    ok.

loop(Success, Error, StartTime) when Success + Error == ?MAX_CONN ->
    TimeDelta = timer:now_diff(now(), StartTime),
    io:format("Time: ~p~n", [TimeDelta]),
    io:format("Successful connections: ~p~n", [Success]),
    io:format("Error connections: ~p~n", [Error]);

loop(Success, Error, StartTime) ->
    receive 
	success ->
	    loop(Success + 1, Error, StartTime);
	error ->
	    loop(Success, Error + 1, StartTime)
    end.

test(Pid) -> 
    case ssl:connect("localhost", 5555, ?SSLOPTIONS, 4000) of
	{ok, _Socket} ->
	    Pid ! success;
	{error, _} ->
	    Pid ! error
    end.
    
