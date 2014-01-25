-module(worker).
-export([start_computing/1]).

start_computing(Board) ->
	%io:format("Hello, I'm ~s PID: ~p!~n",[node(),self()]),
	receive
		{Supervisor, start, first, N, Down} -> 
			Supervisor ! {self(), started},
			run(first, N, {Down}, Board);
		{Supervisor, start, middle, N, Up, Down} -> 
			Supervisor ! {self(), started},
			run(middle, N, {Up, Down}, Board);
		{Supervisor, start, last, N, Up} -> 
			Supervisor ! {self(), started},
			run(last, N, {Up}, Board)
	end.
