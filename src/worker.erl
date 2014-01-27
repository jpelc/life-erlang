-module(worker).
-export([start_computing/1]).

% funkcja inicjalizacyjna
start_computing(Board) ->
	io:format("Hello, I'm ~s PID: ~p!~n",[node(),self()]),
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



% funkcja czekająca na polecenia od supervisora (next lub stop)
run(first, N, {Down}, Board) ->
	Result = receive
		{Supervisor, next} -> 
			NewBoard = next(first, {Down}, Board),
			Supervisor ! {self(), done},
			NewBoard;
		{Supervisor, stop} -> Supervisor ! {self(), finish, N, Board}, exit(done)
	end,
	run(first, N, {Down}, Result);
	
run(middle, N, {Up, Down}, Board) ->
	Result = receive
		{Supervisor, next} -> 
			NewBoard = next(middle, {Up, Down}, Board),
			Supervisor ! {self(), done},
			NewBoard;
		{Supervisor, stop} -> Supervisor ! {self(), finish, N, Board}, exit(done)
	end,
	run(middle, N, {Up, Down}, Result);
	
run(last, N, {Up}, Board) ->
	Result = receive
		{Supervisor, next} -> 
			NewBoard = next(last, {Up}, Board),
			Supervisor ! {self(), done},
			NewBoard;
		{Supervisor, stop} -> Supervisor ! {self(), finish, N, Board}, exit(done)
	end,
	run(last, N, {Up}, Result).



% funkcja odpowiadająca za wymianę wierszy pomiędzy węzłami i wykonanie iteracji
next(first, {Down}, Board) ->
	Down ! {up, array:get(array:size(Board)-2, Board)},
	UpdatedBoard = receive
		{down, Row} -> 
			array:set(array:size(Board)-1,Row,Board)
	end,
	computeRows(array:size(Board)-2, array:size(Board)-2, UpdatedBoard, Board);

next(middle, {Up, Down}, Board) ->
	Down ! {up, array:get(array:size(Board)-2, Board)},
	Up ! {down, array:get(1, Board)},
	UpdatedBoard = receive
		{down, Row} -> array:set(array:size(Board)-1,Row,Board);
		{up, Row} -> array:set(0,Row,Board)
	end,
	UpdatedBoard2 = receive
		{down, Row2} -> array:set(array:size(UpdatedBoard)-1,Row2,UpdatedBoard);
		{up, Row2} -> array:set(0,Row2,UpdatedBoard)
	end,
	computeRows(array:size(Board)-2, array:size(Board)-2, UpdatedBoard2, Board);

next(last, {Up}, Board) ->
	Up ! {down, array:get(1, Board)},
	UpdatedBoard = receive
		{up, Row} ->  
			array:set(0,Row,Board)
	end,
	computeRows(array:size(Board)-2, array:size(Board)-2, UpdatedBoard, Board).



% obliczanie nowej konfiguracji planszy
computeRows(_Len, 0, _Board, NewBoard) -> 
	NewBoard;
computeRows(Len, Count, Board, NewBoard) ->
	Row = array:get(Count, NewBoard),
	NewRow = array:map(fun(I,_) -> 
							Last = array:size(Row)-1,
							if
								I /= 0 andalso I /= Last -> 
									RowPrev = array:get(Count-1, Board),
									RowCurrent = array:get(Count, Board),
									RowNext = array:get(Count+1, Board),
									Value = array:get(I,RowCurrent),
									Neighbors = array:get(I-1,RowPrev)
												+ array:get(I,RowPrev)
												+ array:get(I+1,RowPrev)
												+ array:get(I-1,RowCurrent)
												+ array:get(I+1,RowCurrent)
												+ array:get(I-1,RowNext)
												+ array:get(I,RowNext)
												+ array:get(I+1,RowNext),
									if
										Value == 1 andalso Neighbors == 2 orelse Neighbors == 3 -> 1;
										Value == 0 andalso Neighbors == 3 -> 1;
										true -> 0
									end;
								true -> 0
							end
						end, Row),
	NewBoard2 = array:set(Count, NewRow, NewBoard),
	computeRows(Len, Count-1, Board, NewBoard2).
