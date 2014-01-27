-module(life_supervisor).
-export([init/3, next/2, test_time/2, stop/2]).

% funkcja inicjalizacyjna, zwraca listę PID'ów
init(HowManyNodes,Proccesses,Filename) when HowManyNodes >= 2 andalso Proccesses >= 1->
	Nodes = net_adm:world(),
	if
		length(Nodes) < HowManyNodes ->
			io:format("Brak wystarczajacej liczby wezlow.~n",[]),
			exit(numberOfNodes);
		true -> nop
	end,
	NodesToUse = lists:sublist(Nodes,1,HowManyNodes),
	c:nl(worker),
	{FD, Size} = lifeio:openRead(Filename),
	Length = trunc(math:pow(2,Size)),
	HowManyProccesses = HowManyNodes*Proccesses,
	Rows = Length div HowManyProccesses,
	RowsLast = trunc(Length - ((HowManyProccesses-1)*Rows)),
	Temp = lists:duplicate(HowManyProccesses-1, Rows),
	ListOfRows = lists:append(Temp, [RowsLast]),
	NodesMulProccesses = dupNodes(Proccesses,HowManyNodes,NodesToUse,[]),
	ThreadsList = startThreads(HowManyProccesses, NodesMulProccesses, ListOfRows, [], FD, Size),
	TArray = array:from_list(ThreadsList),
	Temp2 = lists:append([first], lists:duplicate(HowManyProccesses-2,middle)),
	Type = lists:append(Temp2, [last]),
	TyArray = array:from_list(Type),
	sendStartMsg(HowManyProccesses, HowManyProccesses, TArray, TyArray),
	ThreadsList.

% funkcja next, N - ilość iteracji; Nodes - lista PID'ów
next(0, _Nodes) -> ok;
next(N, Nodes) ->
	sendNextMsg(length(Nodes), length(Nodes), Nodes), 
	next(N-1, Nodes).

% sprawdzanie czasu wykonania, wypisuje średni czas wykonania jednej iteracji
% N - ilość prób, Nodes - lista PID'ów
test_time(N, Nodes) ->
	test_avg(life_supervisor, next, [1, Nodes], N).

% zakończenie działania i zapis planszy wynikowej do pliku
% Filename - nazwa pliku do zapisania planszy, Nodes - lista PID'ów
stop(Nodes, Filename) ->
	Len = length(Nodes),
	Result = sendStopMsg(Len, Len, Nodes, array:new([{size,Len},{fixed,true}])),
	Len2 = array:size(array:get(0, array:get(0,Result)))-2,
	Size = trunc(math:log(Len2)/math:log(2)),
	{ok,FD} = lifeio:openWrite(Filename,Size),
	io:format("Zapis planszy o rozmiarze ~Bx~B...",[Len2, Len2]), 
	file:write(FD,[Size]),
	writePart(Len, 0, Result, FD, Size),
	file:close(FD),
	io:format("OK~n",[]).

% ----------------------------------------------------------------
% ----------------------------------------------------------------
% ----------------------------------------------------------------

% uruchamianie procesów na zdalnych węzłach
startThreads(0, _Nodes, _Rows, L, _FD, _Size) -> lists:reverse(L);
startThreads(N, [H|T], [HL|TL], L, FD, Size) ->
	Pid = spawn(H, worker, start_computing, [lifeio:readPartOfBoard(FD, Size, HL)]),
	io:format("Thread created: ~p~n",[Pid]),
	startThreads(N-1, T, TL, [Pid|L], FD, Size).
% ----------------------------------------------------------------

% powielanie nazw węzłów
dupNodes(_Proccesses,0,_NodesToUse,Nodes) -> Nodes;
dupNodes(Proccesses,Count,[H|T],Nodes) ->
	dupNodes(Proccesses,Count-1,T,lists:append(Nodes,lists:duplicate(Proccesses,H))).
% ----------------------------------------------------------------

% wysłanie do węzłów wiadomości 'start'
sendStartMsg(_, 0, _, _) -> ok;
sendStartMsg(All, All, T, Ty) ->
	array:get(0,T) ! {self(), start, array:get(0,Ty),1,array:get(1,T)},
	%io:format("Start msg sent - first~n"),
	receive
		{_Pid, started} -> io:format("Computing started~n")
	end,
	sendStartMsg(All, All-1, T, Ty);
sendStartMsg(All, 1, T, Ty) ->
	array:get(All-1,T) ! {self(), start, array:get(All-1,Ty),All,array:get(All-2,T)},
	%io:format("Start msg sent - last~n"),
	receive
		{_Pid, started} -> io:format("Computing started~n")
	end,
	sendStartMsg(All, 0, T, Ty);
sendStartMsg(All, Count, T, Ty) ->
	array:get(All-Count,T) ! {self(), start, array:get(All-Count,Ty),All-Count+1,array:get(All-Count-1,T),array:get(All-Count+1,T)},
	%io:format("Start msg sent - middle~n"),
	receive
		{_Pid, started} -> io:format("Computing started~n")
	end,
	sendStartMsg(All, Count-1, T, Ty).
% ----------------------------------------------------------------

% wysłanie wiadomości 'next' - wykonanie kolejnej iteracji
sendNextMsg(All,0, _) -> receiveNextDoneStatus(All);
sendNextMsg(All, Count, [H|T]) ->
	H ! {self(), next},
	sendNextMsg(All,Count-1,T).

receiveNextDoneStatus(0) -> ok;
receiveNextDoneStatus(Count) ->
	receive
		{_Pid, done} -> next_done
	end,
	receiveNextDoneStatus(Count-1).
% ----------------------------------------------------------------

% zatrzymanie obliczeń, zakończenie działania procesów w zdalnych węzłach
sendStopMsg(_, 0, _, Result) -> Result;
sendStopMsg(All, Count, [H|T], Result) ->
	H ! {self(), stop},
	NewResult = receive
		{_Pid, finish, N, Board} -> array:set(N-1, Board, Result)
	end,
	sendStopMsg(All, Count-1, T, NewResult).
% ----------------------------------------------------------------

% zapis do pliku
writePart(0, _Count, _Board, _FD, _Size) -> ok;
writePart(Len, Count, Board, FD, Size) ->
	lifeio:writePartOfBoard(FD, Size, array:get(Count, Board)),
	writePart(Len-1, Count+1, Board, FD, Size).
% ----------------------------------------------------------------

% funkcja mierząca czas wykonania
test_avg(M, F, A, N) when N > 0 ->
    L = test_loop(M, F, A, N, []),
    Length = length(L),
    Min = lists:min(L),
    Max = lists:max(L),
    Med = lists:nth(round((Length / 2)), lists:sort(L)),
    Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
    io:format("Range: ~b - ~b mics~n"
          "Median: ~b mics~n"
          "Average: ~b mics~n",
          [Min, Max, Med, Avg]),
    Avg.
 
test_loop(_M, _F, _A, 0, List) ->
    List;
test_loop(M, F, A, N, List) ->
    {T, _Result} = timer:tc(M, F, A),
    test_loop(M, F, A, N - 1, [T|List]).
