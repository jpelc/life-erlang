-module(lifeio).
-export([openRead/1,openWrite/2,readBoard/1,writeRandomBoard/1,writeBoard/2,createNewBoard/2,readBoardAsList/1]).

openRead(FileName) ->
	io:format("Otwieranie pliku ~s do odczytu...",[FileName]),
	{ok,FD} = file:open(FileName,[read,compressed]),
	case file:read(FD,1) of 
		{ok,[Data]} ->
			io:format("OK~n",[]), 
			{FD,Data};
		eof -> io:format("~nKoniec pliku~n",[]);
		{error,Reason} -> io:format("~n~s~n",[Reason])
	end.

readData(FD,Length) ->
	case file:read(FD,Length) of 
		{ok,Data} -> lists:map(fun(X) -> X-48 end, Data);
		eof -> io:format("~nKoniec pliku~n",[]);
		{error,Reason} -> io:format("~n~s~n",[Reason])
	end.

openWrite(FileName,Size)->
	io:format("Otwieranie pliku ~s do zapisu...",[FileName]),
	{ok,FD} = file:open(FileName,[write,compressed]),
	file:write(FD,Size),
	io:format("OK~n",[]), 
	{ok,FD}.

writeData(FD,Data) ->
	file:write(FD,Data).

readBoardAsList(FileName) ->
	{FD,Size} = openRead(FileName),
	Len = trunc(math:pow(2,Size)),
	io:format("Wczytywanie planszy: ~Bx~B (~B)~n",[Len,Len,Size]),
	Data = getDataAsList(FD,Len,Len, []),
	file:close(FD),
	Data.

getDataAsList(_FD,_Len,0,Acc) -> lists:reverse(Acc);
getDataAsList(FD,Len,Count,Acc) ->
	getDataAsList(FD,Len,Count-1,[readData(FD,Len)|Acc]).

readBoard(FileName) ->
	{FD,Size} = openRead(FileName),
	Len = trunc(math:pow(2,Size)),
	io:format("Wczytywanie planszy: ~Bx~B (~B)~n",[Len,Len,Size]),
	Board = createNewBoard(Len+2,Len+2),
	Data = getData(FD,Len,1,Len,Board),
	file:close(FD),
	Data.

getData(_FD, _Len, _Count, 0, Board) -> Board;
getData(FD, Len, Count, Count2, Board) ->
	Row = readData(FD,Len),
	RowArr = createNewRow(Len+2),
	Result = setRow(1, Row, RowArr),
	Board2 = array:set(Count, Result, Board),
	getData(FD, Len, Count+1, Count2-1, Board2).

setRow(_Count, [], RowArr) -> RowArr;
setRow(Count, [H|T], RowArr) ->
	RowArr2 = array:set(Count, H, RowArr),
	setRow(Count+1, T, RowArr2).

writeBoard(FileName, Board) ->
	Len = array:size(Board)-2,
	Size = trunc(math:sqrt(Len)),
	{ok,FD} = openWrite(FileName,Size),
	io:format("Zapis planszy o rozmiarze ~Bx~B...",[Len, Len]), 
	file:write(FD,[Size]),
	saveArray(FD, Len, 1, Len, Board),
	file:close(FD),
	io:format("OK~n",[]).

saveArray(_FD, _Len, _Count, 0, _Board) -> ok;
saveArray(FD, Len, Count, Count2, Board) ->
	Row = array:get(Count, Board),
	RowList = arrayToList(Len, Len, Row, []),
	writeData(FD, lists:map(fun(X) -> X+48 end, RowList)),
	saveArray(FD, Len, Count+1, Count2-1, Board).

arrayToList(_Len, 0, _Row, Acc) -> Acc;
arrayToList(Len, Count, Row, Acc) ->
	arrayToList(Len, Count-1, Row, [array:get(Count, Row)|Acc]).

writeRandomBoard(Size) ->
	Len = trunc(math:pow(2,Size)),
	{ok,FD} = openWrite('board.gz',Size),
	io:format("Zapis planszy o rozmiarze ~Bx~B...",[Len, Len]), 
	file:write(FD,[Size]),
	randomData(FD,Len,Len),
	file:close(FD),
	io:format("OK~n",[]).

randomData(_FD,0,_Len)-> ok;
randomData(FD,Count,Len) ->
	Data = [random:uniform(2)+47 || _ <- lists:seq(1, Len)],
	writeData(FD,Data),
	randomData(FD,Count-1,Len).

createNewBoard(X, Y) ->
	array:map(fun(_, _) -> createNewRow(Y) end, array:new([{size,X},{fixed,true}])).

createNewRow(Len) ->
	array:new([{size,Len},{fixed,true},{default,0}]).
