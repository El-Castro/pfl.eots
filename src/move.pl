% Check if the given move is valid, execute it and update the game state with the new board and player
move(state(Board, CurrentPlayer), Move, state(NewBoard, NextPlayer)) :-
    valid_moves(state(Board, CurrentPlayer), ValidMoves),
    member(Move, ValidMoves),
    execute_move(Board, Move, CurrentPlayer, NewBoard),
    switch_player(CurrentPlayer, NextPlayer).


% Switch the player
switch_player(black, white).
switch_player(white, black).


% Execute the move on the board
execute_move(Board, move(Row, Col, _, NewRow, NewCol), Player, NewBoard) :-
    replace(Board, Row, Col, empty, TempBoard),
    replace(TempBoard, NewRow, NewCol, Player, NewBoard).


% Create a new board with the given piece at the specified position, based on the current board
replace(Board, Row, Col, Value, NewBoard) :-
    nth1(Row, Board, OldRow),
    replace_in_list(OldRow, Col, Value, NewRow),
    replace_in_list(Board, Row, NewRow, NewBoard).


% Replace an element in a list at the specified index
replace_in_list([_|Tail], 1, Value, [Value|Tail]).
replace_in_list([Head|Tail], Index, Value, [Head|NewTail]) :-
    Index > 1,
    NextIndex is Index - 1,
    replace_in_list(Tail, NextIndex, Value, NewTail).
