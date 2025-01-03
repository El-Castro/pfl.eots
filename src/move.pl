move(state(Board, CurrentPlayer), Move, state(NewBoard, NextPlayer)) :-
    % Step 1: Generate all valid moves
    valid_moves(state(Board, CurrentPlayer), ValidMoves),

    % Step 2: Check if the given move is valid
    member(Move, ValidMoves),

    % Step 3: Execute the move to update the board
    execute_move(Board, Move, CurrentPlayer, NewBoard),

    % Step 4: Switch to the next player
    switch_player(CurrentPlayer, NextPlayer).


% switch_player/2: Alternates between black and white
switch_player(black, white).
switch_player(white, black).


% execute_move(+Board, +Move, +Player, -NewBoard)
execute_move(Board, move(Row, Col, _, NewRow, NewCol), Player, NewBoard) :-

    % Replace the current position with `empty`
    replace(Board, Row, Col, empty, TempBoard),

    % Place the players piece at the new position
    replace(TempBoard, NewRow, NewCol, Player, NewBoard).


% replace(+Board, +Row, +Col, +Value, -NewBoard)
replace(Board, Row, Col, Value, NewBoard) :-
    nth1(Row, Board, OldRow),
    replace_in_list(OldRow, Col, Value, NewRow),
    replace_in_list(Board, Row, NewRow, NewBoard).


% replace_in_list(+List, +Index, +Value, -NewList)
replace_in_list([_|Tail], 1, Value, [Value|Tail]).
replace_in_list([Head|Tail], Index, Value, [Head|NewTail]) :-
    Index > 1,
    NextIndex is Index - 1,
    replace_in_list(Tail, NextIndex, Value, NewTail).
