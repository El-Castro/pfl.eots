display_game(state(Board, CurrentPlayer)) :-
    reverse(Board, ReversedBoard),  % Reverse the board to display it from bottom to top
    print_board_rows(ReversedBoard, 8), nl, write('Current Player: '), write(CurrentPlayer), nl.

print_board_rows([], _).
print_board_rows([Row|Rest], RowNumber) :-
    print_row(Row, RowNumber),
    NewRowNumber is RowNumber - 1,
    print_board_rows(Rest, NewRowNumber).

print_row(Row, RowNumber) :-
    write('Row '), write(RowNumber), write(': '),
    print_pieces(Row), nl.

print_pieces([]).
print_pieces([Piece|Rest]) :-
    write(Piece), write(' '),
    print_pieces(Rest).
