
display_game(state(Board, CurrentPlayer)) :-
    reverse(Board, ReversedBoard),  % Reverse the board to display it from bottom to top
    write('       -----------------------------------------------------------------'), nl,
    print_board_rows(ReversedBoard, 8),
    write('           1       2       3       4       5       6       7       8'), nl, nl.


print_board_rows([], _).
print_board_rows([Row|Rest], RowNumber) :-
    print_row(Row, RowNumber),
    NewRowNumber is RowNumber - 1,
    print_board_rows(Rest, NewRowNumber).


print_row(Row, RowNumber) :-
    write('    '), write(RowNumber), write('  | '),
    print_pieces(Row), nl,
    write('       -----------------------------------------------------------------'), nl.


print_pieces([]).
print_pieces([Piece|Rest]) :-
    ( Piece == white ->
        format('\e[34m~w\e[0m | ', [Piece])  % Blue for white
    ; Piece == black ->
        format('\e[31m~w\e[0m | ', [Piece])  % Red for black
    ; % Default color for other pieces (e.g., empty)
        format('~w | ', [Piece])
    ),
    print_pieces(Rest).


display_ascii_art :-
    nl,
    write('         ______    __                                               ______   __                            __           '), nl,
    write('        /      \\  /  |                                             /      \\ /  |                          /  |          '), nl,
    write('       /$$$$$$  |_$$ |_     ______    ______   _____  ____        /$$$$$$  |$$ |  ______   __    __   ____$$ |  _______ '), nl,
    write('       $$ \\__$$// $$   |   /      \\  /      \\ /     \\/    \\       $$ |  $$/ $$ | /      \\ /  |  /  | /    $$ | /       |'), nl,
    write('       $$      \\$$$$$$/   /$$$$$$  |/$$$$$$  |$$$$$$ $$$$  |      $$ |      $$ |/$$$$$$  |$$ |  $$ |/$$$$$$$ |/$$$$$$$/ '), nl,
    write('        $$$$$$  | $$ | __ $$ |  $$ |$$ |  $$/ $$ | $$ | $$ |      $$ |   __ $$ |$$ |  $$ |$$ |  $$ |$$ |  $$ |$$      \\ '), nl,
    write('       /  \\__$$ | $$ |/  |$$ \\__$$ |$$ |      $$ | $$ | $$ |      $$ \\__/  |$$ |$$ \\__$$ |$$ \\__$$ |$$ \\__$$ | $$$$$$  |'), nl,
    write('       $$    $$/  $$  $$/ $$    $$/ $$ |      $$ | $$ | $$ |      $$    $$/ $$ |$$    $$/ $$    $$/ $$    $$ |/     $$/ '), nl,
    write('        $$$$$$/    $$$$/   $$$$$$/  $$/       $$/  $$/  $$/        $$$$$$/  $$/  $$$$$$/   $$$$$$/   $$$$$$$/ $$$$$$$/  '), nl,
    nl.
