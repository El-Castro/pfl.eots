% Print the game board, including the current player and the value of the board for each player
display_game(state(Board, _)) :-
    reverse(Board, ReversedBoard),  % Reverse the board to display it from bottom to top
    write('       -----------------------------------------------------------------'), nl,
    print_board_rows(ReversedBoard, 8),
    write('           1       2       3       4       5       6       7       8'), nl, nl,
    value(state(Board, _), white, WhiteValue),
    value(state(Board, _), black, BlackValue),
    write('       white value: '), write(WhiteValue), nl,
    write('       black value: '), write(BlackValue), nl, nl.


% Print the rows of the board
print_board_rows([], _).
print_board_rows([Row|Rest], RowNumber) :-
    print_row(Row, RowNumber),
    NewRowNumber is RowNumber - 1,
    print_board_rows(Rest, NewRowNumber).


% Print a single row of the board
print_row(Row, RowNumber) :-
    write('    '), write(RowNumber), write('  | '),
    print_pieces(Row), nl,
    write('       -----------------------------------------------------------------'), nl.


% Print the pieces of a row
print_pieces([]).

print_pieces([white|Rest]) :-
    format('\e[34m~w\e[0m | ', [white]),  % Blue for white
    print_pieces(Rest).

print_pieces([black|Rest]) :-
    format('\e[31m~w\e[0m | ', [black]),  % Red for black
    print_pieces(Rest).

print_pieces([Piece|Rest]) :-
    format('~w | ', [Piece]),  % Default color
    print_pieces(Rest).


% Print the art above the menu
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
