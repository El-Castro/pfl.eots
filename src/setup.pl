create_initial_board(Board) :-
Board = [
    [empty, empty, white, white, white, white, white, white],
    [empty, empty, white, white, white, white, white, white],
    [black, black, empty, empty, empty, empty, empty, empty],
    [black, black, empty, empty, empty, empty, empty, empty],
    [black, black, empty, empty, empty, empty, empty, empty],
    [black, black, empty, empty, empty, empty, empty, empty],
    [black, black, empty, empty, empty, empty, empty, empty],
    [black, black, empty, empty, empty, empty, empty, empty]
].

game_config(player1(type), player2(type)).

setup_game_config(1, game_config(player1(human), player2(human))).
setup_game_config(2, game_config(player1(human), player2(pc(Level)))) :-
    write('       Choose difficulty level for PC (1 or 2): '), read(Level), nl.
setup_game_config(3, game_config(player1(pc(Level)), player2(human))) :-
    write('       Choose difficulty level for PC (1 or 2): '), read(Level), nl.
setup_game_config(4, game_config(player1(pc(Level1)), player2(pc(Level2)))) :-
    write('       Choose difficulty level for Player 1 (PC) (1 or 2): '),
    read(Level1),
    write('       Choose difficulty level for Player 2 (PC) (1 or 2): '),
    read(Level2), nl.