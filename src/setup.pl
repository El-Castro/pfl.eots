% Create initial board
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

% Game configuration
game_config(player1(type), player2(type), Color).

% Setup game configuration based on user input
setup_game_config(1, 1, game_config(player1(human), player2(human), white)).
setup_game_config(1, 2, game_config(player1(human), player2(human), black)).

setup_game_config(2, 1, game_config(player1(human), player2(pc(Level)), white)) :-
    write('       Choose difficulty level for PC (1 or 2): '), read(Level), nl.
setup_game_config(2, 2, game_config(player1(human), player2(pc(Level)), black)) :-
    write('       Choose difficulty level for PC (1 or 2): '), read(Level), nl.

setup_game_config(3, 1, game_config(player1(pc(Level)), player2(human), white)) :-
    write('       Choose difficulty level for PC (1 or 2): '), read(Level), nl.
setup_game_config(3, 2, game_config(player1(pc(Level)), player2(human), black)) :-
    write('       Choose difficulty level for PC (1 or 2): '), read(Level), nl.

setup_game_config(4, 1, game_config(player1(pc(Level1)), player2(pc(Level2)), white)) :-
    write('       Choose difficulty level for Player 1 (PC) (1 or 2): '),
    read(Level1),
    write('       Choose difficulty level for Player 2 (PC) (1 or 2): '),
    read(Level2), nl.
setup_game_config(4, 2, game_config(player1(pc(Level1)), player2(pc(Level2)), black)) :-
    write('       Choose difficulty level for Player 1 (PC) (1 or 2): '),
    read(Level1),
    write('       Choose difficulty level for Player 2 (PC) (1 or 2): '),
    read(Level2), nl.