:- use_module(library(lists)).
:- use_module(library(random)).
:- ensure_loaded('game_over.pl').
:- ensure_loaded('valid_moves.pl').
:- ensure_loaded('move.pl').
:- ensure_loaded('display_game.pl').
:- ensure_loaded('choose_move.pl').
:- ensure_loaded('value.pl').
:- ensure_loaded('setup.pl').


play :-
    game_menu,
    read_choice(GameType),
    setup_game_config(GameType, GameConfig),
    initial_state(GameState),
    game_loop(GameState, GameConfig, 1).


game_menu :-
    nl, nl,
    display_ascii_art,
    write('       -----------------------------------------------------------------------------------------------------------------'), nl,
    write('       Welcome to Storm Clouds!'), nl,
    write('       -----------------------------------------------------------------------------------------------------------------'), nl,
    write('       Choose the game type:'), nl,
    write('       1. Human vs Human'),nl, 
    write('       2. Human vs PC'), nl, 
    write('       3. PC vs Human'), nl, 
    write('       4. PC vs PC'), nl, 
    write('       5. Exit'), nl,
    write('       -----------------------------------------------------------------------------------------------------------------'), nl,
    write('       Enter your choice: ').


read_choice(GameType) :-
    read(Input),
    validate_choice(Input, GameType).

validate_choice(1, 1).
validate_choice(2, 2).
validate_choice(3, 3).
validate_choice(4, 4).
validate_choice(_, GameType) :-
    write('Invalid choice'), nl,
    read_choice(GameType).

initial_state(state(Board, white)) :-
    create_initial_board(Board).


read_move(Row, Col, Direction) :-
    write('       Enter the coordinates of the piece to move:'), nl,
    write('         Select X: '), 
    read(Col),
    write('         Select Y: '), 
    read(Row),
    write('       Direction to move (Ex:\'northwest\'): '), 
    read(Direction).



is_pc_player(white, game_config(player1(pc(Level)), _), Level).
is_pc_player(black, game_config(_, player2(pc(Level))), Level).



game_loop(state(Board, CurrentPlayer), _, Turn) :-
    game_over(state(Board, CurrentPlayer), Winner),
    !,
    nl, nl, nl,
    display_game(state(Board, CurrentPlayer)),
    print_winner(Winner, Turn).


game_loop(state(Board, CurrentPlayer), GameConfig, Turn) :-
    nl, write('       ---------------------------------------------------------------------------------------------------------------------'), nl, nl,
    write('       Turn: '), write(Turn), write(' - '), write(CurrentPlayer), write(' is now playing. '), nl, nl,
    display_game(state(Board, CurrentPlayer)),

    valid_moves(state(Board, CurrentPlayer), ValidMoves),
    handle_moves(state(Board, CurrentPlayer), GameConfig, ValidMoves, Turn).


% Handle case where no valid moves are available
handle_moves(state(Board, CurrentPlayer), GameConfig, [], Turn) :-
    write('       No valid moves available. Passing turn.'), nl, nl,
    switch_player(CurrentPlayer, NextPlayer),
    NewTurn is Turn + 1,
    game_loop(state(Board, NextPlayer), GameConfig, NewTurn).


% Handle case where valid moves exist and the current player is a computer
handle_moves(state(Board, CurrentPlayer), GameConfig, ValidMoves, Turn) :-
    is_pc_player(CurrentPlayer, GameConfig, Level),
    !,
    choose_move(state(Board, CurrentPlayer), Level, Move),
    display_move(Move, CurrentPlayer),
    execute_move(state(Board, CurrentPlayer), GameConfig, Move, ValidMoves, Turn).


% Handle case where valid moves exist and the current player is human
handle_moves(state(Board, CurrentPlayer), GameConfig, ValidMoves, Turn) :-
    \+ is_pc_player(CurrentPlayer, GameConfig, _),
    !,
    read_move(Row, Col, Direction),
    fetch_move(Row, Col, Direction, ValidMoves, Move),
    execute_move(state(Board, CurrentPlayer), GameConfig, Move, ValidMoves, Turn).


% Execute a valid move
execute_move(state(Board, CurrentPlayer), GameConfig, Move, ValidMoves, Turn) :-
    member(Move, ValidMoves),
    !,
    move(state(Board, CurrentPlayer), Move, NewGameState),
    NewTurn is Turn + 1,
    game_loop(NewGameState, GameConfig, NewTurn).


% Handle invalid move input
execute_move(state(Board, CurrentPlayer), GameConfig, _Move, _ValidMoves, Turn) :-
    nl, write('       ----------------------------'), nl,
    write('       | Invalid move. Try again! |'), nl,
    write('       ----------------------------'), nl, nl,
    game_loop(state(Board, CurrentPlayer), GameConfig, Turn).


display_move(move(Y, X, Direction, NewY, NewX), CurrentPlayer) :-
    write('       Computer ('), write(CurrentPlayer), write(') moved ('), write(X), write(','), write(Y), write(') towards '), write(Direction), write(' ('), write(NewX), write(','), write(NewY), write(').'), nl, nl, nl.

fetch_move(Row, Col, Direction, ValidMoves, Move) :-
    member(Move, ValidMoves), % Check if Move is in the list of valid moves
    Move = move(Row, Col, Direction, _, _). % Ensure it matches the given Row, Col, and Direction