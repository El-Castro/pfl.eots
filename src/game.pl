:- use_module(library(lists)).
:- use_module(library(random)).
:- ensure_loaded('game_over.pl').
:- ensure_loaded('valid_moves.pl').
:- ensure_loaded('move.pl').
:- ensure_loaded('display_game.pl').
:- ensure_loaded('choose_move.pl').
:- ensure_loaded('value.pl').
:- ensure_loaded('setup.pl').

% Initialize the game
play :-
    game_menu,
    read_choice(GameType),
    setup_game_config(GameType, GameConfig),
    initial_state(GameConfig, GameState),
    game_loop(GameState, GameConfig, 1).


% Display the game menu
game_menu :-
    nl, nl,
    display_ascii_art,
    write('       -----------------------------------------------------------------------------------------------------------------'), nl,
    write('       Welcome to Storm Clouds!'), nl,
    write('       Each player takes turns moving their pieces. The goal is to eliminate all opponent pieces.'), nl,
    write('       Moves are divided into non-capturing (like a chess king) and capturing (like a chess queen):'), nl,
    write('         - White non-capturing moves: east, northeast, north, northwest'), nl,
    write('         - White capturing moves: south, southeast, southwest, west'), nl,
    write('         - Black non-capturing moves: north, northeast, east, southeast'), nl,
    write('         - Black capturing moves: south, southwest, west, northwest'), nl,
    write('       -----------------------------------------------------------------------------------------------------------------'), nl,
    write('       Choose the game type:'), nl,
    write('       1. Human vs Human'),nl, 
    write('       2. Human vs PC'), nl, 
    write('       3. PC vs Human'), nl, 
    write('       4. PC vs PC'), nl, 
    write('       5. Exit'), nl,
    write('       -----------------------------------------------------------------------------------------------------------------'), nl,
    write('       Enter your choice: ').



% Read the game type choice
read_choice(GameType) :-
    read(Input),
    validate_choice(Input, GameType).


% Validate the game type choice
validate_choice(1, 1).
validate_choice(2, 2).
validate_choice(3, 3).
validate_choice(4, 4).
validate_choice(5, _) :-
    halt.  % Exit the SICStus Prolog terminal
validate_choice(_, GameType) :-
    write('       Invalid choice. Try Again.'), nl,
    write('       '),
    read_choice(GameType).


% Setup the initial board state
initial_state(_, state(Board, white)) :-
    create_initial_board(Board).


% Check if a player is a computer based on the game configuration
is_pc_player(white, game_config(player1(pc(Level)), _), Level).
is_pc_player(black, game_config(_, player2(pc(Level))), Level).


% Game loop for the game over state
game_loop(state(Board, CurrentPlayer), _, Turn) :-
    game_over(state(Board, CurrentPlayer), Winner),
    !,
    nl, nl, nl,
    display_game(state(Board, CurrentPlayer)),
    print_winner(Winner, Turn).

% Game loop for the normal game state
game_loop(state(Board, CurrentPlayer), GameConfig, Turn) :-
    nl, write('       ---------------------------------------------------------------------------------------------------------------------'), nl, nl,
    write('       Turn: '), write(Turn), write(' - '), write(CurrentPlayer), write(' is now playing. '), nl, nl,
    display_game(state(Board, CurrentPlayer)),
    valid_moves(state(Board, CurrentPlayer), ValidMoves),
    handle_moves(state(Board, CurrentPlayer), GameConfig, ValidMoves, Turn).


% Handle case where no valid moves are available for the current player
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
    display_pc_move(Move, CurrentPlayer),
    confirm_move(state(Board, CurrentPlayer), GameConfig, Move, ValidMoves, Turn).

% Handle case where valid moves exist and the current player is human
handle_moves(state(Board, CurrentPlayer), GameConfig, ValidMoves, Turn) :-
    \+ is_pc_player(CurrentPlayer, GameConfig, _),
    !,
    choose_move(state(Board, CurrentPlayer), 0, Move),
    confirm_move(state(Board, CurrentPlayer), GameConfig, Move, ValidMoves, Turn).


% Execute a valid move
confirm_move(state(Board, CurrentPlayer), GameConfig, Move, ValidMoves, Turn) :-
    member(Move, ValidMoves), !,
    move(state(Board, CurrentPlayer), Move, NewGameState),
    NewTurn is Turn + 1,
    game_loop(NewGameState, GameConfig, NewTurn).

% Print a message describing the computer move
display_pc_move(move(Y, X, Direction, NewY, NewX), CurrentPlayer) :-
    write('       Computer ('), write(CurrentPlayer), write(') moved ('), write(X), write(','), write(Y), write(') towards '), write(Direction), write(' ('), write(NewX), write(','), write(NewY), write(').'), nl, nl, nl.
