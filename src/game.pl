:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(ansi_term)).
:- ensure_loaded('game_over.pl').
:- ensure_loaded('valid_moves.pl').
:- ensure_loaded('move.pl').
:- ensure_loaded('display_game.pl').
:- ensure_loaded('choose_move.pl').


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
setup_game_config(2, game_config(player1(human), player2(pc))).
setup_game_config(3, game_config(player1(pc), player2(human))).
setup_game_config(4, game_config(player1(pc), player2(pc))).


% ------------------------------------------------------------------------------------------------------------------------------------------------------------
% Actual Game functions --------------------------------------------------------------------------------------------------------------------------------------
% ------------------------------------------------------------------------------------------------------------------------------------------------------------


play :-
game_menu,
read_choice(GameType),
setup_game_config(GameType, GameConfig),
initial_state(GameState),
game_loop(GameState, GameConfig).

game_menu :- write('Welcome to Storm Clouds!'), nl, write('1. Human vs Human'),nl, write('2. Human vs PC'), nl, write('3. PC vs Human'), nl, write('4. PC vs PC'), nl, write('5. Exit'), nl.
read_choice(GameType) :-
read(Input),
( member(Input, [1,2,3,4]) -> GameType = Input ; write('Invalid choice'), fail ).

initial_state(state(Board, white)) :-
create_initial_board(Board).

read_move(Move) :-
write('Enter your move as move(Row, Col, Direction, NewRow, NewCol):'),
read(Move).

is_pc_player(white, game_config(player1(pc), _)).
is_pc_player(black, game_config(_, player2(pc))).

game_loop(state(Board, CurrentPlayer), GameConfig) :-
    display_game(state(Board, CurrentPlayer)),

    ( game_over(state(Board, CurrentPlayer), Winner) ->
        print_winner(Winner)
    ;
        write('       Player Turn: '), write(CurrentPlayer), nl, nl,

        valid_moves(state(Board, CurrentPlayer), ValidMoves),
        ( ValidMoves = [] ->
            write('No valid moves available. Passing turn.'), nl,
            switch_player(CurrentPlayer, NextPlayer),
            game_loop(state(Board, NextPlayer), GameConfig)
        ;
            % Check if the current player is human or PC
            ( is_pc_player(CurrentPlayer, GameConfig) ->
                write('Computer is choosing a move...'), nl,
                choose_move(state(Board, CurrentPlayer), 1, Move),
                write('Computer chose: '), write(Move), nl
            ;
                write('Valid moves: '), write(ValidMoves), nl,
                read_move(Move)
            ),
            % Validate the move and proceed
            ( member(Move, ValidMoves) ->
                move(state(Board, CurrentPlayer), Move, NewGameState),
                game_loop(NewGameState, GameConfig)
            ;
                write('Invalid move. Try again.'), nl,
                game_loop(state(Board, CurrentPlayer), GameConfig)
            )
        )
    ).

