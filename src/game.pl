:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(ansi_term)).
:- ensure_loaded('game_over.pl').
:- ensure_loaded('valid_moves.pl').
:- ensure_loaded('move.pl').
:- ensure_loaded('display_game.pl').
:- ensure_loaded('choose_move.pl').
:- ensure_loaded('value.pl').


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


% ------------------------------------------------------------------------------------------------------------------------------------------------------------
% Actual Game functions --------------------------------------------------------------------------------------------------------------------------------------
% ------------------------------------------------------------------------------------------------------------------------------------------------------------


play :-
game_menu,
read_choice(GameType),
setup_game_config(GameType, GameConfig),
initial_state(GameState),
game_loop(GameState, GameConfig, 1).

game_menu :- write('Welcome to Storm Clouds!'), nl, write('1. Human vs Human'),nl, write('2. Human vs PC'), nl, write('3. PC vs Human'), nl, write('4. PC vs PC'), nl, write('5. Exit'), nl.
read_choice(GameType) :-
read(Input),
( member(Input, [1,2,3,4]) -> GameType = Input ; write('Invalid choice'), fail ).

initial_state(state(Board, white)) :-
create_initial_board(Board).

read_move(Move) :-
write('       Enter your move as move(Row, Col, Direction, NewRow, NewCol):'), read(Move), nl.

is_pc_player(white, game_config(player1(pc(Level)), _), Level).
is_pc_player(black, game_config(_, player2(pc(Level))), Level).

game_loop(state(Board, CurrentPlayer), GameConfig, Turn) :-
    ( game_over(state(Board, CurrentPlayer), Winner) ->
        nl,nl,nl, display_game(state(Board, CurrentPlayer)),
        print_winner(Winner)
    ;
        write('       Turn: '), write(Turn), write(' - '), write(CurrentPlayer), write(' is now playing. '), nl, nl,
        display_game(state(Board, CurrentPlayer)),

        valid_moves(state(Board, CurrentPlayer), ValidMoves),
        ( ValidMoves = [] ->
            write('       No valid moves available. Passing turn.'), nl, nl,
            switch_player(CurrentPlayer, NextPlayer),
            NewTurn is Turn + 1,
            game_loop(state(Board, NextPlayer), GameConfig, NewTurn)
        ;
            ( is_pc_player(CurrentPlayer, GameConfig, Level) ->
                choose_move(state(Board, CurrentPlayer), Level, Move),
                write('       Computer ('), write(CurrentPlayer), write(') chose: '), write(Move), nl, nl, nl
            ;
                write('       Valid moves: '), write(ValidMoves), nl, nl,
                read_move(Move)
            ),
            ( member(Move, ValidMoves) ->
                move(state(Board, CurrentPlayer), Move, NewGameState),
                NewTurn is Turn + 1,
                game_loop(NewGameState, GameConfig, NewTurn)
            ;
                write('       Invalid move. Try again.'), nl, nl,
                game_loop(state(Board, CurrentPlayer), GameConfig, Turn)
            )
        )
    ).
