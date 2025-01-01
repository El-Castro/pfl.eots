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
    write('       ---------------------------------------------------------------------------------------------------------------------'), nl,
    write('       Welcome to Storm Clouds!'), nl,
    write('       ---------------------------------------------------------------------------------------------------------------------'), nl,
    write('       Choose the game type:'), nl,
    write('       1. Human vs Human'),nl, 
    write('       2. Human vs PC'), nl, 
    write('       3. PC vs Human'), nl, 
    write('       4. PC vs PC'), nl, 
    write('       5. Exit'), nl,
    write('       ---------------------------------------------------------------------------------------------------------------------'), nl,
    write('       Enter your choice: ').


read_choice(GameType) :-
    read(Input), ( member(Input, [1,2,3,4]) -> GameType = Input ; 
    write('Invalid choice'), fail ).


initial_state(state(Board, white)) :-
    create_initial_board(Board).


read_move(Move) :-
    write('       Enter the coordinates of the piece to move:'), nl,
    write('         Select X: '), 
    read(Col),
    write('         Select Y: '), 
    read(Row),
    write('       Direction to move: '), 
    read(Direction),
    new_position(Row, Col, Direction, NewRow, NewCol),
    Move = move(Row, Col, Direction, NewRow, NewCol).


is_pc_player(white, game_config(player1(pc(Level)), _), Level).
is_pc_player(black, game_config(_, player2(pc(Level))), Level).


game_loop(state(Board, CurrentPlayer), GameConfig, Turn) :-
    ( game_over(state(Board, CurrentPlayer), Winner) ->
        nl,nl,nl, display_game(state(Board, CurrentPlayer)),
        print_winner(Winner)
    ;
        write('       ---------------------------------------------------------------------------------------------------------------------'), nl, nl,
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
                read_move(Move)
            ),
            ( member(Move, ValidMoves) ->
                move(state(Board, CurrentPlayer), Move, NewGameState),
                NewTurn is Turn + 1,
                game_loop(NewGameState, GameConfig, NewTurn)
            ;
                nl, write('       ----------------------------'), nl,
                write('       | Invalid move. Try again! |'), nl,
                write('       ----------------------------'), nl, nl,
                game_loop(state(Board, CurrentPlayer), GameConfig, Turn)
            )
        )
    ).
