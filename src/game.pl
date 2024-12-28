game_config(player1(type), player2(type)).


create_initial_board(Board) :-
    Board = [
        [black, black, empty, empty, empty, empty, empty, empty],
        [black, black, empty, empty, empty, empty, empty, empty],
        [black, black, empty, empty, empty, empty, empty, empty],
        [black, black, empty, empty, empty, empty, empty, empty],
        [black, black, empty, empty, empty, empty, empty, empty],
        [black, black, empty, empty, empty, empty, empty, empty],
        [empty, empty, white, white, white, white, white, white],
        [empty, empty, white, white, white, white, white, white]
    ].

setup_game_config(1, game_config(player1(human), player2(human))).
setup_game_config(2, game_config(player1(human), player2(pc))).
setup_game_config(3, game_config(player1(pc), player2(human))).
setup_game_config(4, game_config(player1(pc), player2(pc))).

state(Board,CurrentPlayer).

display_game(state(Board, CurrentPlayer)) :-
    print_board_rows(Board, 1), nl, write('Current Player: '), write(CurrentPlayer), nl.

print_board_rows([], _).
print_board_rows([Row|Rest], RowNumber) :-
    print_row(Row, RowNumber),
    NewRowNumber is RowNumber + 1,
    print_board_rows(Rest, NewRowNumber).

print_row(Row, RowNumber) :-
    write('Row '), write(RowNumber), write(': '),
    print_pieces(Row), nl.

print_pieces([]).
print_pieces([Piece|Rest]) :-
    write(Piece), write(' '),
    print_pieces(Rest).



% Actual Game functions --------------------------------------------------------------------------------------------------------------------------------------



play :-
    game_menu,
    read_choice(GameType),
    setup_game_config(GameType, GameConfig),
    initial_state(GameState),
    game_loop(GameState).

game_menu :- write('Welcome to Storm Clouds!'), nl, write('1. Human vs Human'),nl, write('2. Human vs PC'), nl, write('3. PC vs Human'), nl, write('4. PC vs PC'), nl, write('5. Exit').
read_choice(GameType) :-
    read(Input),
    ( member(Input, [1,2,3,4]) -> GameType = Input ; writeln('Invalid choice'), fail ).

initial_state(state(Board, white)) :-
    create_initial_board(Board).






