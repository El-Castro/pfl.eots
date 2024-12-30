game_over(state(Board, _), Winner) :-
    (
        % Check if black has no pieces left
        \+ piece_remaining(Board, black),
        Winner = white
    ;
        % Check if white has no pieces left
        \+ piece_remaining(Board, white),
        Winner = black
    ;
        fail
    ).

piece_remaining(Board, Player) :-
    member(Row, Board),
    member(Player, Row).

print_winner(Winner) :-
    write('       Game Over! The winner is: '), write(Winner), nl, nl.
