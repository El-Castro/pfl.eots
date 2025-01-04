% Game over: Determine the winner if one player has no pieces left
game_over(state(Board, _), white) :-
    \+ piece_remaining(Board, black). % Black has no pieces left, white wins.

game_over(state(Board, _), black) :-
    \+ piece_remaining(Board, white). % White has no pieces left, black wins.


% Check if a player has any pieces remaining
piece_remaining([], _) :-
    fail. % No pieces found in the empty board.

piece_remaining([Row|_], Player) :-
    member(Player, Row), % Players piece found in the current row.
    !. % Stop further search if a piece is found.

piece_remaining([_|Rest], Player) :-
    piece_remaining(Rest, Player). % Check the remaining rows.


% Print the message with the winner of the game
print_winner(Winner, Turn) :-
    NoTurns is Turn - 1,
    write('       Game Over! '), write(Winner), write(' is victorious after '), write(NoTurns), write(' turns!'), nl, nl.
