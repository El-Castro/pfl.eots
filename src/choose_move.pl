choose_move(state(Board, CurrentPlayer), 1, Move) :-
    valid_moves(state(Board, CurrentPlayer), ValidMoves),
    random_member(Move, ValidMoves).


choose_move(state(Board, CurrentPlayer), 2, Move) :-
    valid_moves(state(Board, CurrentPlayer), ValidMoves),
    find_best_move(state(Board, CurrentPlayer), ValidMoves, Move).

find_best_move(GameState, [Move|Moves], BestMove) :-
    apply_move(GameState, Move, NewGameState),
    value(NewGameState, CurrentPlayer, BestValue),
    find_best_move(GameState, Moves, Move, BestValue, BestMove).

find_best_move(_, [], CurrentBestMove, _, CurrentBestMove).
find_best_move(GameState, [Move|Moves], CurrentBestMove, CurrentBestValue, BestMove) :-
    apply_move(GameState, Move, NewGameState),
    value(NewGameState, CurrentPlayer, Value),
    (Value > CurrentBestValue ->
        find_best_move(GameState, Moves, Move, Value, BestMove)
    ;
        find_best_move(GameState, Moves, CurrentBestMove, CurrentBestValue, BestMove)
    ).

apply_move(state(Board, CurrentPlayer), Move, NewGameState) :-
    move(state(Board, CurrentPlayer), Move, NewGameState).

