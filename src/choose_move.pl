choose_move(state(Board, CurrentPlayer), 1, Move) :-
    valid_moves(state(Board, CurrentPlayer), ValidMoves),
    random_member(Move, ValidMoves).


choose_move(state(Board, CurrentPlayer), 2, Move) :-
    valid_moves(state(Board, CurrentPlayer), ValidMoves),
    find_best_moves(state(Board, CurrentPlayer), ValidMoves, BestMoves),
    random_member(Move, BestMoves).


find_best_moves(state(Board, CurrentPlayer), [Move|Moves], BestMoves) :-
    move(state(Board, CurrentPlayer), Move, NewGameState),
    opposite_player(CurrentPlayer, Opponent),
    value(NewGameState, Opponent, BestValue),
    find_best_moves(state(Board, CurrentPlayer), Moves, [Move], BestValue, BestMoves).


find_best_moves(_, [], BestMoves, _, BestMoves).
find_best_moves(state(Board, CurrentPlayer), [Move|Moves], CurrentBestMoves, CurrentBestValue, BestMoves) :-
    move(state(Board, CurrentPlayer), Move, NewGameState),
    opposite_player(CurrentPlayer, Opponent),
    value(NewGameState, Opponent, Value),
    update_best_moves(Move, Value, CurrentBestMoves, CurrentBestValue, UpdatedBestMoves, UpdatedBestValue),
    find_best_moves(state(Board, CurrentPlayer), Moves, UpdatedBestMoves, UpdatedBestValue, BestMoves).

% Update the list of best moves based on the current moves value
update_best_moves(Move, Value, _, CurrentBestValue, [Move], Value) :-
    Value < CurrentBestValue, !. % New best value found, replace.
update_best_moves(Move, Value, CurrentBestMoves, CurrentBestValue, [Move|CurrentBestMoves], CurrentBestValue) :-
    Value =:= CurrentBestValue, !. % Tie, add the move to the list.
update_best_moves(_, Value, CurrentBestMoves, CurrentBestValue, CurrentBestMoves, CurrentBestValue) :-
    Value > CurrentBestValue. % Move is not better, keep current best moves.

