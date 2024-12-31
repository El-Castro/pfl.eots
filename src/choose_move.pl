choose_move(state(Board, CurrentPlayer), 1, Move) :-
    valid_moves(state(Board, CurrentPlayer), ValidMoves),
    random_member(Move, ValidMoves).

choose_move(state(Board, CurrentPlayer), 2, Move) :-
    valid_moves(state(Board, CurrentPlayer), ValidMoves),
    find_best_moves(state(Board, CurrentPlayer), ValidMoves, BestMoves),
    random_member(Move, BestMoves).

find_best_moves(GameState, [Move|Moves], BestMoves) :-
    apply_move(GameState, Move, NewGameState),
    value(NewGameState, CurrentPlayer, BestValue),
    find_best_moves(GameState, Moves, [Move], BestValue, BestMoves).

find_best_moves(_, [], BestMoves, _, BestMoves).
find_best_moves(GameState, [Move|Moves], CurrentBestMoves, CurrentBestValue, BestMoves) :-
    apply_move(GameState, Move, NewGameState),
    value(NewGameState, CurrentPlayer, Value),
    (Value > CurrentBestValue ->
        % New best value found, replace current best moves
        find_best_moves(GameState, Moves, [Move], Value, BestMoves)
    ;
     Value == CurrentBestValue ->
        % Tie for best value, add move to the list
        find_best_moves(GameState, Moves, [Move|CurrentBestMoves], CurrentBestValue, BestMoves)
    ;
        % Move is not better, continue with current best moves
        find_best_moves(GameState, Moves, CurrentBestMoves, CurrentBestValue, BestMoves)
    ).

apply_move(state(Board, CurrentPlayer), Move, NewGameState) :-
    move(state(Board, CurrentPlayer), Move, NewGameState).


