
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
    (Value < CurrentBestValue ->
        % New best value found, so its time to replace
        find_best_moves(state(Board, CurrentPlayer), Moves, [Move], Value, BestMoves)
    ;
     Value == CurrentBestValue ->
        % Best value tie so add move to the list
        find_best_moves(state(Board, CurrentPlayer), Moves, [Move|CurrentBestMoves], CurrentBestValue, BestMoves)
    ;
        % Move is not better so continue 
        find_best_moves(state(Board, CurrentPlayer), Moves, CurrentBestMoves, CurrentBestValue, BestMoves)
    ).
