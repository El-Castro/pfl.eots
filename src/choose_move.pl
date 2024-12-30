
% choose_move(+GameState, +Level, -Move)
choose_move(state(Board, CurrentPlayer), _Level, Move) :-
    % Get all valid moves for the current game state
    valid_moves(state(Board, CurrentPlayer), ValidMoves),
    % Select a random move from the list of valid moves
    random_member(Move, ValidMoves).
