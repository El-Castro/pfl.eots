value(state(Board, CurrentPlayer), CurrentPlayer, Value) :-
    % Count the number of pieces for the current player
    findall(_, (member(Row, Board), member(CurrentPlayer, Row)), PlayerPieces),
    length(PlayerPieces, PlayerCount),
    
    % Determine the opponent
    opposite_player(CurrentPlayer, Opponent),
    
    % Count the number of pieces for the opponent
    findall(_, (member(Row, Board), member(Opponent, Row)), OpponentPieces),
    length(OpponentPieces, OpponentCount),
    
    % Calculate the value as the difference
    Value is PlayerCount - OpponentCount.
