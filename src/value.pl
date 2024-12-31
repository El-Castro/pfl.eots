value(state(Board, CurrentPlayer), CurrentPlayer, Value) :-
    % Count the number of pieces for the current player
    findall(_, (member(Row, Board), member(CurrentPlayer, Row)), PlayerPieces),
    length(PlayerPieces, PlayerCount),
    
    % Determine the opponent
    opposite_player(CurrentPlayer, Opponent),
    
    % Count the number of pieces for the opponent
    findall(_, (member(Row, Board), member(Opponent, Row)), OpponentPieces),
    length(OpponentPieces, OpponentCount),
    
    % Calculate the difference in piece count and make value
    PieceValue is 1000 * (OpponentCount - PlayerCount),
    
    % Calculate the coordinate based value for opponent pieces
    findall(
        X + Y, 
        (nth0(X, Board, Row), nth0(Y, Row, Opponent)), 
        CoordinateValues
    ),
    sum_list(CoordinateValues, CoordinateValue),
    
    % Combine both values
    Value is PieceValue + CoordinateValue.

sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, Rest),
    Sum is H + Rest.