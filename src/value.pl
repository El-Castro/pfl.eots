value(state(Board, _), Player, Value) :-
    % Determine the opponent
    opposite_player(Player, Opponent),

    % Count the number of pieces for the current player
    findall(_, (member(Row, Board), member(Player, Row)), PlayerPieces),
    length(PlayerPieces, PlayerCount),
    
    % Count the number of pieces for the opponent
    findall(_, (member(Row, Board), member(Opponent, Row)), OpponentPieces),
    length(OpponentPieces, OpponentCount),
    
    % Calculate the difference in piece count and make value
    PieceValue is 10 * (PlayerCount - OpponentCount),
    
    
    % Calculate the coordinate-based value for allied pieces
    findall(
        Sum,
        (nth0(X, Board, Row), nth0(Y, Row, Player), Sum is X + Y),
        CoordinateValues
    ),
    sum_list(CoordinateValues, CoordinateValue),
    
    findall(
        Sum,
        (nth0(X, Board, Row), nth0(Y, Row, Opponent), Sum is X + Y),
        OppCoordinateValues
    ),
    sum_list(OppCoordinateValues, OppCoordinateValue),

    PositionValue is CoordinateValue - OppCoordinateValue,

    
    % Combine both values
    Value is PieceValue + PositionValue.

% Helper to sum a list of numbers
sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, Rest),
    Sum is H + Rest.
