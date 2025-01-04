% Returns the value of a given state for a given player
value(state(Board, _), Player, Value) :-
    opposite_player(Player, Opponent),

    findall(_, (member(Row, Board), member(Player, Row)), PlayerPieces),
    length(PlayerPieces, PlayerCount),
    findall(_, (member(Row, Board), member(Opponent, Row)), OpponentPieces),
    length(OpponentPieces, OpponentCount),
    
    PieceValue is 10 * (PlayerCount - OpponentCount),
    
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

    Value is PieceValue + PositionValue.


% Sum a list of numbers
sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, Rest),
    Sum is H + Rest.
