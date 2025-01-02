
black_non_capturing([north, northeast, east, southeast]).
black_capturing([south, southwest, west, northwest]).
white_non_capturing([east, northeast, north, northwest]).
white_capturing([south, southeast, southwest, west]).


valid_moves(state(Board, CurrentPlayer), ListOfMoves) :-
    (CurrentPlayer == black -> 
        black_non_capturing(NonCapturingMoves),
        black_capturing(CapturingMoves),
        find_all_moves(Board, black, NonCapturingMoves, CapturingMoves, ListOfMoves);
    CurrentPlayer == white -> 
        white_non_capturing(NonCapturingMoves),
        white_capturing(CapturingMoves),
        find_all_moves(Board, white, NonCapturingMoves, CapturingMoves, ListOfMoves)
    ).


% Find valid moves for player
find_all_moves(Board, Player, NonCapturingMoves, CapturingMoves, ListOfMoves) :-
    findall(Move, (
        piece_position(Board, Player, Row, Col), % Find each pieces position
        generate_non_capturing_moves(Row, Col, NonCapturingMoves, Board, Player, Move)
    ), NonCapturingList),
    findall(Move, (
        piece_position(Board, Player, Row, Col),
        generate_capturing_moves(Row, Col, CapturingMoves, Board, Player, Move)
    ), CapturingList),
    append(NonCapturingList, CapturingList, ListOfMoves).


% Find non-capturing moves for piece
generate_non_capturing_moves(Row, Col, Directions, Board, Player, move(Row, Col, Direction, NewRow, NewCol)) :-
    member(Direction, Directions),
    new_position(Row, Col, Direction, NewRow, NewCol),
    is_valid_move(Board, Player, NewRow, NewCol, Direction).


% Find capturing moves for piece
generate_capturing_moves(Row, Col, Directions, Board, Player, move(Row, Col, Direction, NewRow, NewCol)) :-
    member(Direction, Directions),
    new_position(Row, Col, Direction, NewRow, NewCol),
    is_valid_move(Board, Player, NewRow, NewCol, Direction).


% New position based on direction
new_position(Row, Col, north, NewRow, NewCol) :- NewRow is Row + 1, NewCol is Col.
new_position(Row, Col, northeast, NewRow, NewCol) :- NewRow is Row + 1, NewCol is Col + 1.
new_position(Row, Col, east, NewRow, NewCol) :- NewRow is Row, NewCol is Col + 1.
new_position(Row, Col, southeast, NewRow, NewCol) :- NewRow is Row - 1, NewCol is Col + 1.
new_position(Row, Col, south, NewRow, NewCol) :- NewRow is Row - 1, NewCol is Col.
new_position(Row, Col, southwest, NewRow, NewCol) :- NewRow is Row - 1, NewCol is Col - 1.
new_position(Row, Col, west, NewRow, NewCol) :- NewRow is Row, NewCol is Col - 1.
new_position(Row, Col, northwest, NewRow, NewCol) :- NewRow is Row + 1, NewCol is Col - 1.


% Check if the move is valid (is within the board boundaries and valid based on move type)
is_valid_move(Board, Player, Row, Col, Direction) :-
    within_board(Row, Col),
    (
        % Non-capturing move: Ensure the destination is empty and the direction is valid for non-capturing moves
        (player_non_capturing_moves(Player, NonCapturingMoves),
         member(Direction, NonCapturingMoves),
         \+ position_occupied(Board, Row, Col))
    ;
        % Capturing move: Ensure the destination has an opponents piece and the direction is valid for capturing moves
        (player_capturing_moves(Player, CapturingMoves),
         member(Direction, CapturingMoves),
         opposite_player(Player, Opponent),
         capture_valid(Board, Opponent, Row, Col))
    ).


position_occupied(Board, Row, Col) :-
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Piece),
    Piece \== empty.


% Check if the move is valid for capturing
capture_valid(Board, Opponent, Row, Col) :-
    % Logic for validating capturing moves (need to ensure the piece is the opponent and the capture is valid)
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Piece),
    Piece == Opponent.


% move is within board
within_board(Row, Col) :-
    Row >= 1, Row =< 8, Col >= 1, Col =< 8.


% Determine the opposite player
opposite_player(black, white).
opposite_player(white, black).


% Example of piece positions on the board (find all positions of a given players pieces)
piece_position(Board, Player, Row, Col) :-
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Piece),
    Piece == Player.

% Define dynamic capturing and non-capturing moves based on the player
player_non_capturing_moves(black, Moves) :-
    black_non_capturing(Moves).
player_non_capturing_moves(white, Moves) :-
    white_non_capturing(Moves).

player_capturing_moves(black, Moves) :-
    black_capturing(Moves).
player_capturing_moves(white, Moves) :-
    white_capturing(Moves).