% Define valid directions for each player based on the rules of the game
black_non_capturing([north, northeast, east, southeast]).
black_capturing([south, southwest, west, northwest]).
white_non_capturing([east, northeast, north, northwest]).
white_capturing([south, southeast, southwest, west]).


% Define valid moves for black
valid_moves(state(Board, black), ListOfMoves) :-
    black_non_capturing(NonCapturingMoves),
    black_capturing(CapturingMoves),
    find_all_moves(Board, black, NonCapturingMoves, CapturingMoves, ListOfMoves).

% Define valid moves for white
valid_moves(state(Board, white), ListOfMoves) :-
    white_non_capturing(NonCapturingMoves),
    white_capturing(CapturingMoves),
    find_all_moves(Board, white, NonCapturingMoves, CapturingMoves, ListOfMoves).


% Find all valid moves for a player based on the board state
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
    is_valid_non_capturing_move(Board, Player, NewRow, NewCol).


% Find capturing moves for piece
generate_capturing_moves(Row, Col, Directions, Board, Player, move(Row, Col, Direction, NewRow, NewCol)) :-
    member(Direction, Directions),
    generate_capturing_move_for_direction(Row, Col, Direction, Board, Player, NewRow, NewCol).

% Check a direction for capturing moves
generate_capturing_move_for_direction(Row, Col, Direction, Board, Player, NewRow, NewCol) :-
    opposite_player(Player, Opponent),
    generate_capturing_move_in_direction(Row, Col, Direction, Board, Opponent, NewRow, NewCol).

% Recursively check capturing moves in the given direction until a valid move is impossible
generate_capturing_move_in_direction(Row, Col, Direction, Board, Opponent, NewRow, NewCol) :-
    new_position(Row, Col, Direction, TempRow, TempCol),  % Calculate new position
    within_board(TempRow, TempCol),  % Ensure the new position is within the board
    piece_position(Board, Opponent, TempRow, TempCol),
    NewRow = TempRow,
    NewCol = TempCol.
   
generate_capturing_move_in_direction(Row, Col, Direction, Board, Opponent, NewRow, NewCol) :-
    new_position(Row, Col, Direction, TempRow, TempCol),  % Calculate new position
    within_board(TempRow, TempCol),  % Ensure the new position is within the board
    piece_position(Board, empty, TempRow, TempCol),
    generate_capturing_move_in_direction(TempRow, TempCol, Direction, Board, Opponent, NewRow, NewCol).


% New position based on direction
new_position(Row, Col, north, NewRow, NewCol) :- NewRow is Row + 1, NewCol is Col.
new_position(Row, Col, northeast, NewRow, NewCol) :- NewRow is Row + 1, NewCol is Col + 1.
new_position(Row, Col, east, NewRow, NewCol) :- NewRow is Row, NewCol is Col + 1.
new_position(Row, Col, southeast, NewRow, NewCol) :- NewRow is Row - 1, NewCol is Col + 1.
new_position(Row, Col, south, NewRow, NewCol) :- NewRow is Row - 1, NewCol is Col.
new_position(Row, Col, southwest, NewRow, NewCol) :- NewRow is Row - 1, NewCol is Col - 1.
new_position(Row, Col, west, NewRow, NewCol) :- NewRow is Row, NewCol is Col - 1.
new_position(Row, Col, northwest, NewRow, NewCol) :- NewRow is Row + 1, NewCol is Col - 1.


% Check if the move is valid for non-capturing moves
is_valid_non_capturing_move(Board, _, Row, Col) :-
    within_board(Row, Col),
    \+ position_occupied(Board, Row, Col).  % Move must be to an empty square


% Check if the move is valid for capturing moves
is_valid_move(Board, Player, Row, Col, Direction) :-
    player_capturing_moves(Player, CapturingMoves),
    member(Direction, CapturingMoves),
    within_board(Row, Col),
    opposite_player(Player, Opponent),
    piece_position(Board, Opponent, Row, Col).


% Check if a piece is on a given square
position_occupied(Board, Row, Col) :-
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Piece),
    Piece \== empty.


% Move is within board boundaries
within_board(Row, Col) :-
    Row >= 1, Row =< 8, Col >= 1, Col =< 8.


% Determine the opposite player
opposite_player(black, white).
opposite_player(white, black).


% Check if a square has a piece of a given player (or empty)
piece_position(Board, Player, Row, Col) :-
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Piece),
    Piece = Player.


% Define dynamic capturing and non-capturing moves based on the player
player_non_capturing_moves(black, Moves) :-
    black_non_capturing(Moves).
player_non_capturing_moves(white, Moves) :-
    white_non_capturing(Moves).

player_capturing_moves(black, Moves) :-
    black_capturing(Moves).
player_capturing_moves(white, Moves) :-
    white_capturing(Moves).