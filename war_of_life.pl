:- use_module(library(lists)).
:- use_module(library(random)).

/********************/
/* Playing the Game */
/********************/

play(Showboard, FirstPlayerStrategy, 
     SecondPlayerStrategy, NumberOfMoves, WinningPlayer) :-
  start_config(random, Board),
  ((Showboard==verbose, write('Initial State:\n\n'));Showboard==quiet),
  draw_board(Showboard, Board),
  show_score(Showboard, Board), !,
  make_move(Showboard, 0, 'b', FirstPlayerStrategy, 'r', SecondPlayerStrategy,
	    Board, _, NumberOfMoves, WinningPlayer), !.

alter_board([A,B,MA,MB], Alives, NewAlives) :-
	nth1(Pos, Alives, [A,B]),
	PP1 is Pos - 1,
	length(LH, PP1),
	append(LH,RHS,Alives),
	RHS = [_|RH],
	append(LH, [[MA,MB]], NAB),
	append(NAB, RH, NewAlives).
  
make_move(Showboard, 250, _, _, _, _, _, _, 250, 'exhaust') :-
  show_winner(Showboard, 'Exhaust', 250).
make_move(Showboard, Num, _, _, _, _, [[],[]], [[],[]], Num, 'draw') :-
  show_winner(Showboard, 'Draw', Num).
make_move(Showboard, Num, _, _, _, _, [[],AliveReds], [[],AliveReds], Num, 'r') :-
  show_winner(Showboard, 'Red', Num).
make_move(Showboard, Num, _, _, _, _, [AliveBlues, []], [AliveBlues, []], Num, 'b') :-
  show_winner(Showboard, 'Blue', Num).
make_move(Showboard, Num, PlayerLetter, Strategy, 
	  NextPlayerLetter, NextStrategy, Board, FinalBoard, PassOn, PassOn2) :-
  NP1 is Num + 1,
  move_piece(PlayerLetter, Strategy, Board, NewBoard, Move),
  show_move(Showboard, NP1, PlayerLetter, Move),
  draw_board(Showboard, NewBoard),
  next_generation(NewBoard, NextGenerationBoard),
  draw_board(Showboard, NextGenerationBoard),
  show_score(Showboard, NextGenerationBoard), 
  make_move(Showboard, NP1, NextPlayerLetter, NextStrategy, PlayerLetter, 
	    Strategy, NextGenerationBoard, FinalBoard, PassOn, PassOn2).
make_move(Showboard, Num, _, _, _, _, _, _, Num, 'stalemate') :-
  show_winner(Showboard, 'Stalemate', Num).

/******************************/
/* The "Random Move" Strategy */
/******************************/

random_move(Alive, OtherPlayerAlive, Move) :-
	findall([A,B,MA,MB],(member([A,B], Alive), neighbour_position(A,B,[MA,MB]),
	                     \+member([MA,MB],Alive), \+member([MA,MB],OtherPlayerAlive)),PossMoves),
	length(PossMoves,L),
	LP1 is L + 1,
	random(1,LP1,Pos),
	nth1(Pos,PossMoves,Move).

move_piece('b', random, [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
	random_move(AliveBlues, AliveReds, Move),
	alter_board(Move, AliveBlues, NewAliveBlues).
move_piece('r', random, [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
	random_move(AliveReds, AliveBlues, Move),
	alter_board(Move, AliveReds, NewAliveReds).

/******************************/
/* Support for the strategies */
/******************************/

move_piece(PieceColour, bloodlust, Board, NewBoard, Move) :-
  bloodlust(PieceColour, Board, NewBoard, Move).

move_piece(PieceColour, self_preservation, Board, NewBoard, Move) :-
  self_preservation(PieceColour, Board, NewBoard, Move).

move_piece(PieceColour, land_grab, Board, NewBoard, Move) :-
  land_grab(PieceColour, Board, NewBoard, Move).

move_piece(PieceColour, safety_in_numbers, Board, NewBoard, Move) :-
  safety_in_numbers(PieceColour, Board, NewBoard, Move).

move_piece(PieceColour, minimax, Board, NewBoard, Move) :-
  minimax(PieceColour, Board, NewBoard, Move).

move_piece(PieceColour, divide_and_conquer, Board, NewBoard, Move) :-
  divide_and_conquer(PieceColour, Board, NewBoard, Move).

/***************************/
/* Starting Configurations */
/***************************/

start_config(Board) :-
  start_config(random, Board).
start_config(random, [FBs, FRs]) :-
  get_random_pairs([],[],FBs),
  get_random_pairs([],FBs,FRs).
start_config(cross, [[[1,1],[2,2],[3,3],[4,4],[5,5],[6,6],[7,7],[8,8]],
		     [[1,8],[2,7],[3,6],[4,5],[5,4],[6,3],[7,2],[8,1]]]).
start_config(checkers, [[[3,1],[4,2],[3,3],[4,4],[3,5],[4,6],[3,7],[4,8]],
			[[5,1],[6,2],[5,3],[6,4],[5,5],[6,6],[5,7],[6,8]]]).
start_config(gliders, [[[2,1],[3,2],[3,3],[1,3],[2,3]], 
		       [[6,6],[6,7],[6,8],[7,6],[8,7]]]).

start_config(X,X) :- ground(X).

get_random_pairs(Current, _, Current) :-
  length(Current, 12).
get_random_pairs(Current, Dont, Final) :-
  random(1,9,A),
  random(1,9,B), 
  \+ member([A,B],Current),
  \+ member([A,B],Dont),
  append(Current,[[A,B]],PassOn),
  get_random_pairs(PassOn,Dont,Final).

get_random_pairs(Current, Dont, Final) :-
  get_random_pairs(Current, Dont, Final).

/*******************/
/* Next Generation */
/*******************/

next_generation(Board, [NewAliveBlues, NewAliveReds]) :-
  findall([A,B,NewW],
	  (
	   cell(A,B), 
	   what_in_cell(Board, A, B, W), 
	   change_cell(Board, A, B, W, NewW)
	  ),
	  ABWs),
  findall([A,B], member([A,B,b], ABWs), NewAliveBlues),
  findall([A,B], member([A,B,r], ABWs), NewAliveReds).

what_in_cell([AliveBlues, _], A, B, 'b') :-
  member([A,B], AliveBlues).
what_in_cell([_, AliveReds], A, B, 'r') :-
  member([A,B], AliveReds).

what_in_cell([AliveBlues, AliveReds], A, B, ' ') :-
  \+ member([A,B], AliveBlues), 
  \+ member([A,B], AliveReds).

change_cell([AliveBlues, AliveReds], A, B, W, NewW) :-
  findall(b,(neighbour_position(A,B,[NA,NB]), member([NA,NB], AliveBlues)),Bs),
  findall(r,(neighbour_position(A,B,[NA,NB]), member([NA,NB], AliveReds)),Rs),
  length(Bs,BL), length(Rs,RL),
  populate_cell(BL,RL,W,NewW), !.

neighbour_position(A,B,[I,J]) :-
  AM1 is A - 1, AP1 is A + 1, BM1 is B - 1, BP1 is B + 1,
  L = [AM1,A,AP1], K = [BM1,B,BP1],
  member(I,L), member(J,K),
  \+ (I==A, J==B), \+ I == 0, \+ J == 0, \+ I > 8, \+ J > 8.

populate_cell(3,0,' ',b).
populate_cell(0,3,' ',r).
populate_cell(2,1,' ',b).
populate_cell(1,2,' ',r).
populate_cell(NumBlues,NumReds,X,X) :-
   2 is NumBlues + NumReds.
populate_cell(NumBlues,NumReds,X,X) :-
   3 is NumBlues + NumReds.
populate_cell(_,_,_,' ').

/*************************/
/* Showing Board States  */
/* And Other Information */
/*************************/

cell(A,B) :-
  L = [1,2,3,4,5,6,7,8],
  member(A,L),
  member(B,L).

draw_board(Board) :- draw_board(verbose, Board).

draw_board(quiet, _) :- !.
draw_board(verbose, Board) :-
  write('  12345678\n'),
  write(' +--------+'),
  findall(_,(cell(A,B),draw_newline(A,B),draw_cell(A,B,Board),draw_endline(B)),_),
  write('\n +--------+\n\n').

draw_cell(A,B,[AliveBlues, _]) :-
  member([A,B],AliveBlues),
  write('b'), !.
draw_cell(A,B,[_, AliveReds]) :-
  member([A,B],AliveReds),
  write('r'), !.
draw_cell(_,_,[_, _]) :-
  write(' ').

draw_newline(A,1) :-
  write('\n'), write(A), write('|').
draw_newline(_,X) :- 
  X=\=1.

draw_endline(8) :-
  write('|').
draw_endline(X) :- 
  X=\=8.

show_score(quiet, _) :- !.
show_score(verbose, [AliveBlues, AliveReds]) :-
  length(AliveBlues,BL),
  length(AliveReds,RL),
  write('\nblue score = '), write(BL), nl, 
  write('red score = '), write(RL), write('\n\n').

show_move(quiet, _, _, _) :- !.
show_move(verbose, Num, PlayerLetter, Move) :-
  write(Num), write('. '), write(PlayerLetter), write(' moves '), write(Move), nl, nl.

show_winner(quiet, _, _) :- !.
show_winner(verbose, 'Exhaust', Num) :-
  write('Game is drawn due to exhaustion after '), write(Num), write(' moves!\n\n').
show_winner(verbose, 'Draw', Num) :-
  write('Game is drawn after '), write(Num), write(' moves!\n\n').
show_winner(verbose, Winner, Num) :-
  write(Winner), write(' wins after '), write(Num), write(' moves!\n\n').
