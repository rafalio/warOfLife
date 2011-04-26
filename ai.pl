:- ensure_loaded( 'war_of_life.pl' ).

test_strategy( NGames, P1Strategy, P2Strategy) :-
    write('----------------------------------------------'), write('\n'),
    write('P1 using: '), write(P1Strategy), write('  | P2 using: '), write(P2Strategy),write('\n'),
    statistics(runtime, [T0|_]),
    test_strategy(NGames, P1Strategy, P2Strategy, Results, Games),
    statistics(runtime, [T1|_]),
    T is T1 - T0,                   % Execution time
    TAvg is T/NGames,
    %max_list(Games,Max), % For swipl
    %min_list(Games,Min),
    delete(Games,250,PGames),
    max_member(Max,PGames), % Pruned games, exclude exhaustion
    min_member(Min,Games),
    sumlist(Games,SumGames), length(Games,GamesLength),
    AvgGames is SumGames/GamesLength,
    findall(B, nth1(I,Results,r), Red), length(Red,RedWins),
    findall(B, nth1(I,Results,b), Blue), length(Blue,BlueWins),
    findall(B, nth1(I,Results,draw), Draws), length(Draws,DrawWins),
    Stalemates is (NGames - RedWins - BlueWins - DrawWins),
    write('Number of wins for player 1 (blue):'), write(BlueWins), write('\n'),
    write('Number of wins for player 2 (red):'), write(RedWins), write('\n'),
    write('Number of draws:'), write(DrawWins), write('\n'),
    write('Number of stalemates:'), write(Stalemates), write('\n'),    
    write('Longest (non-exhaustive) game: '), write(Max), write('\n'),
    write('Shortest Game: '), write(Min), write('\n'),
    write('Average Game Length (including exhaustives): '), write(AvgGames), write('\n'),
    format('Average game time: ~3f sec.~n', [TAvg/1000]).

test_strategy( 0, _, _, [], []) :- !.
test_strategy( NGames, P1s, P2s, Results, Games) :-
    play(quiet,P1s,P2s,NumMoves,Winner),
    N is NGames - 1,
    test_strategy(N,P1s,P2s,R,G),
    append(R,[Winner],Results),
    append(G,[NumMoves],Games).

test_strat(S1,S2,N) :- test_strategy(1000,S1,S2).

tester(N) :-
    map_product(
        test_strat,
        [random,land_grab,self_preservation,bloodlust,minimax],
        [random,land_grab,self_preservation,bloodlust,minimax],
        Res).

% Opponents

opp(r,b).
opp(b,r).

% Give numeric representation to extract out of lists easier

num(b,1).
num(r,2).

% True when position (A,B) is empty under the state [AB,AR]

empty(A,B,[AB,AR]) :-
    \+ member([A,B],AB),
    \+ member([A,B],AR).


% Possible Moves for player P given state State

poss_moves(P,State,Moves) :-
    findall( 
        [A,B,A1,B1],
        ( cell(A,B),
          what_in_cell(State,A,B,P),
          neighbour_position(A,B,[A1,B1]),
          empty(A1,B1,State)
        ),
        Moves).

% Applies the move (A,B) -> (A1,B1)
% Pre: Move must be valid

do_move([A,B,A1,B1],[Blue,Red],[NewBlue,NewRed]) :-
    (
        member([A,B],Blue),
        delete(Blue,[A,B],PartialBlue),
        append([[A1,B1]],PartialBlue,NewBlue),
        NewRed = Red, !
    ) ;
    (
        member([A,B],Red),
        delete(Red,[A,B],PartialRed),
        append([[A1,B1]],PartialRed,NewRed),
        NewBlue = Blue, !
    ).

% Predicate for comparing two lists

predmin([OpPieces, K, V],[OpPieces1,K1, V1]) :-
    OpPieces < OpPieces1.


% Bloodlust Strategy
% Minimize number of opponents pieces

bloodlust(P, [Blue,Red], [NewBlue,NewRed], [X,Y,X1,Y1]) :-
    opp(P,Opp),
    num(Opp,OpNum),
    poss_moves(P,[Blue,Red],Moves),
    findall(
        [OpPieces, [A,B,A1,B1],AfterCrankState],
        (  member([A,B,A1,B1],Moves),
           do_move([A,B,A1,B1], [Blue,Red], NewState),
           next_generation(NewState,AfterCrankState),
           nth1(OpNum,AfterCrankState,OPStates),
           length(OPStates,OpPieces)
        ),
        MoveStateList),
    min_member(predmin,Min,MoveStateList),
    nth1(2,Min,[X,Y,X1,Y1]),
    do_move([X,Y,X1,Y1], [Blue,Red], [NewBlue,NewRed]).
    

% Self Preservation Strategy
% Maximize number of players pieces

self_preservation(P, [Blue,Red], [NewBlue,NewRed], [X,Y,X1,Y1]) :-
    num(P,PNum),
    poss_moves(P,[Blue,Red],Moves),
    findall(
        [PPieces, [A,B,A1,B1],AfterCrankState],
        (  member([A,B,A1,B1],Moves),
           do_move([A,B,A1,B1], [Blue,Red], NewState),
           next_generation(NewState,AfterCrankState),
           nth1(PNum,AfterCrankState,PStates),
           length(PStates,PPieces)
        ),
        MoveStateList),
    max_member(predmin,Max,MoveStateList),
    nth1(2,Max,[X,Y,X1,Y1]),
    do_move([X,Y,X1,Y1], [Blue,Red], [NewBlue,NewRed]).
   
   
% Land Grab strategy
% Maximizes the function (Number of Players pieces – Number of Opponents pieces)
 
land_grab(P, [Blue,Red], [NewBlue,NewRed], [X,Y,X1,Y1]) :-
    findall(
        [Score, [A,B,A1,B1],AfterCrankState],
        (gen_moves(P, [Blue,Red], Score, [A,B,A1,B1], AfterCrankState)),
        MoveStateList),
    max_member(predmin,Max,MoveStateList),
    nth1(2,Max,[X,Y,X1,Y1]),
    do_move([X,Y,X1,Y1], [Blue,Red], [NewBlue,NewRed]).
    

% Move generator for land_grab strategy.

gen_moves(P, [Blue,Red], Score, [A,B,A1,B1], AfterCrankState) :-
    opp(P,Opp),
    num(P,PNum),
    num(Opp,OpNum),
    poss_moves(P,[Blue,Red],Moves),
    member([A,B,A1,B1],Moves),
    do_move([A,B,A1,B1], [Blue,Red], NewState),
    next_generation(NewState,AfterCrankState),
    nth1(PNum,AfterCrankState,PStates),
    length(PStates,PPieces),
    nth1(OpNum,AfterCrankState,OpStates),
    length(OpStates,OpPieces),
    Score is PPieces - OpPieces.
 
minimax(P, CurState, NextState, TheMove) :-
    pval(P,PVal),
    minimax(2,CurState,PVal,_,TheMove),
    %write('The move is: '),write(TheMove), write('\n'),
    do_move(TheMove,CurState,NextState).



% Numeric values for players, used in the minimax algorithm

pval(b,1).
pval(r,-1).

%  The score for current state [Blue,Red] using the landgrab heuristic,
%  from the point of view of player 1.

value([Blue,Red],V) :-
    length(Blue,BLen),
    length(Red,RLen),
    V is BLen - RLen.

% Citation: I have taken inspiration and modified an algorithm found here
% www.it.uu.se/edu/course/homepage/logpro/ht09/Alphabeta+othello.ppt

minimax(0, State, Player, Value, _) :- 
    value(State, V),
    Value is V*Player.      % Value is from the current player’s perspective.
    
minimax(D, State, Player, Value, Move) :-
    D > 0, 
    D1 is D - 1,
    pval(P,Player),
    poss_moves(P,State,Moves),          % All possible moves for P in state State
    minimax(Moves, State, D1, Player, -1000, nil, Value, Move).
 
%  minimax(+Moves,+Position,+Depth,+Player,+Value0,+Move0,-BestValue,-BestMove)
%  Chooses the Best move from the list of Moves from the current Position
%  using the minimax algorithm searching Depth ply ahead.
%  Player indicates if we are currently minimizing (-1) or maximizing (1).
%  Move0 records the best move found so far and Value0 its value.
%  Sometimes this version of minimax is referred to as negamax, due to using
%  the fact that max(a, b) = -min(-a, -b).

minimax([], _, _, _, Value, Best, Value, Best).
minimax([Move|Moves],State,D,Player, Value0,Move0,BestValue,BestMove):-
      do_move(Move, State, NextState),                   % Do the move
      next_generation(NextState,CrankState),             % Turn the crank
      Opponent is -Player,
      minimax(D, CrankState, Opponent, OppValue, _OppMove), 
      Value is -OppValue,
      ( Value > Value0 ->        
        minimax(Moves,State,D,Player, Value ,Move ,BestValue,BestMove)
      ; minimax(Moves,State,D,Player, Value0,Move0,BestValue,BestMove)
      ).       