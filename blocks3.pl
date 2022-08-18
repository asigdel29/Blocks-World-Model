%%%%%%%%%%%%%
%%% Model %%%
%%%%%%%%%%%%%

%% Facts %%

block(b1).
block(b2).
block(b3).
block(b4).
block(b5).
block(b6).
block(b7).
block(b8).

size(b1, large).
size(b2, large).
size(b3, large).
size(b4, large).
size(b5, small).
size(b6, small).
size(b7, small).
size(b8, small).

color(b1, yellow).
color(b2, red).
color(b3, blue).
color(b4, green).
color(b5, yellow).
color(b6, red).
color(b7, blue).
color(b8, green).

initial_state([on(b1, table), on(b2, table), on(b3, table), on(b4, table), on(b5, table),
    on(b6, table), on(b7, table), on(b8, table)]).
    
final_state([on(b1, table), on(b2, b3), on(b3, b1), on(b4, b5), on(b5, b6), on(b6, table), on(b7, b4), on(b8, b7)]).

%%%%%%%%%%%%%
%%% Rules %%%
%%%%%%%%%%%%%
    
clear(Block, State) :- \+ member(on(_, Block), State).
empty_hand(State) :- \+ member(hand(_), State).
holding(State, Block) :- member(hand(Block), State).
is_on(Block1, Block2, State) :- member(on(Block1, Block2), State).

validate(pickup, Block, State) :- clear(Block, State), empty_hand(State).
validate(pickup, Block1, Block2, State) :- clear(Block1, State), is_on(Block1, Block2, State),
    (empty_hand(State)).
validate(put, Block1, Block2, State) :- clear(Block1, State), clear(Block2, State),
    (empty_hand(State) ; holding(State, Block1)).
validate(put, Block, table) :- clear(Block, State), 
    (empty_hand(State) ; holding(State, Block)).
validate(stack, Block1, Block2, State) :- clear(Block1, State), clear(Block2, State),
    (empty_hand(State) ; holding(State, Block1)).
validate(unstack, Block, State) :- clear(Block, State), empty_hand(State).
validate(unstack, Block1, Block2, State) :- clear(Block1, State), is_on(Block1, Block2, State),
    (empty_hand(State)).
    
perform(pickup, Block, State, NewState) :- replace(on(Block,_), hand(Block), State, NewState).
perform(put, Block1, Target, State, NewState) :- perform(pickup, Block1, State, State1),
    replace(hand(Block1), on(Block1, Target), State1, NewState).
perform(stack, Block1, Target, State, NewState) :- perform(pickup, Block1, State, State1),
    replace(hand(Block1), on(Block1, Target), State1, NewState).
perform(unstack, Block, State, NewState) :- replace(on(Block,_), hand(Block), State, NewState).

%%%%%%%%%%%
%%% NLP %%%
%%%%%%%%%%%

%% Parsing %%

sentence(s(VP)) --> verb_phrase(VP).
sentence(s(VP)) --> verb_phrase(VP), dot.
noun_phrase(np(Det, Noun)) --> det(Det), noun(Noun).
noun_phrase(np(Det, Adjp, Noun)) --> det(Det), adj_phrase(Adjp), noun(Noun).
verb_phrase(vp(Verb, NP)) --> verb(Verb), noun_phrase(NP).
verb_phrase(vp(Verb, NP, PP)) --> verb(Verb), noun_phrase(NP), prep_phrase(PP).
prep_phrase(pp(Prep, NP)) --> prep(Prep), noun_phrase(NP).
adj_phrase(adjp(Adj)) --> adj(Adj).
adj_phrase(adjp(Adj, Adjp)) --> adj(Adj), adj_phrase(Adjp).
verb(v(put)) --> [put].
verb(v(pickup)) --> [pickup].
verb(v(stack)) --> [stack].
verb(v(unstack)) --> [unstack].
det(d(the)) --> [the].
adj(a(large)) --> [large].
adj(a(small)) --> [small].
adj(a(green)) --> [green].
adj(a(red)) --> [red].
adj(a(yellow)) --> [yellow].
adj(a(blue)) --> [blue].
noun(n(block)) --> [block].
noun(n(table)) --> [table].
prep(p(on)) --> [on].
prep(p(from)) --> [from].
dot --> ['.'].

%% Extracting Verbs and NPs %%

extract(s(vp(v(Verb), NounPhrase)), Verb, NounPhrase).
extract(s(vp(v(Verb), NounPhrase1, pp(_, NounPhrase2))), Verb, NounPhrase1, NounPhrase2).

%% Reference Resolution %%

%% 2-adjective
resolve_ref(NP, _, Block) :- NP = np(_, adjp(a(Size), adjp(a(Color))), n(block)), 
    size(Block, Size), color(Block, Color).
resolve_ref(NP, _, Block) :- NP = np(_, adjp(a(Color), adjp(a(Size))), n(block)), 
    size(Block, Size), color(Block, Color).

%% Removed clear(Block, State) rule for Task 2 to work
%% 1-adjective 
resolve_ref(NP, State, Block) :- NP = np(_, adjp(a(Adjective)), n(block)), 
    member(on(Block, _), State),
    (findall(Block, size(Block, Adjective), [Block]) ; 
     findall(Block, color(Block, Adjective), [Block])).

%% 0-adjective  
resolve_ref(NP, State, Block) :- NP = np(d(the), n(block)), member(hand(Block), State).
resolve_ref(NP, _, table) :- NP = np(d(the), n(table)).

%% Task 2
%% resolve_ref2 rule are used when dealing with only one noun phrase (NP).
%% resolve_ref2 rule has 1-adjective state removed to deal with ambiguous noun phrases
resolve_ref2(NP, _, Block) :- NP = np(_, adjp(a(Size), adjp(a(Color))), n(block)), 
    size(Block, Size), color(Block, Color).
resolve_ref2(NP, _, Block) :- NP = np(_, adjp(a(Color), adjp(a(Size))), n(block)), 
    size(Block, Size), color(Block, Color).

resolve_ref2(NP, State, Block) :- NP = np(d(the), n(block)), member(hand(Block), State).
resolve_ref2(NP, _, table) :- NP = np(d(the), n(table)).

%% Natural Language Generation %%

nlg(Block) :- size(Block, S), color(Block, C), write("the "), write(S), write(" "), 
    write(C), write(" block"). 
nlg(table) :- write("the table"). 
nlg(on(Block, Target)) :- nlg(Block), write(" is on "), nlg(Target), nl.
nlg(hand(Block)) :- nlg(Block), write(" is in the hand "), nl.

nlg([StateItem]) :- nlg(StateItem).
nlg([StateItem|Rest]) :- nlg(StateItem), nlg(Rest).

%%%%%%%%%%%%%%%%%
%%% Main Loop %%%
%%%%%%%%%%%%%%%%%

perform_act(Parse, State, NewState) :- extract(Parse, Verb, NP), 
    resolve_ref2(NP, State, Block), 
    validate(Verb, Block, State), 
    perform(Verb, Block, State, NewState).
perform_act(Parse, State, NewState) :- extract(Parse, Verb, NP1, NP2), 
    resolve_ref(NP1, State, Block1), 
    resolve_ref(NP2, State, Block2), 
    validate(Verb, Block1, Block2, State), 
    (perform(Verb, Block1, State, NewState) ; perform(Verb, Block1, Block2, State, NewState)).

%% Modified/Added rules for Task 1 %%

%% resolution_check rule identifies reference resolution error
resolution_check(Parse, State) :- \+perform_act(Parse, State, _),
    write("Reference resolution error. Try again."), 
    nl, !, talktome(State).

resolution_check(Parse, State) :- perform_act(Parse, State, NewState), 
    write("Currently..."), nl,
    nlg(NewState), 
    !, talktome(NewState).

%% parse_check rule identifies parsing error
parse_check(State, Input) :- \+sentence(_, Input, []), 
    write("Parsing error. Try again."), 
    nl, !, talktome(State).

parse_check(State, Input) :- sentence(Parse, Input, []),
    resolution_check(Parse, State).

%% modified process_input rule with added 'exit' command that ends the talktome rule
process_input(State) :- read_word_list(Input), 
    (Input \== [exit]) -> parse_check(State, Input);
    !, fail.

talktome :- initial_state(State), write("Currently..."), nl, nlg(State), !, talktome(State).
talktome(State) :- write("What would you like to do next?"), nl, process_input(State).
        
%%%%%%%%%%%%%%%%
%%% Planning %%%
%%%%%%%%%%%%%%%%

plan(CurrentState, CurrentState, _, []).

%% rule to perform stacking
plan(CurrentState, GoalState, CurrentBlock, Steps) :- check_plan(CurrentState, GoalState, CurrentBlock, StackBlock, NextCurrentBlock), 
    clear(NextCurrentBlock, CurrentState), 
    validate(stack, StackBlock, NextCurrentBlock, CurrentState),
    perform(stack, StackBlock, NextCurrentBlock, CurrentState, NextState), !,
    plan(NextState, GoalState, StackBlock, Steps1), Steps = [putOn(StackBlock,NextCurrentBlock)|Steps1].

%% If no block found on top of block 
    check_plan(CurrentState, GoalState, CurrentBlock, StackBlock, NextCurrentBlock) :- \+is_on(_, CurrentBlock, GoalState),
    is_on(StackBlock, NextCurrentBlock, GoalState),
    \+member(on(StackBlock, NextCurrentBlock), CurrentState),
    member(on(NextCurrentBlock, table), GoalState).


check_plan(_, GoalState, CurrentBlock, StackBlock, CurrentBlock) :- is_on(StackBlock, CurrentBlock, GoalState).

%% rule find the top block of stack
get_first_block(GoalState, CurrentBlock) :- block(CurrentBlock), \+is_on(_, CurrentBlock, GoalState).

%% simple solve rule to find the steps from initial state to final state
solve(Steps) :- initial_state(CurrentState), final_state(GoalState), get_first(GoalState, CurrentBlock), plan(CurrentState, GoalState, CurrentBlock, Steps).


% Utilities

replace(_, _, [], []).
replace(O, R, [O|T], [R|T2]) :- replace(O, R, T, T2).
replace(O, R, [H|T], [H|T2]) :- dif(H,O), replace(O, R, T, T2).

find_in(Pattern, [H|_], Result) :- Pattern = H, Result = H.
find_in(Pattern, [_|R], Result) :- find_in(Pattern, R, Result).
find_in(_, [], _) :- fail.

read_word_list(Ws) :-
    read_line_to_codes(user_input, Cs),
    atom_codes(A, Cs),
    tokenize_atom(A, Ws).