:- include(words).
:- use_module(library(dif)). % Add import statement for dif/2 predicate

guessedWords([]).

% States:
%   - mandatory letter
% MandatoryLetter(X).
%   - available letters
%   - list of words found
%   - number of words guessed 

% INIT FUNCTION
% - Check if player has won the game
% - Init random day
% - Init list of words guessed 
% - Display stuff

%initGame(Ans) :-
%    write("Welcome to ProloggingBee! Inspired by the NYTimes hit game Spelling Bee...but in Prolog!"),
%    nl,
%    random_between(0,1,CurrentDay), % get random day
%    play(CurrentDay).
    

play(CurrentDay) :-
    write("Ask me: "), flush_output(current_output), 
    read_line_to_string(user_input, St),
    notin(St, ["quit", "quit.", "q", "q."]), % quit or q ends interaction
    split_string(St, " -", " ,?.!-", Ln), % ignore punctuation
    random_between(0,1,CurrentDay), % get random day
    printGrid(CurrentDay),
    (checkWordValidGuess(St, CurrentDay) ->
    write("No more answers\n").

% IMPLEMENT FUNCTION to retrieve definitions

printGrid(Day) :-
    mandatoryLetter(Day, MandatoryLetter),
    availableLetters(Day, AvailableLetters),
    exclude(=(MandatoryLetter), AvailableLetters, NonMandatoryLetters),
    nth0(0, NonMandatoryLetters, Zeroth),
    nth0(1, NonMandatoryLetters, First),
    nth0(2, NonMandatoryLetters, Second),
    nth0(3, NonMandatoryLetters, Third),
    nth0(4, NonMandatoryLetters, Fourth),
    nth0(5, NonMandatoryLetters, Fifth),
    write(Zeroth), nl,
    write(First), nl,
    write(Second), nl,
    write(MandatoryLetter), nl,
    write(Third), nl,
    write(Fourth), nl,
    write(Fifth), nl,
    write(Sixth), nl.


% notin(E,L) is true if E is not in list L. Allows for E or elements of L to be variables.
% from lecture file geography_QA.pl
notin(_,[]).
notin(E,[H|T]) :-
    dif(E,H),
    notin(E,T).

checkLength(Word) :-
    string_length(Word, Length),
    Length >= 3.

% moduralize this so easier to test 
% Check if word length >= 3
checkLength(Word) :-
    string_length(Word, Length),
    Length >= 3.

checkLength(Word) :-
    string_length(Word, Length),
    Length < 3,
    write("not long enough bruh!"),
    false.

checkContainsMandatoryLetter(Word, MandatoryLetter) :-
    \+ sub_string(Word, _, 1, _, MandatoryLetter),
    write("does not contain mandatory letter"),
    false.    
    
% Check if word contains mandatory letter
checkContainsMandatoryLetter(Word, MandatoryLetter) :-
    sub_string(Word, _, 1, _, MandatoryLetter).


checkWordHasOnlyAvailableLetters([], _).

checkWordHasOnlyAvailableLetters("", _).

checkWordHasOnlyAvailableLetters([Head | Tail], AvailableLetters) :-
    (member(Head, AvailableLetters) -> 
    checkWordHasOnlyAvailableLetters(Tail, AvailableLetters) ; 
    write("Word contains unavailable letters bruh"),
    false).

checkWordIsAWord(Word, Day) :-
    word(Day, Word).

checkWordIsAWord(Word, Day) :-
    \+ word(Day, Word),
    write("not a real word bruh"),
    false.


% Word is of type String
% checkWordValidGuess(bruh, [b, r, u, h], b).
% https://stackoverflow.com/questions/14541164/knowing-when-to-use-cut-in-prolog reference for "once"
checkWordValidGuess(Word, Day) :-
    mandatoryLetter(Day, MandatoryLetter),
    availableLetters(Day, AvailableLetters),
    once(checkLength(Word)), % only check length once
    once(checkContainsMandatoryLetter(Word, MandatoryLetter)), % only check mandatory letters once
    string_chars(Word, Chars),
    once(checkWordHasOnlyAvailableLetters(Chars, AvailableLetters)), % only check if word is using available letters once
    checkWordIsAWord(Word, Day).
    
