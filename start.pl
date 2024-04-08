:- dynamic guessedWord/1.
:- include(words).
:- use_module(library(dif)). % Add import statement for dif/2 predicate

%:- discontiguous mandatoryLetter/2.
%:- discontiguous availableLetters/2.
% :- discontiguous word/2.
:-style_check(-discontiguous).
% referred to https://github.students.cs.ubc.ca/linky98/Loldle for state management
guessedWord("").
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

initGame(Ans) :-
    retractall(guessedWord(_)),
    write("Welcome to ProloggingBee! Inspired by the NYTimes hit game Spelling Bee...but in Prolog!"),
    nl,
    random_between(0,1,CurrentDay), % get random day
    play(CurrentDay).
    

play(CurrentDay) :-
    % printGuessedWords, nl,
    printGrid(CurrentDay),
    write("Guess A Word: "), flush_output(current_output), 
    read(St),
    notin(St, [quit, quit, q, q]), % quit or q ends interaction
    (checkWordValidGuess(St, CurrentDay) ->
    write("Good Guess Broski\n"), 
    assert(guessedWord(St)),
    countGuessedWords(NumGuessedWords),
    printRanking(CurrentDay, NumGuessedWords), write(NumGuessedWords), nl,
    checkIfWon(Day, NumGuessedWords),
    play(CurrentDay) ; play(CurrentDay)).

% IMPLEMENT FUNCTION to retrieve definitions (1 to guess word, 2 to get definition of already guessed word, and q to quit)

countGuessedWords(NumGuessedWords) :- 
    findall(_, guessedWord(_), GuessedWords),
    length(GuessedWords, NumGuessedWords).

checkIfWon(Day, NumGuessedWords) :-
    numWords(Day, NumWordsNeeded),
    NumGuessedWords \= NumWordsNeeded.

checkIfWon(Day, NumGuessedWords) :-
    numWords(Day, NumWordsNeeded),
    NumGuessedWords == NumWordsNeeded,
    write("Congratulations! You have guessed all of the possible words."), nl,
    write("Thanks for playing bruh"),
    halt(0). 

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
    write("      "), writeGrey(Zeroth), nl,
    write("   "), writeGrey(First), write("     "), writeGrey(Second),  nl,
    write("      "), write('\e[43m'), write(MandatoryLetter), write('\e[0m'), nl,
    write("   "), writeGrey(Third), write("     "), writeGrey(Fourth), nl,
    write("      "), writeGrey(Fifth), nl.


printGuessedWords :- 
    write("Words Guessed: "),
    forall(guessedWord(Word), (printtGuessedWord(Word)).

%printGuessedWord(Word) :-
%   write('\e[43m]'),
%   write(Word),
%   write('\e[43m]'),
%   nl,
%   write("------------")
%   nl.


writeGrey(ToWrite) :-
    write('\e[100m'),
    write('\e[30m'),
    write(ToWrite),
    write('\e[0m').


% notin(E,L) is true if E is not in list L. Allows for E or elements of L to be variables.
% from lecture file geography_QA.pl
notin(_,[]).
notin(E,[H|T]) :-
    dif(E,H),
    notin(E,T).

% this function prints the ranking of the player based on how many words they have gotten

% found less than 25% of words
printRanking(Day, NumWordsGuessed) :-
    numWords(Day, TotalWords),
    Result is NumWordsGuessed / TotalWords,
    Quartile is Result * 100,
    Quartile =< 25,
    write("Rank: Good").

% found more than 25 of words but less than 50%
printRanking(Day, NumWordsGuessed) :-
    numWords(Day, TotalWords),
    Result is NumWordsGuessed / TotalWords,
    Quartile is Result * 100,
    Quartile > 25,
    Quartile =< 50,
    write("Rank: Great").
% found more than 50% of words but less than 75%
printRanking(Day, NumWordsGuessed) :-
    numWords(Day, TotalWords),
    Result is NumWordsGuessed / TotalWords,
    Quartile is Result * 100,
    Quartile > 50,
    Quartile =< 75,
    write("Rank: Amazing").
% found more than 75% of words but less than 100%
printRanking(Day, NumWordsGuessed) :-
    numWords(Day, TotalWords),
    Result is NumWordsGuessed / TotalWords,
    Quartile is Result * 100,
    Quartile > 75,
    Quartile =< 100,
    write("Rank: The Boss").
    
% Checking functionality

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

checkWordAlreadyGuessed(Word) :-
    \+ guessedWord(Word).

checkWordAlreadyGuessed(Word) :-
    guessedWord(Word),
    write("Word already guessed bruh"),
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
    checkWordIsAWord(Word, Day),
    checkWordAlreadyGuessed(Word).
    
