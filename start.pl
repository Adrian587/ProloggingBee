:- dynamic guessedWord/1.
:- include(words).
:- use_module(library(dif)). % Add import statement for dif/2 predicate

:-style_check(-discontiguous).
% referred to https://github.students.cs.ubc.ca/linky98/Loldle for state management
guessedWord("").

% States:
%   - mandatory letter
% MandatoryLetter(X).
%   - available letters
%   - list of words found
%   - number of words guessed 

% initGame
% - Retracts all guessedWords (new empty state)
% - Chooses a random day to select some set of letters
% - Runs play
initGame(Ans) :-
    retractall(guessedWord(_)),
    write("Welcome to ProloggingBee! Inspired by the NYTimes hit game Spelling Bee...but in Prolog!"),
    nl,
    random_between(0,1,CurrentDay), % get random day
    play(CurrentDay).

% play
% - Prints the grid
% - Takes in input
% - Allows user to either guess a word, quit, or print the list of words already guessed
% - If the user guesses a word, then we update the list of words if it is a new and valid guess
% - If the user has met the win condition (guessed all words) then we end the game
play(CurrentDay) :-
    printGrid(CurrentDay),
    write("Guess A Word Using Lowercase Or Type 'print' To See Guesses. Add a '.' at the end of your input: "), flush_output(current_output), 
    read(St),
    notin(St, [quit, quit, q, q]), % quit or q ends interaction
    (St = print -> printGuessedWords, play(CurrentDay) ; 
    (checkWordValidGuess(St, CurrentDay) ->
    write("Good Guess Broski\n"), 
    assert(guessedWord(St)),
    countGuessedWords(NumGuessedWords),
    printRanking(CurrentDay, NumGuessedWords), write(" | Guessed Words: "), write(NumGuessedWords), nl,
    checkIfWon(CurrentDay, NumGuessedWords),
    play(CurrentDay) ; play(CurrentDay))).

% IMPLEMENT FUNCTION to retrieve definitions (1 to guess word, 2 to get definition of already guessed word, and q to quit)

% Counts the number of guessed words and returns it as NumGuessedWords
countGuessedWords(NumGuessedWords) :- 
    findall(_, guessedWord(_), GuessedWords),
    length(GuessedWords, NumGuessedWords).

% Checks if the user has guessed all the words -- if not, do nothing.
checkIfWon(Day, NumGuessedWords) :-
    numWords(Day, NumWordsNeeded),
    NumGuessedWords \= NumWordsNeeded.

% Checks if the user has guessed all the words -- if so, then print a congratulatory message and halt the program. 
checkIfWon(Day, NumGuessedWords) :-
    numWords(Day, NumWordsNeeded),
    NumGuessedWords == NumWordsNeeded,
    write("Congratulations! You have guessed all of the possible words."), nl,
    write("Thanks for playing bruh"), nl,
    halt(0). 

% Prints the grid in the same format that is printed like in NYT Spelling Bee (as a hive, with the mandatory letter in the center)
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

% Helper function for printGrid -- prints a letter with a grey background and white text.
writeGrey(ToWrite) :-
    write('\e[100m'),
    write('\e[30m'),
    write(ToWrite),
    write('\e[0m').

% Prints each of the guessed words
printGuessedWords :- 
  write("Words Guessed: "), nl,
  findall(_, guessedWord(_), GuessedWords),
  forall(guessedWord(Word), (printGuessedWord(Word))),
  write("------------"), nl.

% Helper function for printGuessedWords -- prints each word individually
printGuessedWord(Word) :-
  write(Word), nl.

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

% moduralize this so easier to test 
% Check if word length >= 3
checkLength(Word) :-
    string_length(Word, Length),
    Length >= 3.

% If the word is not longer than 3, than we fail.
checkLength(Word) :-
    string_length(Word, Length),
    Length < 3,
    write("word is not long enough bruh!"), nl,
    false.

% If the word does not contain the mandatory letter, we fail.
checkContainsMandatoryLetter(Word, MandatoryLetter) :-
    \+ sub_string(Word, _, 1, _, MandatoryLetter),
    write("does not contain mandatory letter"), nl,
    false.    
    
% Check if word contains mandatory letter
checkContainsMandatoryLetter(Word, MandatoryLetter) :-
    sub_string(Word, _, 1, _, MandatoryLetter).

% Base case for checkWordHasOnlyAvailableLetters
checkWordHasOnlyAvailableLetters([], _).

% Checks that the word has only letters from the list of available letteres
checkWordHasOnlyAvailableLetters([Head | Tail], AvailableLetters) :-
    (member(Head, AvailableLetters) -> 
    checkWordHasOnlyAvailableLetters(Tail, AvailableLetters) ; 
    write("Word contains unavailable letters bruh"), nl,
    false).

% Checking for the case that the inputted word is a real word
checkWordIsAWord(Word, Day) :-
    word(Day, Word).

% Checking for the case that the inputted word is not a real word
checkWordIsAWord(Word, Day) :-
    \+ word(Day, Word),
    write("not a real word bruh"), nl,
    false.

% Checking for the case where the inputted word has not been guessed yet by the user
checkWordAlreadyGuessed(Word) :-
    \+ guessedWord(Word).

% Checking for case where the inputted word has already been guessed by the user
checkWordAlreadyGuessed(Word) :-
    guessedWord(Word),
    write("Word already guessed bruh"), nl,
    false.

% Validity checker that encompasses all of the previous word checking functions -- used in the play function.
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
    

