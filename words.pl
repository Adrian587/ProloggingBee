% Knowledge Base for the game.

% each day's mandatory letter.
% mandatoryLetter(<day>, <mandatory letter>).
mandatoryLetter(0, p).
mandatoryLetter(1, c).

% the available letters for each day's game.
% availableLetters(<day>, <available letters>).
availableLetters(0, [p,e,l,h,g,i,o]).
availableLetters(1, [a,m,e,d,i,c,t]).

% the total number of possible words for each day's game.
% numWords(<day>, <number of words>)
numWords(0, 37).
numWords(1, 75).

% each word predicate represents the current day of the game and the word itself.
% for example, the game on day 0 contains all words with word(0, _).
% word(<day>, <word>). 

word(0, elope).
word(0, epee).
word(0, epilog).
word(0, gloop).
word(0, glop).
word(0, goop).
word(0, help).
word(0, hippie).
word(0, hippo).
word(0, hoop).
word(0, hope).
word(0, lipo).
word(0, logophile).
word(0, lollipop).
word(0, lollop).
word(0, loop).
word(0, loophole).
word(0, lope).
word(0, peel).
word(0, peep).
word(0, peephole).
word(0, people).
word(0, piehole).
word(0, pile).
word(0, pill).
word(0, pipe).
word(0, plie).
word(0, plop).
word(0, pogo).
word(0, pole).
word(0, polio).
word(0, poll).
word(0, polo).
word(0, pooh).
word(0, pool).
word(0, poop).
word(0, pope).


word(1, acacia).
word(1, academe).
word(1, academia).
word(1, academic).
word(1, acai).
word(1, accede).
word(1, acceded).
word(1, aced).
word(1, acetate).
word(1, acetic).
word(1, acid).
word(1, acidic).
word(1, acme).
word(1, acted).
word(1, addict).
word(1, addicted).
word(1, attic).
word(1, cacti).
word(1, caddie).
word(1, caddied).
word(1, cadet).
word(1, came).
word(1, cami).
word(1, cede).
word(1, ceded).
word(1, cicada).
word(1, cite).
word(1, cited).
word(1, decade).
word(1, deceit).
word(1, decide).
word(1, decided).
word(1, decimate).
word(1, decimated).
word(1, dedicate).
word(1, dedicated).
word(1, dedicatee).
word(1, deice).
word(1, deiced).
word(1, deicide).
word(1, detect).
word(1, detected).
word(1, dice).
word(1, diced).
word(1, dicta).
word(1, dictate).
word(1, dictated).
word(1, didact).
word(1, didactic).
word(1, dietetic).
word(1, edict).
word(1, eidetic).
word(1, emaciate).
word(1, emaciated).
word(1, emcee).
word(1, emceed).
word(1, emetic).
word(1, iced).
word(1, macadam).
word(1, macadamia).
word(1, mace).
word(1, maced).
word(1, mecca).
word(1, medic).
word(1, medicate).
word(1, medicated).
word(1, mica).
word(1, mice).
word(1, mimetic).
word(1, mimic).
word(1, tacet).
word(1, tacit).
word(1, tact).
word(1, tactic).
word(1, titmice).