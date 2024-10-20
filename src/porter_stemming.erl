-module(porter_stemming).

%%%
%%% Porter Stemming Algorithm Implementation in Erlang  
%%%
%%% Alden Dima (alden.dima@nist.gov)
%%% National Institute of Standards and Technology,
%%% Gaithersburg, MD
%%% September, 2007
%%%
%%% This software was developed at the National Institute of Standards
%%% and Technology by employees of the Federal Government in the course
%%% of their official duties. Pursuant to title 17 Section 105 of the
%%% United States Code this software is not subject to copyright
%%% protection and is in the public domain. This software is an
%%% experimental system. NIST assumes no responsibility whatsoever for
%%% its use by other parties, and makes no guarantees, expressed or
%%% implied, about its quality, reliability, or any other characteristic.
%%% We would appreciate acknowledgement if the software is used.
%%%
%%% This software can be redistributed and/or modified freely provided
%%% that any derivative works bear some notice that they are derived
%%% from it, and any modified versions bear some notice that they have
%%% been modified.
%%%
%%%
%%% The basic philosophy behind this implementation is to map the
%%% structure of Martin Porter's original description of his algorithm
%%% (http://tartarus.org/~martin/PorterStemmer) as closely as possible
%%% into Erlang. I made liberal use of Erlang's pattern matching
%%% facility. In order for this to work, there is one quirk - the
%%% word to be matched must be reversed before it is stemmed: "hopping"
%%% becomes "gnippoh". This is necessary because Erlang's pattern
%%% matching won't work with Stem ++ "ing" but instead requires
%%% "gni" ++ Mets (where Mets is the backwards stem).  Despite this
%%% quirk, the flipping the words allowed me to greatly simplify the
%%% rest of the coding, almost to the point of being a tedious translation
%%% of the textual description of the algorithm.
%%%
%%% Thanks to Paul Black for his helpful comments.
%%%
%%% ---
%%%
%%% Updated and published to the Hex package manager by Louis Pilfold in 2024.
%%%

-export([stem/1]).

stem(Word) when is_binary(Word) ->
    unicode:characters_to_list(stem(unicode:characters_to_list(Word)));
stem(Word) when is_list(Word) ->
    stem_word(Word).

stem_word(Word) when length(Word) > 2 ->
    Lowered = string:lowercase(Word),
    Reversed = lists:reverse(Lowered),
    Stemmed = step5(step4(step3(step2(step1(Reversed))))),
    lists:reverse(Stemmed);
stem_word(Word) ->
    Word.

%%%%%%%%%%%%%
%% Step 1a %%
%%%%%%%%%%%%%

% Some notational goofiness on my part:
% "Mets" means "Stem" backwards, "Drow" means "Word".
% Given that a word can be processed by several rules and that for
% some of the rules what matches as the "stem" isn't the final result,
% I'm using the terms word and stem loosely, but I don't have anything
% better.

step1a("sess" ++ Mets) -> "ss" ++ Mets;

step1a("sei" ++ Mets) -> "i" ++ Mets;

step1a(Drow = "ss" ++ _) -> Drow;

step1a("s" ++ Mets) -> Mets;

step1a(Drow) -> Drow.


%%%%%%%%%%%%%
%% Step 1b %%
%%%%%%%%%%%%%

step1b(Drow = "dee" ++ Mets) ->
    case measure(Mets) of
        M when M > 0 ->
            "ee" ++ Mets;
        0 ->
            Drow
    end;

step1b(Drow = "de" ++ Mets) ->
    case has_vowel(Mets) of
        true -> 
            step1b2(Mets);
        false ->
            Drow
    end;

step1b(Drow = "gni" ++ Mets) ->
    case has_vowel(Mets) of
        true -> 
            step1b2(Mets);
        false ->
            Drow
    end;

step1b(Drow) -> Drow.


step1b2("ta" ++ Mets) -> "eta" ++ Mets;

step1b2("lb" ++ Mets) -> "elb" ++ Mets;

step1b2("zi" ++ Mets) -> "ezi" ++ Mets;

step1b2(Drow = [C, C | Mets]) ->
    case ends_with_double_cons(Drow)
        and not(ends_with($l, Drow)
             or ends_with($s, Drow)
             or ends_with($z, Drow)) of
        true ->
            [C] ++ Mets;
        false ->
            Drow
    end;

step1b2(Drow) ->
    M = measure(Drow),
    O = ends_with_cvc(Drow),
    if
        (M == 1) and O -> "e" ++ Drow;
        true -> Drow
    end.


%%%%%%%%%%%%%
%% Step 1c %%
%%%%%%%%%%%%%

step1c(Drow = "y" ++ Mets) ->
    case has_vowel(Mets) of
        true -> "i" ++ Mets;
        false -> Drow
    end;
    
step1c(Drow) -> Drow.


step1(Drow) ->
    step1c(step1b(step1a(Drow))).


%%%%%%%%%%%%
%% Step 2 %%
%%%%%%%%%%%%

m_repl(N, Drow, Mets, Repl) ->
    M = measure(Mets),
    if
        M > N -> Repl ++ Mets;
        true -> Drow
    end.

% To make the code less cluttered, I've used macros here and elsewhere
% to define function clauses that are structurally identical but that
% pattern match on differing terms.

-define(step2(MATCH, REPL),
        step2(Drow = MATCH ++ Mets) -> m_repl(0, Drow, Mets, REPL)).

?step2("lanoita", "eta");
?step2("lanoit", "noit");
?step2("icne", "ecne");
?step2("icna", "ecna");
?step2("rezi", "ezi");
?step2("ilb", "elb");   % replacement rule from website
?step2("illa", "la");
?step2("iltne", "tne");
?step2("ile", "e");
?step2("ilsuo", "suo");
?step2("noitazi", "ezi");
?step2("noita", "eta");
?step2("rota", "eta");
?step2("msila", "la");
?step2("ssenevi", "evi");
?step2("ssenluf", "luf");
?step2("ssensuo", "suo");
?step2("itila", "la");
?step2("itivi", "evi");
?step2("itilib", "elb");
?step2("igol", "gol");  % new rule from website

step2(Drow) -> Drow.


%%%%%%%%%%%%
%% Step 3 %%
%%%%%%%%%%%%

-define(step3(MATCH, REPL),
        step3(Drow = MATCH ++ Mets) -> m_repl(0, Drow, Mets, REPL)).

?step3("etaci", "ci");
?step3("evita", "");
?step3("ezila", "la");
?step3("itici", "ci");
?step3("laci", "ci");
?step3("luf", "");
?step3("ssen", "");

step3(Drow) -> Drow.


%%%%%%%%%%%%
%% Step 4 %%
%%%%%%%%%%%%

m_chop(Drow, Mets) ->
    M = measure(Mets),
    if
        M > 1 -> Mets;
        true -> Drow
    end.

-define(step4(MATCH),
        step4(Drow = MATCH ++ Mets) -> m_chop(Drow, Mets)).

?step4("la");
?step4("ecna");
?step4("ecne");
?step4("re");
?step4("ci");
?step4("elba");
?step4("elbi");
?step4("tna");
?step4("tneme");
?step4("tnem");
?step4("tne");

step4(Drow = "noi" ++ Mets) ->
    Cond = ends_with($s, Mets) orelse ends_with($t, Mets),
    if
        Cond -> m_chop(Drow, Mets);
        true -> Drow
    end;

?step4("uo");
?step4("msi");
?step4("eta");
?step4("iti");
?step4("suo");
?step4("evi");
?step4("ezi");

step4(Drow) -> Drow.


%%%%%%%%%%%%%
%% Step 5a %%
%%%%%%%%%%%%%

step5a(Drow = "e" ++ Mets) ->
    M = measure(Mets),
    O = ends_with_cvc(Mets),
    if
        M > 1 -> Mets;
        M == 1, not(O) -> Mets;
        true -> Drow
    end;

step5a(Drow) -> Drow.


%%%%%%%%%%%%%
%% Step 5b %%
%%%%%%%%%%%%%

step5b(Drow = "ll" ++ Mets) ->
    M = measure(Drow),
    if M > 1 -> "l" ++ Mets;
        true -> Drow
    end;

step5b(Drow) -> Drow.


step5(Drow) -> step5b(step5a(Drow)).


%%%%%%%%%%%%%
%% Helpers %%
%%%%%%%%%%%%%

%
% Utility function to cut down on different sections code testing for vowels.
%
is_vowel(L, Y_is_vowel) ->
    case L of
        $a -> true;
        $e -> true;
        $i -> true;
        $o -> true;
        $u -> true;
        $y when Y_is_vowel -> true;
        _  -> false
    end.

%
% Implements m, the measure of a word or word part.
%
measure(Drow) -> measure(lists:reverse(Drow), 0).


measure([], 0) -> 0;

measure([H|T], 0) ->
    case is_vowel(H, false) of
        true  -> found_vowel(T, 0);
        false -> found_leading_consonant(T)
    end.


found_leading_consonant([]) -> 0;

found_leading_consonant([H|T]) ->
    case is_vowel(H, true) of
        true  -> found_vowel(T, 0);
        false -> found_leading_consonant(T)
    end.


found_vowel([], M) -> M;

found_vowel([H|T], M) ->
    case is_vowel(H, false) of
        true  -> found_vowel(T, M);
        false -> found_consonant(T, M + 1)
    end.


found_consonant([], M) -> M;

found_consonant([H|T], M) ->
    case is_vowel(H, true) of
        true  -> found_vowel(T, M);
        false -> found_consonant(T, M)
    end.


%
% Implements *S - the stem ends with "s" (and similarly for other letters).
%
ends_with(_, [])   -> false;
ends_with(L, Mets) -> L == hd(Mets).


%
% Implements *v* - the stem contains a vowel
%
has_vowel(Mets) -> has_vowel1(lists:reverse(Mets)).


has_vowel1([]) -> false;

has_vowel1([H|T]) ->
    case is_vowel(H, false) of
        true  -> true;
        false -> has_vowel2(T)
    end.


has_vowel2([]) -> false;

has_vowel2([H|T]) ->
    case is_vowel(H, true) of
        true  -> true;
        false -> has_vowel2(T)
    end.


%
% Implements *d - the stem ends with a double consonant.
%
ends_with_double_cons([C, C|_]) -> not is_vowel(C, true);
ends_with_double_cons(_)        -> false.


%
% Implements *o - the stem ends cvc, where the second c is not w, x, or y.
%
ends_with_cvc([C2, V, C1|_]) ->
    (not is_vowel(C1, false))
    andalso is_vowel(V, true)
    andalso not (is_vowel(C2, true) orelse lists:member(C2, [$w, $x]));

ends_with_cvc(_) -> false.
