/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Bubble sort. This is very easy to describe and encode in Prolog:

   As long as there are two elements ...,A,B,... where A @> B (note
   the use use (@>)/2 to make it work for all terms, using the
   standard order of terms), exchange the elements and repeat.

   Usage example:

      ?- bubblesort([a,b,c,1,5,0,x], Ls).
      %@ Ls = [0, 1, 5, a, b, c, x].

   I am using ediprolog to evaluate queries directly in the Emacs buffer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bubblesort(Ls0, Ls) :-
        (	append(Lefts, [A,B|Rights], Ls0), A @> B ->
            append(Lefts, [B,A|Rights], Ls1),
            bubblesort(Ls1, Ls)
        ;	Ls = Ls0
        ).
    
    /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       Quicksort
    
       The well-known and often very slow quicksort. Beware! This is
       typically not a good algorithm for sorting. One of the reasons is
       that data that occurs naturally is often already presorted in some
       way. See the literature for more information. In Prolog, quicksort
       can be much more elegantly be described using DCGs, see below.
    
       We use the auxiliary predicate partition/4 (see below), which
       splits the initial list into two parts: Elements smaller and
       elements bigger than the pivot element. In the implementation
       below, we pick the first element of the original list as the pivot.
       Other strategies are possible and often affect the average running
       time, but typically not the worst-case complexity of the algorithm.
    
       Usage example:
    
          ?- quicksort([a,b,c,1,5,0,x], Qs).
          %@ Qs = [0, 1, 5, a, b, c, x].
    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    
    quicksort([], []).
    quicksort([L|Ls0], Ls) :-
            partition(Ls0, L, Smallers0, Biggers0),
            quicksort(Smallers0, Smallers),
            quicksort(Biggers0, Biggers),
            append(Smallers, [L|Biggers], Ls).
    
    partition([], _, [], []).
    partition([L|Ls], Pivot, Smallers0, Biggers0) :-
            (   L @< Pivot ->
                Smallers0 = [L|Smallers],
                partition(Ls, Pivot, Smallers, Biggers0)
            ;   Biggers0 = [L|Biggers],
                partition(Ls, Pivot, Smallers0, Biggers)
            ).
    
    /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       The above version of quicksort is not very elegant. A much more
       elegant way to describe lists in Prolog is to use a built-in
       formalism called Definite Clause Grammars (DCGs).
    
       A short DCG primer explaining the core ideas is available at:
    
                     https://www.metalevel.at/prolog/dcg
                     ===================================
    
       We now use a DCG to express quicksort in a very natural and more
       elegant way. Note that it is no longer necessary to use append/3 in
       this version. We reuse the definition of partition/4 above.
    
       We use the interface predicate phrase/2 to run the DCG:
    
          ?- phrase(quicksort([a,b,c,1,5,0,x]), Ls).
          %@ Ls = [0, 1, 5, a, b, c, x].
    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    
    quicksort([])	  --> [].
    quicksort([L|Ls]) -->
            { partition(Ls, L, Smallers, Biggers) },
            quicksort(Smallers),
            [L],
            quicksort(Biggers).
    
    /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       Merge sort.
    
       This is often a good choice. One advantage is that it can be easily
       made stable, which means that equal elements retain their original
       relative positions.
    
       Notice the use of append/3 to split the list. The auxiliary
       predicate merge/3 is used, see below. In some Prolog systems, this
       predicate is available as a built-in or library predicate, and you
       can omit its definition.
    
       Usage example:
    
          ?- mergesort([a,b,c,1,5,0,x], Ls).
          %@ Ls = [0, 1, 5, a, b, c, x].
    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    
    :- use_module(library(clpfd)).
    
    mergesort(Ls0, Ls) :-
            length(Ls0, L),
            zcompare(C, L, 1),
            halving(C, L, Ls0, Ls).
    
    halving(<, _, Ls, Ls).
    halving(=, _, Ls, Ls).
    halving(>, L, Ls0, Ls) :-
            Half #= L // 2,
            length(Lefts0, Half),
            append(Lefts0, Rights0, Ls0),
            mergesort(Lefts0, Lefts),
            mergesort(Rights0, Rights),
            merge(Lefts, Rights, Ls).
    
    % If your Prolog library provides merge/3, you can remove this definition.
    
    merge([], Ys, Ys) :- !.
    merge(Xs, [], Xs) :- !.
    merge([X|Xs], [Y|Ys], Ms) :-
            (   X @< Y ->
                Ms = [X|Rs],
                merge(Xs, [Y|Ys], Rs)
            ;   Ms = [Y|Rs],
                merge([X|Xs], Ys, Rs)
            ).
    