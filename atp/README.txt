README

This code has 3 examples. It is run by the command (atp nqi kbi) where i is 1 2 or 3 for the 1st, 2nd, or 3rd example.

The theorem prover uses 3 heuristics, subsumption, set of support, and preference for shorter clauses in order to select the next candidate in order to run. Note, that the code does not need any ANDs or ORs for the KB or negated query (it assumes the placement of the clause in CNF is accurate and determines the appropriate logical operator from that. I.e, a list of sentences in the form (sent1 sent2 (clause1 clause 2) ...) can be provided, and the program will interpret that sent1, sent2 and (clause1 clause2) are separated by conjunctions and clause1 clause2 are separated by disjunctions.

Each resolution step is printed to the console as (clause1 clause2 resolvant (bindings for that step))

In order to run the extra credit conversion to skolem normal form, run (snf sample-kb) where sample-kb must be provided by the user (or one of the example kb's, kb1, kb2 or kb3 can be used.