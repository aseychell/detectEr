detectEr
========

A formally proven Runtime Verification tool for Erlang that is able to synthesizing concurrent monitors from a correctness property.

detectEr is a prototype academic tool for Runtime Verification. The tool is written in Erlang and currently targeted for Erlang programs. The aim of this tool is to monitor for sHML formulas which are the safety properties subset of the full Hennessey-Milner Logic with recursion. Apart from the actual implementation that is available below, this tool is formally proven correct in the technical report which can be found at http://www.cs.um.edu.mt/~svrg/Papers/shml-correct.pdf.

The tool is able to trace external interactions of a system with external clients of the system and define properties over these interactions using sHML.
