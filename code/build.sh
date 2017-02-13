# © Copyright University of Birmingham, UK

#!/bin/bash
set -e
ocamlyacc RegexParser.mly
ocamllex RegexLexer.mll
ocamlyacc PatternParser.mly
ocamllex PatternLexer.mll
ocamlc -g -bin-annot -c ParsingData.mli ParsingData.ml
ocamlc -g -bin-annot -c RegexParser.mli RegexParser.ml
ocamlc -g -bin-annot -c RegexLexer.ml
ocamlc -g -bin-annot -c PatternParser.mli PatternParser.ml
ocamlc -g -bin-annot -c PatternLexer.ml
ocamlc -g -bin-annot -c ParsingMain.mli ParsingMain.ml
ocamlc -g -bin-annot -c Common.mli Common.ml
ocamlc -g -bin-annot -c Nfa.mli Nfa.ml
ocamlc -g -bin-annot -c RegexScanner.mli RegexScanner.ml
ocamlc -g -bin-annot -c Flags.mli Flags.ml
ocamlc -g -bin-annot -c Word.mli Word.ml
ocamlc -g -bin-annot -c Util.mli Util.ml
ocamlc -g -bin-annot -c Beta.mli Beta.ml
ocamlc -g -bin-annot -c Phi.mli Phi.ml
ocamlc -g -bin-annot -c Triple.mli Triple.ml
ocamlc -g -bin-annot -c Product.mli Product.ml
ocamlc -g -bin-annot -c XAnalyser.mli XAnalyser.ml
ocamlc -g -bin-annot -c Y1Analyser.mli Y1Analyser.ml
ocamlc -g -bin-annot -c Y2Analyser.mli Y2Analyser.ml
ocamlc -g -bin-annot -c ZAnalyser.mli ZAnalyser.ml
ocamlc -g -bin-annot -c AnalyserMain.mli AnalyserMain.ml
ocamlc -g -bin-annot -c RuleScanner.mli RuleScanner.ml
ocamlc -g -bin-annot -c Run.ml
ocamlc -g -bin-annot str.cma ParsingData.cmo RegexParser.cmo RegexLexer.cmo PatternParser.cmo PatternLexer.cmo ParsingMain.cmo Common.cmo Nfa.cmo RegexScanner.cmo Flags.cmo Word.cmo Util.cmo Beta.cmo Phi.cmo Triple.cmo Product.cmo XAnalyser.cmo Y1Analyser.cmo Y2Analyser.cmo ZAnalyser.cmo unix.cma AnalyserMain.cmo RuleScanner.cmo Run.cmo -o scan.bin
#rm *.cmi *.cmo RegexParser.mli RegexParser.ml RegexLexer.ml PatternParser.mli PatternParser.ml PatternLexer.ml
