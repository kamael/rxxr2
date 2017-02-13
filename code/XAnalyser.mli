(* © Copyright University of Birmingham, UK *)

open Common
open Beta
open Phi

(* internal representation of the analyser *)
type t = {
  (* NFA being analysed *)
  nfa : Nfa.t;
  (* set of pumpable kleene states *)
  kset : IntSet.t;
  (* current prefix word *)
  mutable w : Word.t;
  (* set of betas seen so far *)
  mutable bcache : BetaSet.t;
  (* set of phis seen so far *)
  mutable pcache : PhiSet.t;
  (* machine component - current kleene hits *)
  mutable hits : (int * Beta.t) list;
  (* machine component - betas to be evolved *)
  mutable evolve : (Word.t * Beta.t) list;
  (* machine component - betas to be advanced *)
  mutable advance : (Word.t * Beta.t) list;
  (* processing flags *)
  mutable flgs : Flags.t;
};;

(* initialize analyser instance with the given NFA and the pumpable kleene set *)
val init : Nfa.t -> IntSet.t -> t;;

(* calculate the next (x, phi) corresponding to some pumpable kleene *)
val next : t -> (int * Word.t * Phi.t) option;;

(* read current analyser flags *)
val flags : t -> Flags.t;;
