(* © Copyright University of Birmingham, UK *)

open Common
open Nfa

(* each beta is represented by a simple (reversed) list of states *)
type t = int list;;

module BetaSet = Set.Make (
  struct
    type t = int list;;
    let compare = Pervasives.compare;;
  end);;

let make i = [i];;

let elems b = List.fold_left (fun s i -> IntSet.add i s) IntSet.empty b;;

(* sorted tree of betas *)
(* 实现了左节点小于当前节点，当前节点小于右节点，且节点无重复。简化状态。*)
type otree = OTNull | OTNode of char * char * int list * otree * otree;;

(*
  - inserts a new beta into the tree
  - in case of overlaps, the new beta is pushed in front of the old one

  - TODO: Not sure if pushing in-front improves performance or not (needs profiling)
  - is this the whole reason for storing beta in reverse ?
*)
let rec otr_add tr input_1 input_2 input_num_list = match tr with
  |OTNull -> OTNode (input_1, input_2, input_num_list, OTNull, OTNull)
  |OTNode (u, v, l, ltr, rtr) when input_2 < u -> OTNode (u, v, l, otr_add ltr input_1 input_2 input_num_list, rtr)
  |OTNode (u, v, l, ltr, rtr) when v < input_1 -> OTNode (u, v, l, ltr, otr_add rtr input_1 input_2 input_num_list)
  |OTNode (u, v, l, ltr, rtr) ->
    let _ltr = if u == input_1 then ltr else if u < input_1 then otr_add ltr u (zprev input_1) l else otr_add ltr input_1 (zprev u) input_num_list in
    let _rtr = if v == input_2 then rtr else if input_2 < v then otr_add rtr (znext input_2) v l else otr_add rtr (znext v) input_2 input_num_list in
    OTNode (max u input_1, min v input_2, input_num_list @ l, _ltr, _rtr);;

  (*
  otr_add:

      if input_2 < u 
          OTNode (u, v, l, otr_add ltr input_1 input_2 input_num_list, rtr)
      else if v < input_1
          OTNode (u, v, l, ltr, otr_add rtr input_1 input_2 input_num_list)
      else if input_2 >= u and input_1 <= v

          if u == input_1
              _ltr = ltr
          else if u < input_1
              _ltr = otr_add ltr u (zprev input_1) l 
          else if u > input_1
              _ltr = otr_add ltr input_1 (zprev u) input_num_list

          if v == input_2
              _rtr = rtr
          else if input_2 < v 
              _rtr = otr_add rtr (znext input_2) v l
          else if v < input_2
              _rtr = otr_add rtr (znext v) input_2 input_num_list

          OTNode (max u input_1, min v input_2, input_num_list @ l, _ltr, _rtr);;
  *)
  

(* collect the new betas, with the corresponding prefix words leading to them *)
let rec otr_collect tr w lst = match tr with
  |OTNull -> lst
  |OTNode (u, v, l, ltr, rtr) ->
    (* 
      - we must fold right, since the beta is stored in reverse
      - only keep the left-most (right-most when reversed) occurrences using an integer set
    *)
    (*
      将 l 中的元素去重放进 b 中
    *)
    let (_, b) = List.fold_right (
          fun i (s, b) -> 

            if IntSet.mem i s then 

              (s, b)

            else 

              (IntSet.add i s, i :: b)

        )
        
        l (IntSet.empty, [])
    
    in
    (* package the resulting beta along with its prefix *)
    (* 遍历左子树和右子树，将子树中的l去重得到b，然后将子树的以 ((u, v), b)::w 的对加入到结果中返回 *)
    otr_collect rtr w (otr_collect ltr w ((Word.extend w (u, v), b) :: lst));;

let advance (nfa, w, b) =
  (*
    - we must fold right, since the beta is stored in reverse
    - otr_add pushes new betas in front of old ones, respecting the reverse order
  *)
  (*
    取出b中的所有数字，分别获取他们代表的节点，在NFA上的所有后继。
      单个后继表示为 (u, v, num)
      将所有的后继以 (u, v, [num]) 形式加入到tr树中
  *)
  let tr = List.fold_right (
        fun i tr 
                -> List.fold_left (
                      fun tr (u, v, j)
                                        -> otr_add tr u v [j]
                                  )
                          tr (Nfa.get_transitions nfa i)
        )
            b OTNull
  in
  otr_collect tr w [];;

(*
  w 为字符范围，b 为 NFA 的序号；[('z', 'z')], [1]

  这个函数消除\epsilon
*)
let evolve (nfa, w, b) kset =
  let flgs = ref Flags.empty in
  (*
    - consume all epsilon moves, no duplicates allowed in the result
    - notice that the final evolved beta comes out in reverse order (as desired)
  *)
  let rec evolve rb st eb hits = match rb with
  (*
    rb b 的倒序列表
    st 存储 b 中元素去重后的元素集合
    eb 存储已经处理的 NFA 状态
    hits 捕获的 kleene 元素
  *)
    |[] -> (!flgs, eb, hits)
    |i :: t when IntSet.mem i st -> evolve t st eb hits (* already explored the left-most occurence, ignore *)
    |i :: t ->
      let st = IntSet.add i st in
      match Nfa.get_state nfa i with
        |End |Kill -> evolve t st (i :: eb) hits
        |Pass j -> evolve (j :: t) st eb hits
        |MakeB j -> evolve (j :: t) st eb hits
        |EvalB j -> evolve (j :: t) st eb hits
        |BeginCap (_, j) -> evolve (j :: t) st eb hits
        |EndCap (_, j) -> evolve (j :: t) st eb hits
        |Match _ -> evolve t st (i :: eb) hits
        |CheckPred (P_BOI, j) ->
          if Word.is_empty w then evolve (j :: t) st eb hits else evolve t st eb hits
        |CheckPred (P_BOL ulines, j) ->
          begin
            match Word.tail w with
              |None -> evolve (j :: t) st eb hits
              |Some ((u, v), _) when u <= '\n' && '\n' <= v -> evolve (j :: t) st eb hits
              |Some ((u, v), _) when ulines && u <= '\r' && '\r' <= v -> evolve (j :: t) st eb hits
              |_ -> evolve t st eb hits
          end
        |CheckPred (P_EOI, _) ->
          evolve t st (i :: eb) hits
        |CheckPred _  | CheckBackref _ ->
          flgs := Flags.set_interrupted !flgs;
          evolve t st eb hits
        |BranchAlt (j, k) -> evolve (j :: k :: t) st eb hits
        |BranchKln (gd, j, k) ->
          (* record kleene hits, along with the corresponding left-beta *)
          let hits = if IntSet.mem i kset then (i, (i :: eb)) :: hits else hits in
          (* ordering implied by the quantifier must be taken into account when evolving a beta *)
          if gd then
            evolve (j :: k :: t) st eb hits
          else
            evolve (k :: j :: t) st eb hits in
  (* must reverse beta in order to be able to pattern match *)
  evolve (List.rev b) IntSet.empty [] [];;
