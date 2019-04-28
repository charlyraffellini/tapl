open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies

let rec isnumericval t = match t with
    TmZero(_) -> true
  | TmSucc(_,t1) -> isnumericval t1
  | _ -> false

let rec isval t = match t with
    TmTrue(_)  -> true
  | TmFalse(_) -> true
  | t when isnumericval t  -> true
  | _ -> false

(*---------------------------------------------------------------*)

let rec evalInBigSteps1 t = match t with
    TmIf(fi,t1,t2,t3) ->
      evalTmIf t
  | TmSucc(fi,t1) when (not (isval t)) ->
      evalTmSucc t
  | TmPred(_,nv) when (not (isval t)) ->
      evalTmPred t
  | TmIsZero(_,_) ->
      evalTmIsZero t
  | _ -> 
      raise NoRuleApplies
  
and evalTmIsZero t = match t with
    TmIsZero(fi,ti) ->
        let t1 = evalInBigSteps ti in
        match t1 with
            TmZero(_) -> TmTrue(dummyinfo)
          | TmSucc(_,_) -> TmFalse(dummyinfo)

and evalTmPred t = match t with
    TmPred(fi,ti) ->
        let t1 = evalInBigSteps ti in
        match t1 with
            TmZero(_) -> t1
          | TmSucc(_, nv) -> nv   

and evalTmSucc t = match t with
    TmSucc(fi,t1) ->
        TmSucc(fi, evalInBigSteps t1)

and evalTmIf t = match t with
    TmIf(fi,t1,t2,t3) ->
        match (evalInBigSteps t1) with
            TmTrue(_) -> evalInBigSteps t2
          | TmFalse(_) -> evalInBigSteps t3

and evalInBigSteps t =
    try let t' = evalInBigSteps1 t
        in evalInBigSteps t'
    with NoRuleApplies -> t
