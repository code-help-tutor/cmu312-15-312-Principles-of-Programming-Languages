signature SYN_EXT =
sig
  type class = PyCF.Class.t

  type label = FPC.Label.t
  (* Dictionary-like structure mapping labels to 'a *)
  type 'a labeled = 'a FPC.Labeled.t
  type typ = FPC.Typ.t
  type typVar = FPC.Typ.typVar
  type exp = FPC.Exp.t
  type expVar = FPC.Exp.expVar

  val bindTy: string -> (typVar -> 'a) -> typVar * 'a
  val bindExp: string -> (expVar -> 'a) -> expVar * 'a

  val fromList: (label * 'a) list -> 'a labeled

  val Unit: typ
  val Triv: exp

  val Bool: typ
  val True: exp
  val False: exp
  (* If rho e (e1, e0) *)
  val If: typ -> exp -> exp * exp -> exp

  (* classToSummand t c gives the label and type associated with the
     class c in the PyCF dynamic type, with type t in all recursive spots *)
  val classToSummand: typ -> class -> label * typ

  (* DynTys is the specific labeling for the dynamic type of PyCF *)
  val DynTys: typ -> typ labeled

  (* DynView t is the large sum type parameterized by type t
     DynView Dyn is the type of Dyn when unfolded *)
  val DynView: typ -> typ

  (* Dyn is the recursive type Dyn = rec(d.DynView[d]) *)
  val Dyn: typ

  (* DynMatch rho (ebool, eint, elist, efun)
     DynMatch allows you to case on a Dyn and you can decide what to
     do in case of it having 4 different labels *)
  val DynMatch:
    typ
    -> exp
    -> (expVar -> exp) * (expVar -> exp) * (expVar -> exp) * (expVar -> exp)
    -> exp
  (* new c e *)
  val new: class -> exp -> exp
  (* cast e c *)
  val cast: exp -> class -> exp

  (* SelfView tau t *)
  val SelfView: typ -> typ -> typ
  (* Self tau *)
  val Self: typ -> typ
  (* FoldSelf tau (x, e) *)
  val FoldSelf: typ -> expVar * exp -> exp
  (* Unroll tau exp *)
  val Unroll: typ -> exp -> exp

  (* Fun (tau1, tau2) (f, x, e) *)
  val Fun: typ * typ -> expVar * expVar * exp -> exp
end
