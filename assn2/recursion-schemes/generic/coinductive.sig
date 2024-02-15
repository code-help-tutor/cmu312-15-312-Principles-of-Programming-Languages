signature COINDUCTIVE =
sig
  (* t.τ *)
  structure T: POSITIVE_TYPE_OPERATOR (* parameter *)

  (* ν(t.τ) *)
  type t (* abstract *)

  (*
     Γ, x' : σ ⊢ e' : [σ/t]τ     Γ ⊢ e : σ
    ---------------------------------------
        Γ ⊢ gen{t.τ}(x'.e')(e) : ν(t.τ)
  *)
  val GEN: ('sigma -> 'sigma T.view) -> 'sigma -> t

  (*
              Γ ⊢ e : ν(t.τ)
    ----------------------------------
     Γ ⊢ unfold{t.τ}(e) : [ν(t.τ)/t]τ
  *)
  val UNFOLD: t -> t T.view
end
