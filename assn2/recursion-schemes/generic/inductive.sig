signature INDUCTIVE =
sig
  (* t.τ *)
  structure T: POSITIVE_TYPE_OPERATOR (* parameter *)

  (* μ(t.τ) *)
  type t (* abstract *)

  (*
        Γ ⊢ e : [μ(t.τ)/t]τ
    ---------------------------
     Γ ⊢ fold{t.τ}(e) : μ(t.τ)
  *)
  val FOLD: t T.view -> t

  (*
     Γ, x' : [ρ/t]τ ⊢ e' : ρ     Γ ⊢ e : μ(t.τ)
    --------------------------------------------
             Γ ⊢ rec{t.τ}(x'.e')(e) : ρ
  *)
  val REC: ('rho T.view -> 'rho) -> t -> 'rho
end
