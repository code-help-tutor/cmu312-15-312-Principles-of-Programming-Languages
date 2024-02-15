signature TYPE_OPERATOR =
sig
  (* t.τ *)
  type 't view (* parameter *)
end

signature POSITIVE_TYPE_OPERATOR =
sig
  include TYPE_OPERATOR

  (*
     Γ, x : ρ ⊢ e' : ρ'     Γ ⊢ e : [ρ/t]τ
    ---------------------------------------
        Γ ⊢ map{t.τ}(x.e')(e) : [ρ'/t]τ
  *)
  val map: ('rho1 -> 'rho2) -> 'rho1 view -> 'rho2 view
end
