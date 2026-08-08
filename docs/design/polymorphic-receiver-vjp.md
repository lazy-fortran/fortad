# Polymorphic array receiver VJP

P8.3f closes the reverse half of the fixed-path polymorphic array receiver
slice. The accepted source shape is a borrowed, one-dimensional
class(base_t) :: a(:) receiver used at one literal index, for example:

    y = a(2)%value(x)

FortFront must prove exactly one same-file concrete dispatch target. The
receiver's dynamic type is passive: FortAD differentiates the selected
concrete implementation, but does not differentiate a change of the type
tag.

The generated VJP retains the primal SELECT TYPE replay and adds a parallel
selection on the caller-owned cotangent array. Inside the one matching
concrete arm, each active component contribution is scattered into the
selected cotangent element:

    select type (receiver => a(2))
    type is (child_t)
        select type (receiver_b => a_b(2))
        type is (child_t)
            receiver_b%scale = receiver_b%scale + seed*x
            receiver_b%bias = receiver_b%bias + seed
        class default
        end select
    class default
    end select

The reverse emitter resolves the lowered SELECT TYPE alias back to its source
receiver path for activity matching, while routing the actual cotangent writes
through the selected cotangent alias. Active cotangent components are zeroed
inside that nested concrete arm, so the output does not depend on caller
initialization. JVP lowering is unchanged.

This slice deliberately refuses dynamic indices, array sections, pointers,
associate aliases, allocatable or ownership-changing receivers, unresolved
or multi-target dispatch, and a bare active polymorphic receiver whose
dynamic type would be perturbed. General polymorphic ownership replay,
receiver aliases, dynamic target selection, and active type-tag derivatives
remain outside this contract.

test/test_polymorphic_array_receiver_oracle.f90 independently compiles the
generated JVP and VJP, checks hand values, a central finite difference for
the JVP, the full reverse adjoint identity across receiver components and x,
and both JVP/VJP refusal diagnostics.
