# Active scalar polymorphic receiver

FortAD differentiates an active scalar class(base_t) receiver when FortFront proves exactly one same-file concrete implementation of its deferred or inherited binding. The receiver is borrowed and its dynamic type is passive.

JVP and VJP carry ordinary component shadows through paired SELECT TYPE constructs. The reverse sweep repeats the primal dispatch, selects the scalar receiver cotangent in parallel, and accumulates only active concrete components. The shadow is caller-owned; FortAD does not differentiate the dynamic type tag or replay polymorphic ownership.

This bounded slice refuses pointer or ASSOCIATE receiver aliases and active dispatch with multiple concrete targets. Array elements, sections, allocatable or ownership-changing receivers, dynamic target selection, and active dynamic-type perturbations remain separate boundaries. The independent analytical, finite-difference, and adjoint-identity oracle is test/test_polymorphic_scalar_receiver_oracle.f90.
