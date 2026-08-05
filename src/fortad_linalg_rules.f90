module fortad_linalg_rules
    !! Built-in structured rules for the external dense-solve path.
    !!
    !! The caller still supplies the explicit BLAS/LAPACK interfaces and links
    !! those libraries. The rule table supplies the mathematics: dgesv leaves
    !! an LU factorisation and X in its arguments, so its tangent uses dgemm
    !! plus dgetrs and its adjoint solves the transposed system and applies
    !! the rank-one/multiple-right-hand-side update with dgemm.
    use fortad_registry, only: fad_add_call_rule
    implicit none
    private

    public :: fad_register_blas_lapack_rules

contains

    subroutine fad_register_blas_lapack_rules(stat)
        !! Register the dgesv implicit-differentiation rule table.
        integer, intent(out), optional :: stat
        integer :: local

        local = 0
        call fad_add_call_rule("dgesv", 8, &
            tangent=[character(len=256) :: &
                     "call dgemm('N', 'N', $1, $2, $1, -1.0d0, "// &
                     "$3d, $4, $6, $7, 1.0d0, $6d, $7)", &
                     "call dgetrs('N', $1, $2, $3, $4, $5, $6d, $7, $8)"], &
            adjoint=[character(len=256) :: &
                     "call dgetrs('T', $1, $2, $3, $4, $5, $6b, $7, $8)", &
                     "call dgemm('N', 'T', $1, $1, $2, -1.0d0, "// &
                     "$6b, $7, $6, $7, 1.0d0, $3b, $4)"], stat=local)
        if (present(stat)) stat = local
    end subroutine fad_register_blas_lapack_rules

end module fortad_linalg_rules
