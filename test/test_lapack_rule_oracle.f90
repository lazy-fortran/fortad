program test_lapack_rule_oracle
    !! Independent oracle for the built-in dgesv structured rule.
    !!
    !! The generated forward and reverse code is linked against the real
    !! LAPACK implementation. The oracle is a complete-solve central
    !! difference plus the forward/reverse adjoint identity; neither uses
    !! another AD implementation.
    use fortad, only: fad_jvp, fad_vjp, fad_clear_rules, &
                      fad_register_blas_lapack_rules, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: SOURCE = &
        "subroutine k(n, nrhs, amat, rhs, ipiv, info, s)"//nl// &
        "    use lapack_support, only: dgesv, dgetrs, dgemm"//nl// &
        "    integer, intent(in) :: n"//nl// &
        "    integer, intent(in) :: nrhs"//nl// &
        "    real(8), intent(inout) :: amat(n, n), rhs(n, nrhs)"//nl// &
        "    integer, intent(out) :: ipiv(n), info"//nl// &
        "    real(8), intent(out) :: s"//nl// &
        "    call dgesv(n, nrhs, amat, n, ipiv, rhs, n, info)"//nl// &
        "    s = sum(rhs*rhs)"//nl// &
        "end subroutine k"//nl
    integer :: failures, stat
    type(fad_result_t) :: jvp, vjp
    character(len=:), allocatable :: dir
    integer :: unit

    failures = 0
    call fad_clear_rules()
    call fad_register_blas_lapack_rules(stat)
    if (stat /= 0) then
        print *, "FAIL register_blas_lapack_rules: status", stat
        error stop 1
    end if

    jvp = fad_jvp(SOURCE, [character(len=4) :: "amat", "rhs"], name="k_jvp")
    vjp = fad_vjp(SOURCE, [character(len=4) :: "amat", "rhs"], &
                  dependent="s", name="k_vjp")
    if (.not. jvp%ok) then
        print *, "FAIL dgesv forward generation: ", jvp%message
        failures = failures + 1
    end if
    if (.not. vjp%ok) then
        print *, "FAIL dgesv reverse generation: ", vjp%message
        failures = failures + 1
    end if
    if (failures > 0) error stop 1

    dir = "build/oracle_lapack_rule"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    open (newunit=unit, file=dir//"/support.f90", status="replace", &
          action="write")
    write (unit, '(a)') support_text()
    close (unit)
    open (newunit=unit, file=dir//"/derivs.f90", status="replace", &
          action="write")
    write (unit, '(a)') "module fad_generated"
    write (unit, '(a)') "    implicit none"
    write (unit, '(a)') "contains"
    write (unit, '(a)') jvp%code
    write (unit, '(a)') vjp%code
    write (unit, '(a)') "end module fad_generated"
    close (unit)
    open (newunit=unit, file=dir//"/driver.f90", status="replace", &
          action="write")
    write (unit, '(a)') driver_text()
    close (unit)

    call execute_command_line( &
        "cd "//dir//" && gfortran -O2 -o run support.f90 derivs.f90 "// &
        "driver.f90 -llapack -lblas > build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL dgesv generated code did not compile"
        call show_file(dir//"/build.log")
        error stop 1
    end if
    call execute_command_line("cd "//dir//" && ./run > out.txt 2>&1", &
                              exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL dgesv oracle"
        call show_file(dir//"/out.txt")
        error stop 1
    end if
    print *, "test_lapack_rule_oracle: all cases passed"

contains

    function support_text() result(text)
        character(len=:), allocatable :: text

        text = &
            "module lapack_support"//nl// &
            "    implicit none"//nl// &
            "    interface"//nl// &
            "        subroutine dgesv(n, nrhs, a, lda, ipiv, b, ldb, info)"//nl// &
            "            integer :: n, nrhs, lda, ldb, ipiv(*), info"//nl// &
            "            real(8) :: a(lda,*), b(ldb,*)"//nl// &
            "        end subroutine dgesv"//nl// &
            "        subroutine dgetrs(trans, n, nrhs, a, lda, ipiv, b, ldb, info)"//nl// &
            "            character(len=1) :: trans"//nl// &
            "            integer :: n, nrhs, lda, ldb, ipiv(*), info"//nl// &
            "            real(8) :: a(lda,*), b(ldb,*)"//nl// &
            "        end subroutine dgetrs"//nl// &
            "        subroutine dgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)"//nl// &
            "            character(len=1) :: transa, transb"//nl// &
            "            integer :: m, n, k, lda, ldb, ldc"//nl// &
            "            real(8) :: alpha, beta, a(lda,*), b(ldb,*), c(ldc,*)"//nl// &
            "        end subroutine dgemm"//nl// &
            "    end interface"//nl// &
            "contains"//nl// &
            "    subroutine k(n, nrhs, amat, rhs, ipiv, info, s)"//nl// &
            "        integer, intent(in) :: n"//nl// &
            "        integer, intent(in) :: nrhs"//nl// &
            "        real(8), intent(inout) :: amat(n,n), rhs(n,nrhs)"//nl// &
            "        integer, intent(out) :: ipiv(n), info"//nl// &
            "        real(8), intent(out) :: s"//nl// &
            "        call dgesv(n, nrhs, amat, n, ipiv, rhs, n, info)"//nl// &
            "        s = sum(rhs*rhs)"//nl// &
            "    end subroutine k"//nl// &
            "end module lapack_support"//nl
    end function support_text

    function driver_text() result(text)
        character(len=:), allocatable :: text

        text = &
            "program driver"//nl// &
            "    use lapack_support, only: k"//nl// &
            "    use fad_generated, only: k_jvp, k_vjp"//nl// &
            "    implicit none"//nl// &
            "    integer, parameter :: n=4, nrhs=2"//nl// &
            "    real(8) :: a(n,n), b(n,nrhs), ad(n,n), bd(n,nrhs), bd0(n,nrhs)"//nl// &
            "    real(8) :: aa(n,n), bb(n,nrhs), ac(n,n), bc(n,nrhs)"//nl// &
            "    real(8) :: ab(n,n), bbv(n,nrhs), s, sd, sp, sm, h"//nl// &
            "    real(8) :: sb, lhs, rhs, err"//nl// &
            "    integer :: ipiv(n), info, i, j, q"//nl// &
            "    do j=1,n"//nl// &
            "        do i=1,n"//nl// &
            "            a(i,j) = merge(5.0d0, 0.0d0, i == j) + 0.03d0/(i+j)"//nl// &
            "            ad(i,j) = 0.01d0*sin(0.7d0*i + 0.4d0*j)"//nl// &
            "        end do"//nl// &
            "        do q=1,nrhs"//nl// &
            "            b(j,q) = 0.5d0*j + 0.2d0*q"//nl// &
            "            bd(j,q) = cos(0.3d0*j + 0.8d0*q)"//nl// &
            "        end do"//nl// &
            "    end do"//nl// &
            "    bd0=bd"//nl// &
            "    aa=a; bb=b"//nl// &
            "    call k_jvp(n, nrhs, aa, ad, bb, bd, ipiv, info, s, sd)"//nl// &
            "    if (info /= 0) error stop 2"//nl// &
            "    h=1.0d-6"//nl// &
            "    ac=a+h*ad; bc=b+h*bd0; call k(n,nrhs,ac,bc,ipiv,info,sp)"//nl// &
            "    ac=a-h*ad; bc=b-h*bd0; call k(n,nrhs,ac,bc,ipiv,info,sm)"//nl// &
            "    err=abs(sd-(sp-sm)/(2.0d0*h))/max(1.0d0,abs(sd))"//nl// &
            "    if (err > 1.0d-7) then"//nl// &
            "        print *, 'jvp fd error',err,sd,(sp-sm)/(2.0d0*h),s"//nl// &
            "        print *, 'rhs',bb"//nl// &
            "        print *, 'rhsd',bd"//nl// &
            "        error stop 3"//nl// &
            "    end if"//nl// &
            "    aa=a; bb=b; sb=1.0d0"//nl// &
            "    call k_vjp(n,nrhs,aa,bb,ipiv,info,s,sb,ab,bbv)"//nl// &
            "    if (info /= 0) error stop 4"//nl// &
            "    lhs=sd; rhs=sum(ab*ad)+sum(bbv*bd0)"//nl// &
            "    err=abs(lhs-rhs)/max(1.0d0,abs(lhs))"//nl// &
            "    if (err > 1.0d-10) then; print *, 'vjp identity error',err; error stop 5; end if"//nl// &
            "end program driver"//nl
    end function driver_text

    subroutine show_file(path)
        character(len=*), intent(in) :: path
        integer :: unit, ios
        character(len=512) :: line

        open (newunit=unit, file=path, status="old", action="read", iostat=ios)
        if (ios /= 0) return
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            print *, trim(line)
        end do
        close (unit)
    end subroutine show_file

end program test_lapack_rule_oracle
