program test_products_oracle
    !! The worked examples in docs/products.md, executed.
    !!
    !! Documentation that is not run drifts. Every claim on that page about how
    !! to obtain a product is checked here against an independent computation of
    !! the same quantity.
    !!
    !! The linear-UQ case is the one worth stating: `cov(y) = J cov(x) Jᵀ` is
    !! checked against the covariance formed from the explicit Jacobian, which
    !! is itself built column by column from the scalar tangent. So the claim
    !! "vector forward mode already is this product" is verified rather than
    !! asserted.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: SOURCE = &
        "subroutine model(n, x, y1, y2)"//nl// &
        "    integer, intent(in) :: n"//nl// &
        "    real(8), intent(in) :: x(n)"//nl// &
        "    real(8), intent(out) :: y1"//nl// &
        "    real(8), intent(out) :: y2"//nl// &
        "    integer :: i"//nl// &
        "    y1 = 0.0d0"//nl// &
        "    y2 = 0.0d0"//nl// &
        "    do i = 1, n"//nl// &
        "        y1 = y1 + x(i)*x(i)"//nl// &
        "        y2 = y2 + sin(x(i))*x(i)"//nl// &
        "    end do"//nl// &
        "end subroutine model"//nl
    integer :: failures

    failures = 0
    call check(failures)

    if (failures == 0) then
        print *, "test_products_oracle: all cases passed"
    else
        print *, "test_products_oracle: ", failures, " case(s) FAILED"
        error stop 1
    end if

contains

    subroutine check(failures)
        !! Generate the scalar tangent, the vector tangent, and the gradient of
        !! each output, then check the products they are meant to deliver.
        integer, intent(inout) :: failures
        type(fad_result_t) :: jvp, jvp_v, vjp1, vjp2
        character(len=:), allocatable :: dir
        integer :: stat, unit

        dir = "build/oracle_products"
        call execute_command_line("mkdir -p "//dir, exitstat=stat)

        jvp = fad_jvp(SOURCE, ["x"], name="model_jvp")
        jvp_v = fad_jvp(SOURCE, ["x"], name="model_jvp_v", n_directions="n_dir")
        vjp1 = fad_vjp(SOURCE, ["x"], dependent="y1", name="model_vjp1")
        vjp2 = fad_vjp(SOURCE, ["x"], dependent="y2", name="model_vjp2")

        if (.not. (jvp%ok .and. jvp_v%ok .and. vjp1%ok .and. vjp2%ok)) then
            print *, "FAIL products: generation failed"
            if (.not. jvp%ok) print *, "  jvp: ", jvp%message
            if (.not. jvp_v%ok) print *, "  jvp_v: ", jvp_v%message
            if (.not. vjp1%ok) print *, "  vjp1: ", vjp1%message
            if (.not. vjp2%ok) print *, "  vjp2: ", vjp2%message
            failures = failures + 1
            return
        end if

        open (newunit=unit, file=dir//"/primal.f90", status="replace", &
              action="write")
        write (unit, '(a)') "module fad_primal"
        write (unit, '(a)') "    implicit none"
        write (unit, '(a)') "contains"
        write (unit, '(a)') SOURCE
        write (unit, '(a)') "end module fad_primal"
        close (unit)

        open (newunit=unit, file=dir//"/derivs.f90", status="replace", &
              action="write")
        write (unit, '(a)') "module fad_generated"
        write (unit, '(a)') "    implicit none"
        write (unit, '(a)') "contains"
        write (unit, '(a)') jvp%code
        write (unit, '(a)') jvp_v%code
        write (unit, '(a)') vjp1%code
        write (unit, '(a)') vjp2%code
        write (unit, '(a)') "end module fad_generated"
        close (unit)

        open (newunit=unit, file=dir//"/driver.f90", status="replace", &
              action="write")
        write (unit, '(a)') driver_text()
        close (unit)

        call execute_command_line( &
            "cd "//dir//" && gfortran -O2 -o run primal.f90 derivs.f90 "// &
            "driver.f90 > build.log 2>&1", exitstat=stat)
        if (stat /= 0) then
            print *, "FAIL products: generated code did not compile"
            call show_file(dir//"/build.log")
            failures = failures + 1
            return
        end if
        call execute_command_line("cd "//dir//" && ./run > out.txt 2>&1", &
                                  exitstat=stat)
        if (stat /= 0) then
            print *, "FAIL products: mismatch"
            call show_file(dir//"/out.txt")
            failures = failures + 1
            return
        end if
        print *, "pass products_gradient_jacobian_and_linear_uq"
    end subroutine check

    function driver_text() result(text)
        !! Build the Jacobian two ways and propagate a covariance.
        character(len=:), allocatable :: text

        text = &
            "program driver"//nl// &
            "    use fad_primal, only: model"//nl// &
            "    use fad_generated, only: model_jvp, model_jvp_v, &"//nl// &
            "                             model_vjp1, model_vjp2"//nl// &
            "    implicit none"//nl// &
            "    integer, parameter :: n = 6, k = n"//nl// &
            "    real(8) :: x(n), xd(n), xb(n)"//nl// &
            "    real(8) :: y1, y2, y1d, y2d, y1b, y2b"//nl// &
            "    real(8) :: jac(2, n), jac_rev(2, n)"//nl// &
            "    real(8) :: xv(k, n), y1v(k), y2v(k)"//nl// &
            "    real(8) :: l(n, k), jl(2, k), cov_y(2, 2), cov_ref(2, 2)"//nl// &
            "    real(8) :: sigma(n)"//nl// &
            "    integer :: i, j"//nl// &
            "    logical :: bad"//nl// &
            "    bad = .false."//nl// &
            "    do i = 1, n"//nl// &
            "        x(i) = 0.3d0 + 0.17d0*i"//nl// &
            "        sigma(i) = 0.05d0 + 0.01d0*i"//nl// &
            "    end do"//nl// &
            ! Jacobian column by column, scalar forward mode.
            "    do j = 1, n"//nl// &
            "        xd = 0.0d0"//nl// &
            "        xd(j) = 1.0d0"//nl// &
            "        call model_jvp(n, x, xd, y1, y1d, y2, y2d)"//nl// &
            "        jac(1, j) = y1d"//nl// &
            "        jac(2, j) = y2d"//nl// &
            "    end do"//nl// &
            ! Same Jacobian row by row, reverse mode: one sweep per output.
            ! The generated signature is the primal arguments in order, then
            ! the dependent's adjoint, then one adjoint per independent.
            "    y1b = 1.0d0"//nl// &
            "    call model_vjp1(n, x, y1, y2, y1b, xb)"//nl// &
            "    jac_rev(1, :) = xb"//nl// &
            "    y2b = 1.0d0"//nl// &
            "    call model_vjp2(n, x, y1, y2, y2b, xb)"//nl// &
            "    jac_rev(2, :) = xb"//nl// &
            "    if (maxval(abs(jac - jac_rev)) > 1.0d-12) then"//nl// &
            "        print *, 'forward and reverse Jacobians differ:', &"//nl// &
            "            maxval(abs(jac - jac_rev))"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
            ! Linear UQ: seed the tangent block with the covariance factor.
            "    l = 0.0d0"//nl// &
            "    do i = 1, n"//nl// &
            "        l(i, i) = sigma(i)"//nl// &
            "    end do"//nl// &
            "    do j = 1, k"//nl// &
            "        do i = 1, n"//nl// &
            "            xv(j, i) = l(i, j)"//nl// &
            "        end do"//nl// &
            "    end do"//nl// &
            "    call model_jvp_v(k, n, x, xv, y1, y1v, y2, y2v)"//nl// &
            "    jl(1, :) = y1v"//nl// &
            "    jl(2, :) = y2v"//nl// &
            "    cov_y = matmul(jl, transpose(jl))"//nl// &
            "    cov_ref = matmul(matmul(jac, matmul(l, transpose(l))), &"//nl// &
            "                     transpose(jac))"//nl// &
            "    if (maxval(abs(cov_y - cov_ref)) > &"//nl// &
            "        1.0d-12*max(1.0d0, maxval(abs(cov_ref)))) then"//nl// &
            "        print *, 'linear UQ mismatch:', maxval(abs(cov_y - cov_ref))"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
            "    if (bad) error stop 1"//nl// &
            "end program driver"//nl
    end function driver_text

    subroutine show_file(path)
        !! Echo a file to stdout, for failure diagnostics.
        character(len=*), intent(in) :: path
        integer :: unit, ios
        character(len=512) :: buf

        open (newunit=unit, file=path, status="old", action="read", iostat=ios)
        if (ios /= 0) return
        do
            read (unit, '(a)', iostat=ios) buf
            if (ios /= 0) exit
            print *, "    ", trim(buf)
        end do
        close (unit)
    end subroutine show_file

end program test_products_oracle
