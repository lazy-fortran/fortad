program test_lstm_oracle
    !! Compiled numerical oracle for the Enzyme-suite LSTM rank case.
    !!
    !! The generated VJP and gradient-only routine are checked against the
    !! untouched primal with central-difference convergence and against each
    !! other.  The latter also checks the scalar/array temporaries in both
    !! reverse products without making the generated source its own oracle.
    use fortad, only: fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=:), allocatable :: source, generated, driver, dir
    type(fad_result_t) :: vjp, grad
    integer :: unit, stat

    source = lstm_source()
    vjp = fad_vjp(source, [character(len=1) :: "z"], dependent="y", &
        name="lstm_vjp")
    grad = fad_vjp(source, [character(len=1) :: "z"], dependent="y", &
        name="lstm_grad", with_primal=.false.)
    if (.not. vjp%ok .or. .not. grad%ok) then
        print *, "FAIL LSTM derivative generation"
        if (allocated(vjp%message)) print *, trim(vjp%message)
        if (allocated(grad%message)) print *, trim(grad%message)
        error stop 1
    end if

    dir = "build/oracle_lstm"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop 2

    generated = "module lstm_generated"//nl// &
        "    implicit none"//nl// &
        "contains"//nl//source//vjp%code//grad%code// &
        "end module lstm_generated"//nl
    open (newunit=unit, file=dir//"/generated.f90", status="replace", &
        action="write")
    write (unit, '(a)') generated
    close (unit)

    driver = driver_source()
    open (newunit=unit, file=dir//"/driver.f90", status="replace", &
        action="write")
    write (unit, '(a)') driver
    close (unit)

    call execute_command_line( &
        "gfortran -std=f2018 -O2 -o "//dir//"/run "// &
        dir//"/generated.f90 "//dir//"/driver.f90 > "// &
        dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL LSTM generated source did not compile"
        call show_file(dir//"/build.log")
        error stop 3
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL LSTM numerical oracle"
        call show_file(dir//"/out.txt")
        error stop 4
    end if

    print *, "test_lstm_oracle: all cases passed"

contains

    function lstm_source() result(text)
        character(len=:), allocatable :: text

        text = "subroutine lstm(n, z, y)"//nl// &
            "    integer, intent(in) :: n"//nl// &
            "    real(8), intent(in) :: z(n)"//nl// &
            "    real(8), intent(out) :: y"//nl// &
            "    real(8) :: cell, change, forget, hidden, ingate, outgate"//nl// &
            "    integer :: i"//nl// &
            "    cell = 0.2d0"//nl// &
            "    hidden = -0.1d0"//nl// &
            "    y = 0.0d0"//nl// &
            "    do i = 1, n"//nl// &
            "        forget = 1.0d0/(1.0d0 + exp(-(0.7d0*z(i) + 0.2d0)))"//nl// &
            "        ingate = 1.0d0/(1.0d0 + exp(-(-0.4d0*hidden + 0.1d0)))"//nl// &
            "        outgate = 1.0d0/(1.0d0 + exp(-(0.5d0*z(i) - 0.3d0)))"//nl// &
            "        change = tanh(0.8d0*hidden + 0.6d0*z(i))"//nl// &
            "        cell = cell*forget + ingate*change"//nl// &
            "        hidden = outgate*tanh(cell)"//nl// &
            "        y = y + log(2.0d0 + exp(hidden)) - 0.1d0*hidden"//nl// &
            "    end do"//nl// &
            "    y = y/real(n, 8)"//nl// &
            "end subroutine lstm"//nl
    end function lstm_source

    function driver_source() result(text)
        character(len=:), allocatable :: text

        text = "program driver"//nl// &
            "    use lstm_generated, only: lstm, lstm_vjp, lstm_grad"//nl// &
            "    implicit none"//nl// &
            "    integer, parameter :: n = 19"//nl// &
            "    real(8) :: z(n), zd(n), zb(n), gb(n), zp(n), zm(n)"//nl// &
            "    real(8) :: y, yp, ym, y0, h, e1, e2, dot_forward"//nl// &
            "    integer :: i"//nl// &
            "    do i = 1, n"//nl// &
            "        z(i) = 0.17d0*sin(0.31d0*i) - 0.03d0*i"//nl// &
            "        zd(i) = 0.11d0*cos(0.23d0*i) - 0.02d0"//nl// &
            "    end do"//nl// &
            "    call lstm(n, z, y0)"//nl// &
            "    call lstm_vjp(n, z, y, 1.0d0, zb)"//nl// &
            "    call lstm_grad(n, z, 1.0d0, gb)"//nl// &
            "    if (abs(y-y0) > 1.0d-13) error stop 1"//nl// &
            "    if (maxval(abs(zb-gb)) > 1.0d-13) error stop 2"//nl// &
            "    dot_forward = sum(zb*zd)"//nl// &
            "    h = 1.0d-3"//nl// &
            "    zp = z + h*zd"//nl// &
            "    zm = z - h*zd"//nl// &
            "    call lstm(n, zp, yp)"//nl// &
            "    call lstm(n, zm, ym)"//nl// &
            "    e1 = abs(dot_forward-(yp-ym)/(2.0d0*h))"//nl// &
            "    h = 5.0d-4"//nl// &
            "    zp = z + h*zd"//nl// &
            "    zm = z - h*zd"//nl// &
            "    call lstm(n, zp, yp)"//nl// &
            "    call lstm(n, zm, ym)"//nl// &
            "    e2 = abs(dot_forward-(yp-ym)/(2.0d0*h))"//nl// &
            "    if (e2 > 1.0d-7 .or. e2 > 0.75d0*e1) error stop 3"//nl// &
            "    print '(a,1x,es12.4,1x,es12.4)', 'lstm_fd_errors', e1, e2"//nl// &
            "end program driver"//nl
    end function driver_source

    subroutine show_file(path)
        character(len=*), intent(in) :: path
        character(len=512) :: line
        integer :: file_unit, ios

        open (newunit=file_unit, file=path, status="old", action="read", &
            iostat=ios)
        if (ios /= 0) return
        do
            read (file_unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            print *, "    ", trim(line)
        end do
        close (file_unit)
    end subroutine show_file

end program test_lstm_oracle
