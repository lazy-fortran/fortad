program test_library_rules_oracle
    !! Independent oracle for representative FFT, quadrature, interpolation,
    !! and special-function structured rules.
    !!
    !! Each primal operation is opaque to fortad. Its tangent and adjoint
    !! callbacks are registered as statement rules, then checked against fresh
    !! complete evaluations of the composite kernel.
    use fortad, only: fad_jvp, fad_vjp, fad_add_call_rule, fad_clear_rules, &
                      fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: SOURCE = &
        "subroutine k(signal, nodes, values, alpha, spec_c, spec_s, q, y, z, s)"//nl// &
        "    real(8), intent(in) :: signal(8)"//nl// &
        "    real(8), intent(in) :: nodes(4)"//nl// &
        "    real(8), intent(in) :: values(4)"//nl// &
        "    real(8), intent(in) :: alpha"//nl// &
        "    real(8), intent(out) :: spec_c(8)"//nl// &
        "    real(8), intent(out) :: spec_s(8)"//nl// &
        "    real(8), intent(out) :: q"//nl// &
        "    real(8), intent(out) :: y"//nl// &
        "    real(8), intent(out) :: z"//nl// &
        "    real(8), intent(out) :: s"//nl// &
        "    call fft8_r2c(signal, spec_c, spec_s)"//nl// &
        "    call quad4(values, q)"//nl// &
        "    call interp4(nodes, values, alpha, y)"//nl// &
        "    call special_erf(alpha, z)"//nl// &
        "    s = sum(spec_c) + sum(spec_s) + q + y + z"//nl// &
        "end subroutine k"//nl
    type(fad_result_t) :: jvp, vjp
    integer :: stat, unit
    character(len=:), allocatable :: dir

    call fad_clear_rules()
    call register_rules(stat)
    if (stat /= 0) error stop "library rule registration failed"

    jvp = fad_jvp(SOURCE, [character(len=7) :: "signal", "nodes", "values", &
                           "alpha"], name="k_jvp")
    vjp = fad_vjp(SOURCE, [character(len=7) :: "signal", "nodes", "values", &
                          "alpha"], dependent="s", name="k_vjp")
    if (.not. jvp%ok) then
        print *, "FAIL library forward generation: ", jvp%message
        error stop 1
    end if
    if (.not. vjp%ok) then
        print *, "FAIL library reverse generation: ", vjp%message
        error stop 1
    end if

    dir = "build/oracle_library_rules"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    open (newunit=unit, file=dir//"/support.f90", status="replace", &
          action="write")
    write (unit, '(a)') support_text()
    close (unit)
    open (newunit=unit, file=dir//"/derivs.f90", status="replace", &
          action="write")
    write (unit, '(a)') "module fad_generated"
    write (unit, '(a)') "    use library_support"
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
        "driver.f90 > build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL library-rule generated code did not compile"
        call show_file(dir//"/build.log")
        error stop 1
    end if
    call execute_command_line("cd "//dir//" && ./run > out.txt 2>&1", &
                              exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL library-rule oracle"
        call show_file(dir//"/out.txt")
        error stop 1
    end if
    print *, "test_library_rules_oracle: all cases passed"

contains

    subroutine register_rules(stat)
        integer, intent(out) :: stat

        stat = 0
        call fad_add_call_rule("fft8_r2c", 3, &
            tangent=[character(len=256) :: &
                     "call fft8_r2c_tangent($1d, $2d, $3d)"], &
            adjoint=[character(len=256) :: &
                     "call fft8_r2c_adjoint($2b, $3b, $1b)"], stat=stat)
        if (stat /= 0) return
        call fad_add_call_rule("quad4", 2, &
            tangent=[character(len=256) :: &
                     "call quad4_tangent($1d, $2d)"], &
            adjoint=[character(len=256) :: &
                     "call quad4_adjoint($2b, $1b)"], stat=stat)
        if (stat /= 0) return
        call fad_add_call_rule("interp4", 4, &
            tangent=[character(len=256) :: &
                     "call interp4_tangent($1, $1d, $2, $2d, $3, $3d, $4, $4d)"], &
            adjoint=[character(len=256) :: &
                     "call interp4_adjoint($1, $2, $3, $4b, $1b, $2b, $3b)"], &
            stat=stat)
        if (stat /= 0) return
        call fad_add_call_rule("special_erf", 2, &
            tangent=[character(len=256) :: &
                     "call special_erf_tangent($1, $1d, $2d)"], &
            adjoint=[character(len=256) :: &
                     "call special_erf_adjoint($1, $2b, $1b)"], stat=stat)
    end subroutine register_rules

    function support_text() result(text)
        character(len=:), allocatable :: text

        text = &
            "module library_support"//nl// &
            "    implicit none"//nl// &
            "    real(8), parameter :: pi = 3.14159265358979323846d0"//nl// &
            "    real(8), parameter :: weights(4) = [1.0d0, 2.0d0, 2.0d0, 1.0d0]"//nl// &
            "contains"//nl// &
            "    subroutine fft8_r2c(signal, cosine, sine)"//nl// &
            "        real(8), intent(in) :: signal(8)"//nl// &
            "        real(8), intent(out) :: cosine(8), sine(8)"//nl// &
            "        integer :: k, j"//nl// &
            "        real(8) :: angle"//nl// &
            "        cosine = 0.0d0; sine = 0.0d0"//nl// &
            "        do k = 1, 8"//nl// &
            "            do j = 1, 8"//nl// &
            "                angle = 2.0d0*pi*real((k-1)*(j-1),8)/8.0d0"//nl// &
            "                cosine(k) = cosine(k) + signal(j)*cos(angle)"//nl// &
            "                sine(k) = sine(k) + signal(j)*sin(angle)"//nl// &
            "            end do"//nl// &
            "        end do"//nl// &
            "    end subroutine fft8_r2c"//nl// &
            "    subroutine fft8_r2c_tangent(signal_d, cosine_d, sine_d)"//nl// &
            "        real(8), intent(in) :: signal_d(8)"//nl// &
            "        real(8), intent(out) :: cosine_d(8), sine_d(8)"//nl// &
            "        call fft8_r2c(signal_d, cosine_d, sine_d)"//nl// &
            "    end subroutine fft8_r2c_tangent"//nl// &
            "    subroutine fft8_r2c_adjoint(cosine_b, sine_b, signal_b)"//nl// &
            "        real(8), intent(in) :: cosine_b(8), sine_b(8)"//nl// &
            "        real(8), intent(inout) :: signal_b(8)"//nl// &
            "        integer :: k, j"//nl// &
            "        real(8) :: angle"//nl// &
            "        do k = 1, 8"//nl// &
            "            do j = 1, 8"//nl// &
            "                angle = 2.0d0*pi*real((k-1)*(j-1),8)/8.0d0"//nl// &
            "                signal_b(j) = signal_b(j) + cosine_b(k)*cos(angle) + sine_b(k)*sin(angle)"//nl// &
            "            end do"//nl// &
            "        end do"//nl// &
            "    end subroutine fft8_r2c_adjoint"//nl// &
            "    subroutine quad4(values, q)"//nl// &
            "        real(8), intent(in) :: values(4)"//nl// &
            "        real(8), intent(out) :: q"//nl// &
            "        q = sum(weights*values)"//nl// &
            "    end subroutine quad4"//nl// &
            "    subroutine quad4_tangent(values_d, q_d)"//nl// &
            "        real(8), intent(in) :: values_d(4)"//nl// &
            "        real(8), intent(out) :: q_d"//nl// &
            "        q_d = sum(weights*values_d)"//nl// &
            "    end subroutine quad4_tangent"//nl// &
            "    subroutine quad4_adjoint(q_b, values_b)"//nl// &
            "        real(8), intent(in) :: q_b"//nl// &
            "        real(8), intent(inout) :: values_b(4)"//nl// &
            "        values_b = values_b + q_b*weights"//nl// &
            "    end subroutine quad4_adjoint"//nl// &
            "    subroutine interpolation_parts(nodes, alpha, w, wa, wn)"//nl// &
            "        real(8), intent(in) :: nodes(4), alpha"//nl// &
            "        real(8), intent(out) :: w(4), wa(4), wn(4,4)"//nl// &
            "        integer :: i, j, m"//nl// &
            "        w = 1.0d0; wa = 0.0d0; wn = 0.0d0"//nl// &
            "        do i = 1, 4"//nl// &
            "            do j = 1, 4"//nl// &
            "                if (j /= i) w(i) = w(i)*(alpha-nodes(j))/(nodes(i)-nodes(j))"//nl// &
            "            end do"//nl// &
            "            do j = 1, 4"//nl// &
            "                if (j /= i) wa(i) = wa(i) + 1.0d0/(alpha-nodes(j))"//nl// &
            "            end do"//nl// &
            "            wa(i) = w(i)*wa(i)"//nl// &
            "            do m = 1, 4"//nl// &
            "                if (m == i) then"//nl// &
            "                    do j = 1, 4"//nl// &
            "                        if (j /= i) wn(i,m) = wn(i,m) - w(i)/(nodes(i)-nodes(j))"//nl// &
            "                    end do"//nl// &
            "                else"//nl// &
            "                    wn(i,m) = w(i)*(-1.0d0/(alpha-nodes(m)) + 1.0d0/(nodes(i)-nodes(m)))"//nl// &
            "                end if"//nl// &
            "            end do"//nl// &
            "        end do"//nl// &
            "    end subroutine interpolation_parts"//nl// &
            "    subroutine interp4(nodes, values, alpha, y)"//nl// &
            "        real(8), intent(in) :: nodes(4), values(4), alpha"//nl// &
            "        real(8), intent(out) :: y"//nl// &
            "        real(8) :: w(4), wa(4), wn(4,4)"//nl// &
            "        call interpolation_parts(nodes, alpha, w, wa, wn)"//nl// &
            "        y = sum(w*values)"//nl// &
            "    end subroutine interp4"//nl// &
            "    subroutine interp4_tangent(nodes, nodes_d, values, values_d, alpha, alpha_d, y, y_d)"//nl// &
            "        real(8), intent(in) :: nodes(4), nodes_d(4), values(4), values_d(4), alpha, alpha_d"//nl// &
            "        real(8), intent(in) :: y"//nl// &
            "        real(8), intent(out) :: y_d"//nl// &
            "        real(8) :: w(4), wa(4), wn(4,4), i_alpha, i_nodes"//nl// &
            "        integer :: i, m"//nl// &
            "        call interpolation_parts(nodes, alpha, w, wa, wn)"//nl// &
            "        y_d = sum(w*values_d) + alpha_d*sum(wa*values)"//nl// &
            "        do m = 1, 4"//nl// &
            "            i_nodes = 0.0d0"//nl// &
            "            do i = 1, 4"//nl// &
            "                i_nodes = i_nodes + wn(i,m)*values(i)"//nl// &
            "            end do"//nl// &
            "            y_d = y_d + nodes_d(m)*i_nodes"//nl// &
            "        end do"//nl// &
            "    end subroutine interp4_tangent"//nl// &
            "    subroutine interp4_adjoint(nodes, values, alpha, y_b, nodes_b, values_b, alpha_b)"//nl// &
            "        real(8), intent(in) :: nodes(4), values(4), alpha, y_b"//nl// &
            "        real(8), intent(inout) :: nodes_b(4), values_b(4), alpha_b"//nl// &
            "        real(8) :: w(4), wa(4), wn(4,4), i_nodes"//nl// &
            "        integer :: i, m"//nl// &
            "        call interpolation_parts(nodes, alpha, w, wa, wn)"//nl// &
            "        values_b = values_b + y_b*w"//nl// &
            "        alpha_b = alpha_b + y_b*sum(wa*values)"//nl// &
            "        do m = 1, 4"//nl// &
            "            i_nodes = 0.0d0"//nl// &
            "            do i = 1, 4"//nl// &
            "                i_nodes = i_nodes + wn(i,m)*values(i)"//nl// &
            "            end do"//nl// &
            "            nodes_b(m) = nodes_b(m) + y_b*i_nodes"//nl// &
            "        end do"//nl// &
            "    end subroutine interp4_adjoint"//nl// &
            "    subroutine special_erf(alpha, z)"//nl// &
            "        real(8), intent(in) :: alpha"//nl// &
            "        real(8), intent(out) :: z"//nl// &
            "        z = erf(alpha)"//nl// &
            "    end subroutine special_erf"//nl// &
            "    subroutine special_erf_tangent(alpha, alpha_d, z_d)"//nl// &
            "        real(8), intent(in) :: alpha, alpha_d"//nl// &
            "        real(8), intent(out) :: z_d"//nl// &
            "        z_d = 1.1283791670955126d0*exp(-alpha*alpha)*alpha_d"//nl// &
            "    end subroutine special_erf_tangent"//nl// &
            "    subroutine special_erf_adjoint(alpha, z_b, alpha_b)"//nl// &
            "        real(8), intent(in) :: alpha, z_b"//nl// &
            "        real(8), intent(inout) :: alpha_b"//nl// &
            "        alpha_b = alpha_b + z_b*1.1283791670955126d0*exp(-alpha*alpha)"//nl// &
            "    end subroutine special_erf_adjoint"//nl// &
            "    subroutine evaluate(signal, nodes, values, alpha, spec_c, spec_s, q, y, z, s)"//nl// &
            "        real(8), intent(in) :: signal(8), nodes(4), values(4), alpha"//nl// &
            "        real(8), intent(out) :: spec_c(8), spec_s(8), q, y, z, s"//nl// &
            "        call fft8_r2c(signal, spec_c, spec_s)"//nl// &
            "        call quad4(values, q)"//nl// &
            "        call interp4(nodes, values, alpha, y)"//nl// &
            "        call special_erf(alpha, z)"//nl// &
            "        s = sum(spec_c) + sum(spec_s) + q + y + z"//nl// &
            "    end subroutine evaluate"//nl// &
            "end module library_support"//nl
    end function support_text

    function driver_text() result(text)
        character(len=:), allocatable :: text

        text = &
            "program driver"//nl// &
            "    use library_support, only: evaluate"//nl// &
            "    use fad_generated, only: k_jvp, k_vjp"//nl// &
            "    implicit none"//nl// &
            "    real(8) :: signal(8), signal_d(8), nodes(4), nodes_d(4)"//nl// &
            "    real(8) :: values(4), values_d(4), alpha, alpha_d"//nl// &
            "    real(8) :: sc(8), scd(8), ss(8), ssd(8), q, qd, y, yd, z, zd"//nl// &
            "    real(8) :: s, sd, sb, signal_b(8), nodes_b(4), values_b(4), alpha_b"//nl// &
            "    real(8) :: pp_signal(8), mm_signal(8), pp_nodes(4), mm_nodes(4)"//nl// &
            "    real(8) :: pp_values(4), mm_values(4), pp_alpha, mm_alpha"//nl// &
            "    real(8) :: sp, sm, fd, h, err"//nl// &
            "    integer :: i"//nl// &
            "    signal = [0.2d0, -0.4d0, 0.7d0, 0.1d0, -0.3d0, 0.5d0, 0.9d0, -0.2d0]"//nl// &
            "    signal_d = [0.1d0, -0.2d0, 0.3d0, -0.1d0, 0.2d0, 0.4d0, -0.3d0, 0.2d0]"//nl// &
            "    nodes = [-1.0d0, -0.2d0, 0.4d0, 1.2d0]"//nl// &
            "    nodes_d = [0.02d0, -0.01d0, 0.03d0, -0.02d0]"//nl// &
            "    values = [1.1d0, -0.7d0, 0.4d0, 1.8d0]"//nl// &
            "    values_d = [-0.2d0, 0.3d0, -0.1d0, 0.25d0]"//nl// &
            "    alpha = 0.1d0; alpha_d = -0.15d0; h = 1.0d-6"//nl// &
            "    call k_jvp(signal, signal_d, nodes, nodes_d, values, values_d, alpha, alpha_d, sc, scd, ss, ssd, &"//nl// &
            "        q, qd, y, yd, z, zd, s, sd)"//nl// &
            "    pp_signal = signal+h*signal_d; mm_signal = signal-h*signal_d"//nl// &
            "    pp_nodes = nodes+h*nodes_d; mm_nodes = nodes-h*nodes_d"//nl// &
            "    pp_values = values+h*values_d; mm_values = values-h*values_d"//nl// &
            "    pp_alpha = alpha+h*alpha_d; mm_alpha = alpha-h*alpha_d"//nl// &
            "    call evaluate(pp_signal, pp_nodes, pp_values, pp_alpha, sc, ss, q, y, z, sp)"//nl// &
            "    call evaluate(mm_signal, mm_nodes, mm_values, mm_alpha, sc, ss, q, y, z, sm)"//nl// &
            "    fd = (sp-sm)/(2.0d0*h); err = abs(sd-fd)/max(1.0d0,abs(sd))"//nl// &
            "    if (err > 1.0d-7) then; print *, 'jvp error', err, sd, fd; error stop 2; end if"//nl// &
            "    sb = 1.0d0"//nl// &
            "    call k_vjp(signal, nodes, values, alpha, sc, ss, q, y, z, s, sb, signal_b, nodes_b, values_b, alpha_b)"//nl// &
            "    err = abs(sd-sum(signal_b*signal_d)-sum(nodes_b*nodes_d)-sum(values_b*values_d)-&"//nl// &
            "        alpha_b*alpha_d)/max(1.0d0,abs(sd))"//nl// &
            "    if (err > 1.0d-8) then; print *, 'vjp identity error', err; error stop 3; end if"//nl// &
            "    do i = 1, 8"//nl// &
            "        pp_signal = signal; mm_signal = signal"//nl// &
            "        pp_signal(i)=pp_signal(i)+h; mm_signal(i)=mm_signal(i)-h"//nl// &
            "        call evaluate(pp_signal,nodes,values,alpha,sc,ss,q,y,z,sp)"//nl// &
            "        call evaluate(mm_signal,nodes,values,alpha,sc,ss,q,y,z,sm)"//nl// &
            "        fd=(sp-sm)/(2.0d0*h); if (abs(signal_b(i)-fd)>1.0d-7) error stop 4"//nl// &
            "    end do"//nl// &
            "    do i = 1, 4"//nl// &
            "        pp_nodes = nodes; mm_nodes = nodes; pp_nodes(i)=pp_nodes(i)+h; mm_nodes(i)=mm_nodes(i)-h"//nl// &
            "        call evaluate(signal,pp_nodes,values,alpha,sc,ss,q,y,z,sp)"//nl// &
            "        call evaluate(signal,mm_nodes,values,alpha,sc,ss,q,y,z,sm)"//nl// &
            "        fd=(sp-sm)/(2.0d0*h); if (abs(nodes_b(i)-fd)>1.0d-7) error stop 5"//nl// &
            "        pp_values = values; mm_values = values; pp_values(i)=pp_values(i)+h; mm_values(i)=mm_values(i)-h"//nl// &
            "        call evaluate(signal,nodes,pp_values,alpha,sc,ss,q,y,z,sp)"//nl// &
            "        call evaluate(signal,nodes,mm_values,alpha,sc,ss,q,y,z,sm)"//nl// &
            "        fd=(sp-sm)/(2.0d0*h); if (abs(values_b(i)-fd)>1.0d-7) error stop 6"//nl// &
            "    end do"//nl// &
            "    pp_alpha=alpha+h; mm_alpha=alpha-h"//nl// &
            "    call evaluate(signal,nodes,values,pp_alpha,sc,ss,q,y,z,sp)"//nl// &
            "    call evaluate(signal,nodes,values,mm_alpha,sc,ss,q,y,z,sm)"//nl// &
            "    fd=(sp-sm)/(2.0d0*h); if (abs(alpha_b*1.0d0-fd)>1.0d-7) error stop 7"//nl// &
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

end program test_library_rules_oracle
