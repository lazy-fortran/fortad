program test_assumed_rank_select_rank_oracle
    !! Independent compiled oracle for the one-arm assumed-rank slice.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=:), allocatable :: source, dir
    type(fad_result_t) :: jvp, vjp, refused
    integer :: stat

    source = positive_source()
    jvp = fad_jvp(source, [character(len=6) :: "values"], name="rank_jvp")
    if (.not. jvp%ok) error stop "assumed-rank JVP generation failed: "//jvp%message
    vjp = fad_vjp(source, [character(len=6) :: "values"], dependent="y", &
        name="rank_vjp")
    if (.not. vjp%ok) error stop "assumed-rank VJP generation failed: "//vjp%message

    dir = "build/oracle/assumed_rank_select_rank"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create assumed-rank oracle directory"
    call write_file(dir//"/primal.f90", "module rank_primal"//nl// &
        "contains"//nl//source//"end module rank_primal"//nl)
    call write_file(dir//"/tangent.f90", "module rank_tangent"//nl// &
        "contains"//nl//jvp%code//"end module rank_tangent"//nl)
    call write_file(dir//"/derivatives.f90", "module rank_derivatives"//nl// &
        "contains"//nl//vjp%code//"end module rank_derivatives"//nl)
    call write_file(dir//"/driver.f90", driver_text())
    call execute_command_line( &
        "gfortran -std=f2018 -pedantic-errors -O2 -J"//dir//" -I"//dir// &
        " -o "//dir//"/run "//dir//"/primal.f90 "//dir//"/tangent.f90 "//dir// &
        "/derivatives.f90 "//dir//"/driver.f90 > "//dir//"/build.log 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/build.log")
        error stop "generated assumed-rank derivative source did not compile"
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/out.txt")
        error stop "assumed-rank independent oracle failed"
    end if

    refused = fad_jvp(default_source(), [character(len=6) :: "values"])
    call require_refusal(refused, "exactly one explicit RANK (1) arm", &
        "rank default")
    refused = fad_jvp(star_source(), [character(len=6) :: "values"])
    call require_refusal(refused, "RANK (*)", "rank star")
    refused = fad_jvp(rank_two_source(), [character(len=6) :: "values"])
    call require_refusal(refused, "only rank-one dispatch", "rank two")
    refused = fad_jvp(pointer_source(), [character(len=6) :: "values"])
    call require_refusal(refused, "pointer", "pointer selector")
    refused = fad_jvp(allocatable_source(), [character(len=6) :: "values"])
    call require_refusal(refused, "allocatable", "allocatable selector")
    refused = fad_jvp(multiple_source(), [character(len=6) :: "values"])
    call require_refusal(refused, "exactly one explicit RANK (1) arm", &
        "multiple arms")
    refused = fad_jvp(unresolved_source(), [character(len=6) :: "values"])
    call require_refusal(refused, "SELECT RANK", "unresolved selector")
    refused = fad_jvp(global_source(), [character(len=6) :: "values"])
    call require_refusal(refused, "global mutable", "global mutable state")

    print '(a)', "test_assumed_rank_select_rank_oracle: all cases passed"

contains

    function positive_source() result(text)
        character(len=:), allocatable :: text

        text = "subroutine rank_kernel(values, y)"//nl// &
            "    real(8), intent(in) :: values(..)"//nl// &
            "    real(8), intent(out) :: y"//nl// &
            "    select rank (values)"//nl// &
            "    rank (1)"//nl// &
            "        y = values(1)*values(1) + 2.0d0*values(1)"//nl// &
            "    end select"//nl// &
            "end subroutine rank_kernel"//nl
    end function positive_source

    function default_source() result(text)
        character(len=:), allocatable :: text
        text = positive_source()
        text = replace(text, "    end select", "    rank default"//nl// &
            "        y = 0.0d0"//nl//"    end select")
    end function default_source

    function star_source() result(text)
        character(len=:), allocatable :: text
        text = positive_source()
        text = replace(text, "    rank (1)", "    rank (*)")
    end function star_source

    function rank_two_source() result(text)
        character(len=:), allocatable :: text
        text = positive_source()
        text = replace(text, "    rank (1)", "    rank (2)")
    end function rank_two_source

    function pointer_source() result(text)
        character(len=:), allocatable :: text
        text = replace(positive_source(), "real(8), intent(in) :: values(..)", &
            "real(8), pointer, intent(in) :: values(..)")
    end function pointer_source

    function allocatable_source() result(text)
        character(len=:), allocatable :: text
        text = replace(positive_source(), "real(8), intent(in) :: values(..)", &
            "real(8), allocatable, intent(in) :: values(..)")
    end function allocatable_source

    function multiple_source() result(text)
        character(len=:), allocatable :: text
        text = replace(positive_source(), "    end select", "    rank (2)"//nl// &
            "        y = 0.0d0"//nl//"    end select")
    end function multiple_source

    function unresolved_source() result(text)
        character(len=:), allocatable :: text
        text = replace(positive_source(), "select rank (values)", &
            "select rank (missing_values)")
    end function unresolved_source

    function global_source() result(text)
        character(len=:), allocatable :: text
        text = "module rank_global"//nl//"    real(8), save :: bias = 1.0d0"//nl// &
            "contains"//nl//replace(positive_source(), "2.0d0*values(1)", &
            "2.0d0*values(1) + bias")//"end module rank_global"//nl
    end function global_source

    function driver_text() result(text)
        character(len=:), allocatable :: text
        text = "program driver"//nl// &
            "    use rank_primal, only: rank_kernel"//nl// &
            "    use rank_tangent, only: rank_jvp"//nl// &
            "    use rank_derivatives, only: rank_vjp"//nl// &
            "    implicit none"//nl// &
            "    real(8) :: x(1), xd(1), xb(1), y, yd, yp, ym, seed, h"//nl// &
            "    real(8) :: expected"//nl// &
            "    x(1) = 0.73d0"//nl//"    xd(1) = -0.41d0"//nl// &
            "    call rank_jvp(x, xd, y, yd)"//nl// &
            "    expected = (2.0d0*x(1) + 2.0d0)*xd(1)"//nl// &
            "    if (abs(yd - expected) > 1.0d-12) error stop 1"//nl// &
            "    h = 1.0d-6"//nl//"    call rank_kernel(x + h*xd, yp)"//nl// &
            "    call rank_kernel(x - h*xd, ym)"//nl// &
            "    if (abs((yp - ym)/(2.0d0*h) - expected) > 1.0d-7) error stop 2"//nl// &
            "    seed = 0.67d0"//nl//"    call rank_vjp(x, y, seed, xb)"//nl// &
            "    if (abs(xb(1) - seed*(2.0d0*x(1) + 2.0d0)) > 1.0d-12) error stop 3"//nl// &
            "    if (abs(seed*yd - sum(xb*xd)) > 1.0d-12) error stop 4"//nl// &
            "end program driver"//nl
    end function driver_text

    function replace(text, old, new) result(out)
        character(len=*), intent(in) :: text, old, new
        character(len=:), allocatable :: out
        integer :: at
        at = index(text, old)
        if (at == 0) then
            out = text
        else
            out = text(:at - 1)//new//text(at + len(old):)
        end if
    end function replace

    subroutine write_file(path, text)
        character(len=*), intent(in) :: path, text
        integer :: file_unit
        open (newunit=file_unit, file=path, status="replace", action="write")
        write (file_unit, '(a)') text
        close (file_unit)
    end subroutine write_file

    subroutine require_refusal(result, reason, label)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: reason, label
        if (result%ok .or. .not. allocated(result%message) .or. &
            index(result%message, reason) == 0) then
            error stop "accepted or unnamed refusal: "//trim(label)
        end if
    end subroutine require_refusal

    subroutine show_file(path)
        character(len=*), intent(in) :: path
        character(len=512) :: line
        integer :: file_unit, ios
        open (newunit=file_unit, file=path, status="old", action="read", iostat=ios)
        if (ios /= 0) return
        do
            read (file_unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            print '(a)', trim(line)
        end do
        close (file_unit)
    end subroutine show_file

end program test_assumed_rank_select_rank_oracle
