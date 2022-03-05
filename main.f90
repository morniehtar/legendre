#define _pr 8

program output
    implicit none

    call wrZeros(4, 6)

contains

    recursive function P(n, x) result(res)
        integer, intent(in) :: n
        real(_pr), intent(in) :: x
        real(_pr) :: res

        if (n .eq. 0) then
            res = 1
        else if (n .eq. 1) then
            res = x
        else if (n .eq. 2) then
            res = (3*x**2)/2-5.d-1
        else
            res = ((2*n-1)*x*P(n-1, x)-(n-1)*P(n-2, x))/n
        end if
    end function P

    function dP(n, x) result(res)
        integer, intent(in) :: n
        real(_pr), intent(in) :: x
        real(_pr) :: res

        res = (P(n-1, x)-x*P(n, x))*n/(1-x**2)
    end function dP

    recursive function zero(k, i, n) result(res)
        integer, intent(in) :: k, i, n
        real(_pr) :: res
        real(_pr), parameter :: pi = 4*atan(real(1))

        if (k .eq. 0) then
            res = cos(pi*(4*i-1)/(4*n+2))
        else if (k .gt. 0) then
            res = zero(k-1, i, n) - P(n, zero(k-1, i, n))/dP(n, zero(k-1, i, n))
        end if
    end function zero

    subroutine prZeros(k, n)
        integer, intent(in) :: k, n
        integer :: j
        character(len=8) :: str

        write(str, "(I8)") n

        write(*, fmt="(a)", advance='yes') "For " // trim(adjustl(str)) // "'th power Legendre polynomial:"
        do j = 1, n
            write(str, "(I8)") j
            if (j == 1) then
                write(*, fmt="(a)", advance='no') "  " // trim(adjustl(str)) // "'st root: "
            else if (j == 2) then
                write(*, fmt="(a)", advance='no') "  " // trim(adjustl(str)) // "'nd root: "
            else if (j == 3) then
                write(*, fmt="(a)", advance='no') "  " // trim(adjustl(str)) // "'rd root: "
            else
                write(*, fmt="(a)", advance='no') "  " // trim(adjustl(str)) // "'th root: "
            end if
            print *, zero(k, j, n)
        end do

    end subroutine prZeros

    subroutine wrZeros(k, n)
        integer, intent(in) :: k, n
        integer :: j

        open(unit=1, file="output.dat")
        do j = 1, n
            write(1, *) zero(k, j, n)
        end do
    end subroutine wrZeros

end program output

