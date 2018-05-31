module mult
    contains
        subroutine mm(first, second, multiply, status)
            implicit none
            real(kind= 8),intent(in):: first(:,:) ! pierwsza macierz
            real(kind= 8),intent(in):: second(: ,:) ! druga macierz
            real(kind= 8),intent(out):: multiply(:,:) ! macierz wynikowa
            integer(kind= 4),intent(out):: status ! kod błędu, 0 gdy OK
            integer(kind = 4) :: i, j, k
            integer(kind=4) :: f1, f2, s1, s2, m1, m2
            real (kind = 8) :: sum

            f1=size(first, dim=1)
            f2=size(first, dim=2)
            s1=size(second, dim=1)
            s2=size(second, dim=2)
            m1=size(multiply, dim=1)
            m2=size(multiply, dim=2)
            if(f2 == s1 .AND. f1==m1 .AND. s2==m2) then
                sum = 0.d0
                multiply = 0.d0

                do j = 1, f1
                    do k = 1, f2
                        do i = 1, s2
                            multiply(i, j) = multiply(i, j) + first(i, k) * second(k, j)
                        end do
                    end do
                end do
                status = 0.d0
                else
                status=1.d0
            end if
        end subroutine
end module