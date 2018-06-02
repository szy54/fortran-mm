!#define DOT
!#define CACHE
                    !i5 4200M 3M cache
module mult
    contains
        subroutine mm(first, second, multiply, status)
            implicit none
            real(kind= 8),intent(in):: first(:,:) ! pierwsza macierz
            real(kind= 8),intent(in):: second(: ,:) ! druga macierz
            real(kind= 8),intent(out):: multiply(:,:) ! macierz wynikowa
            integer(kind= 4),intent(out):: status ! kod błędu, 0 gdy OK
            integer(kind = 4) :: i, j, k, ii, jj, ichunk
            integer(kind=4) :: f1, f2, s1, s2, m1, m2
            ichunk=512 !i54200M - 3M cache

            f1=size(first, dim=1)
            f2=size(first, dim=2)
            s1=size(second, dim=1)
            s2=size(second, dim=2)
            m1=size(multiply, dim=1)
            m2=size(multiply, dim=2)

#ifdef DOT
                    !write(*,*) "using dot"
#else
                    !write(*,*) "not using dot"
#endif

#ifdef CACHE
                    !write(*,*) "using cache"
#else
                    !write(*,*) "not using cache"
#endif
            if(f2 == s1 .AND. f1==m1 .AND. s2==m2) then

                multiply = 0.d0
#ifdef CACHE
                    do ii = 1, f1, ichunk
                        do jj = 1, s2, ichunk
                            do i = ii, min(ii + ichunk - 1, f1)! columns in mmultiply
                                do j = jj, min(jj + ichunk - 1, s2) ! rows in multiply
#ifdef DOT
                                        multiply(i, j) = dot_product(first(i,:),second(:,j))

#else
                                        do k=1, f2
                                            multiply(i, j) = multiply(i, j) + first(i, k) * second(k, j)
                                        end do
#endif

                                end do
                            end do
                        end do
                    end do
#else
                    do i=1, f1
                        do j=1, s2
#ifdef DOT
                                multiply(i, j) = dot_product(first(i,:),second(:,j))
#else
                                do k=1,f2
                                    multiply(i, j) = multiply(i, j) + first(i, k) * second(k, j)
                                end do
#endif
                        end do
                    end do
#endif
                !check where to put status errors, make tests
                status = 0.d0
                else
                status=1.d0
            end if
        end subroutine
end module