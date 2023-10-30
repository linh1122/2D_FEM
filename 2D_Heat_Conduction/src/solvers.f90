module solvers

            
    contains
    subroutine solv(iter,prtn_iter,minimum_tol,numberOfVariables,numberOfval,row_A,col_A,val_A,B,X)
        integer, intent(in) :: iter, prtn_iter, numberOfVariables, numberOfval, row_A(numberOfval), col_A(numberOfval)
        real,    intent(in) :: B(numberOfVariables)
        double precision, intent(in) :: val_A(numberOfval),minimum_tol
        double precision, intent(out) :: X(numberOfVariables)
       
        call gausSeidal(iter,prtn_iter,minimum_tol,numberOfVariables,numberOfval,row_A,col_A,val_A,B,X)
    end subroutine solv

    subroutine gausSeidal(iter,prtn_iter,minimum_tol,numberOfVariables,numberOfval,row_A,col_A,val_A,B,X)
        integer, intent(in) :: iter, prtn_iter, numberOfVariables, numberOfval, row_A(numberOfval), col_A(numberOfval)
        real,    intent(in) :: B(numberOfVariables)
        double precision, intent(in) :: val_A(numberOfval),minimum_tol
        double precision, intent(out) :: X(numberOfVariables)

        integer                       :: k,i,j,count_row
        double precision              :: x_new(numberOfVariables), sum, a_ii, tol

        X(:) = 0.
        do k=1, iter
            if(k.eq.1) then
            print*, minimum_tol
                print*, "Iter   ", "rtol     "
            end if
            x_new(:) = X(:)
            count_row = 1
            do i=1, numberOfval
                if(row_A(i).eq.col_A(i)) then
                    a_ii = val_A(i)
                end if
                if(((i.ne.1).and.(row_A(i).ne.row_A(i-1))).or.(i.eq.numberOfval)) then
                    x_new(count_row) = (B(count_row) - sum)/a_ii
                    sum = 0.
                    count_row = count_row + 1
                end if
                if(row_A(i).ne.col_A(i)) then
                    sum = sum + val_A(i)*x_new(col_A(i))
                end if
            end do
            call rtol(X,x_new,numberOfVariables,tol)
            if((k.eq.1).or.(mod(k,prtn_iter).eq.0)) then
                write(*,"(i7)",advance="no"), k
                write(*,"(ES24.7)",advance="yes"), tol
            end if
            X(:) = x_new(:)
            if(tol.lt.minimum_tol) then
                write(*,"(i7)",advance="no"), k
                write(*,"(ES24.7)",advance="yes"), tol
                exit
            end if
        end do

    end subroutine gausSeidal


    subroutine rtol(x,x_new,numberOfVariables,tol)
        integer, intent(in) :: numberOfVariables
        double precision, intent(in) :: x(numberOfVariables), x_new(numberOfVariables)
        double precision, intent(out):: tol
        double precision             :: tol_max,a
        integer                      :: i

        tol_max = 1e-16
        do i=1, numberOfVariables
            a = x_new(i)-x(i)
            if(abs(a).gt.tol_max) then
                tol_max = abs(a)
            end if
            ! print*, a
        end do

        tol= tol_max
    end subroutine rtol

end module solvers