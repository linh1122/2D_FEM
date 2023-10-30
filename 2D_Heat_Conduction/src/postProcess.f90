module postProcess

    contains
    subroutine completeT(T,T_final,nONBCN,nOn,TBound,boundNodes)
        integer,          intent(in)  :: nONBCN, nOn, boundNodes(nOn)
        real,             intent(in)  :: TBound(4)
        double precision, intent(in)  :: T(nONBCN)
        double precision, intent(out) :: T_final(nOn)
        integer                      :: i,count

        count = 0
        do i=1,nOn
            if(boundNodes(i).eq.0) then
                count = count + 1
                T_final(i) = T(count)
            else
                T_final(i) = TBound(boundNodes(i))
            end if
        end do

    end subroutine completeT

end module postProcess