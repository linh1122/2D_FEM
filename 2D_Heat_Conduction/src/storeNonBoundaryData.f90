module storeNonBoundaryData

    contains
    subroutine storeNonBC(nOn,nONBCN,Kg,F,boundNodes,KnonBC,FnonBC,nodeVar)
        integer, intent(in) :: nOn,nONBCN,boundNodes(nOn)
        real,    intent(in) :: Kg(nOn,nOn),F(nOn)
        real,    intent(out):: KnonBC(nONBCN,nONBCN),FnonBC(nONBCN)
        integer, intent(out):: nodeVar(nONBCN)
        integer             :: i,j,i_count,j_count
        i_count = 1
        do i=1, nOn
            if(boundNodes(i).eq.0) then
                nodeVar(i_count)=i
                i_count = i_count + 1
            end if
        end do

        do i=1, nONBCN
            do j=1, nONBCN
                KnonBC(i,j) = Kg(nodeVar(i),nodeVar(j))
            end do
                FnonBC(i) = F(nodeVar(i))
        end do

    end subroutine storeNonBC

end module storeNonBoundaryData