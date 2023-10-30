module boundaryCondition

    contains
    subroutine BCsetValues(nOn,boundNodes,TBound,nonZ,csc_Krow_idx,csc_Kcol_idx,csc_Kval,csc_Kval1,nONBCN,FnonBC,nodesNonBC,nodeVar)
        integer, intent(in) :: nOn,boundNodes(nOn),nONBCN,nonZ,csc_Krow_idx(nonZ),csc_Kcol_idx(nonZ)
        real,    intent(in) :: TBound(4)
        double precision, intent(in)  :: csc_Kval(nonZ)
        double precision, intent(out) :: csc_Kval1(nonZ)
        real,    intent(out):: FnonBC(nONBCN)
        integer, intent(out):: nodeVar(nONBCN)
        integer             :: i,currentRow,previousRow,Findex,countZeros
        FnonBC(:) = 0.
        Findex = 1
        csc_Kval1(:) = csc_Kval(:)
        previousRow = csc_Krow_idx(1)
        nodeVar(1) = csc_Krow_idx(1)
        countZeros = 0
        do i=1, nonZ
            currentRow = csc_Krow_idx(i)
            if(currentRow.ne.previousRow) then
                Findex = Findex + 1
                previousRow = currentRow 
                nodeVar(Findex) = csc_Krow_idx(i)

            end if
            if(boundNodes(csc_Kcol_idx(i)).ne.0) then
                countZeros = countZeros + 1
                FnonBC(Findex) = FnonBC(Findex) - csc_Kval(i)*TBound(boundNodes(csc_Kcol_idx(i)))
                csc_Kval1(i) = 0.
                ! print*
            end if
        end do
        nodesNonBC = nonZ - countZeros
    end subroutine BCsetValues

end module boundaryCondition