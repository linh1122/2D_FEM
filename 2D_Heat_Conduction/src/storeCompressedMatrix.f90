module storeCompressedMatrix

    contains

    subroutine calNumberOfNonzeros(nONBCN,KnonBC,nonZ)
        integer, intent(in) :: nONBCN
        real,    intent(in) :: KnonBC(nONBCN,nONBCN)
        integer, intent(out):: nonZ
        integer             :: i,j

        nonZ = 0
        do i=1, nONBCN
            do j=1, nONBCN
                if(KnonBC(i,j).ne.0.) then
                    nonZ = nonZ + 1
                end if
            end do
        end do
    end subroutine calNumberOfNonzeros

    subroutine compressedRow(nONBCN,KnonBC,nonZ,Kval,Kcol_idx,Krow_ptr)
        integer, intent(in) :: nONBCN,nonZ
        real,    intent(in) :: KnonBC(nONBCN,nONBCN)
        double precision,    intent(out):: Kval(nonZ)
        integer, intent(out):: Kcol_idx(nonZ),Krow_ptr(nONBCN+1)

        integer             :: i,j,count_val,count_row,pre_i

        count_val = 0
        count_row = 0
        pre_i = 0
        do i=1, nONBCN
            do j=1, nONBCN
                if(KnonBC(i,j).ne.0.) then
                    count_val = count_val + 1
                    Kval(count_val) = KnonBC(i,j)
                    Kcol_idx(count_val) = j
                    if(i.ne.pre_i) then 
                         pre_i = i
                         count_row = count_row + 1
                         Krow_ptr(count_row) = count_val
                    end if
                end if
            end do
        end do
        Krow_ptr(nONBCN+1) = nonZ+1
    end subroutine compressedRow

    subroutine compressedColumn(nONBCN,KnonBC,nonZ,Kval,Kcol_idx,Krow_ptr)
        integer, intent(in) :: nONBCN,nonZ
        real,    intent(in) :: KnonBC(nONBCN,nONBCN)
        double precision,    intent(out):: Kval(nonZ)
        integer, intent(out):: Kcol_idx(nonZ),Krow_ptr(nonZ)

        integer             :: i,j,count_val,count_row,pre_i

        count_val = 0
        count_row = 0
        pre_i = 0
        do j=1, nONBCN
            do i=1, nONBCN
                if(KnonBC(i,j).ne.0.) then
                    count_val = count_val + 1
                    Kval(count_val) = KnonBC(i,j)
                    Kcol_idx(count_val) = i
                    Krow_ptr(count_val) = j
                end if
            end do
        end do
    end subroutine compressedColumn

    subroutine setUpCSC(nOn,nonZ,nodes_adj,csc_Kcol_idx,csc_Krow_idx)
        integer, intent(in) :: nOn,nonZ,nodes_adj(nOn,9)
        integer, intent(out):: csc_Kcol_idx(nonZ),csc_Krow_idx(nonZ)
        integer             :: i,j,count

        count = 0
        do i=1, nOn
            do j=1, 9
                if(nodes_adj(i,j).ne.-1) then
                    count = count + 1
                    csc_Kcol_idx(count) = nodes_adj(i,j)
                    csc_Krow_idx(count) = i
                end if
            end do
        end do

    end subroutine setUpCSC

    subroutine findLocation(nonZ,csc_Kcol_idx,csc_Krow_idx,node_i,node_j,k)
        integer, intent(in) :: node_i, node_j, nonZ, csc_Kcol_idx(nonZ), csc_Krow_idx(nonZ)
        integer, intent(out):: k
        integer             :: hi, lo, md

        lo = 1
        hi = nonZ

        do
            if ( hi < lo ) then
                k = -1
                exit
            end if

            md = ( lo + hi ) / 2

            if ( csc_Krow_idx(md) < node_i .or. ( csc_Krow_idx(md) == node_i .and. csc_Kcol_idx(md) < node_j ) ) then
                lo = md + 1
            else if ( node_i < csc_Krow_idx(md) .or. ( csc_Krow_idx(md) == node_i .and. node_j < csc_Kcol_idx(md) ) ) then
                hi = md - 1
            else
                k = md
                exit
            end if
        end do
    end subroutine findLocation

    subroutine reshapeCSCmatrix(nONBCN,nodesNonBC,nonZ,nodeVar,csc_Kval,csc_Kcol_idx,csc_Krow_idx,csc_Kval_BC,csc_Kcol_idx_BC,csc_Krow_idx_BC)
        integer, intent(in) :: nodesNonBC, nonZ, nONBCN,nodeVar(nONBCN)
        integer, intent(in) :: csc_Kcol_idx(nonZ), csc_Krow_idx(nonZ)
        integer, intent(out) :: csc_Kcol_idx_BC(nodesNonBC), csc_Krow_idx_BC(nodesNonBC)
        double precision,    intent(in) :: csc_Kval(nonZ)
        double precision,    intent(out) :: csc_Kval_BC(nodesNonBC)

        integer             :: i,j,count

        count = 0
        do i=1, nonZ
            if(csc_Kval(i).ne.0.) then
                count = count + 1
                csc_Kval_BC(count) = csc_Kval(i)
                ! csc_Kcol_idx_BC(count) = csc_Kcol_idx(i)
                ! csc_Krow_idx_BC(count) = csc_Krow_idx(i)
                do j=1, nONBCN
                    if(csc_Krow_idx(i).eq.nodeVar(j)) then
                        csc_Krow_idx_BC(count) = j
                    end if
                    if(csc_Kcol_idx(i).eq.nodeVar(j)) then
                        csc_Kcol_idx_BC(count) = j
                    end if
                end do
            end if
        end do
    end subroutine reshapeCSCmatrix

end module storeCompressedMatrix