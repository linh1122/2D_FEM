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
                    Krow_ptr(count_row) = j
                end if
            end do
        end do
    end subroutine compressedColumn


end module storeCompressedMatrix