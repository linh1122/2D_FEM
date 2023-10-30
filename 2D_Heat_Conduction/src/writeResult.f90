module writeResult

    contains
    subroutine writeMatrix(FnonBC,nONBCN,nonZ,csc_Kval,csc_Kcol_idx,csc_Krow_idx,TBound,nodeVar)
        integer, intent(in) :: nONBCN,nonZ,csc_Kcol_idx(nonZ),csc_Krow_idx(nonZ),nodeVar(nONBCN)
        real,    intent(in) :: FnonBC(nONBCN), TBound(4)
        double precision, intent(in) :: csc_Kval(nonZ)

        open(unit=28,file='./data/matrix/CSC_VAL.dat')
        open(unit=29,file='./data/matrix/CSC_COL.dat')
        open(unit=30,file='./data/matrix/CSC_ROW.dat')
        open(unit=31,file='./data/matrix/F.dat')
        open(unit=32,file='./data/matrix/T_ON_BOUNDARY.dat')
        open(unit=33,file='./data/matrix/NODE_VARIABLES.dat')

        do i=1, nonZ         
            write(28,"(ES24.9)", advance="no"), csc_Kval(i)
            write(28,"(a)", advance="no"), ","
            write(29,"(i7)", advance="no"), csc_Kcol_idx(i)
            write(29,"(a)", advance="no"), ","
            write(30,"(i7)", advance="no"), csc_Krow_idx(i)
            write(30,"(a)", advance="no"), ","
        end do

        do i=1, nONBCN
            write(31,"(ES24.9)", advance="no"), FnonBC(i)
            write(31,"(a)", advance="no"), ","

            write(33,"(i7)", advance="no"), nodeVar(i)
            write(33,"(a)", advance="no"), ","
        end do

        do i=1, 4
            write(32,"(ES24.9)", advance="no"), TBound(i)
            write(32,"(a)", advance="no"), ","
        end do

        close(28)
        close(29)
        close(30)
        close(31)
        close(32)
        close(33)
    end subroutine writeMatrix

    subroutine writeTec(nx,ny,nOn,nodes,nodes_number,T_final)
        integer, intent(in)          :: nx,ny,nOn,nodes_number(nx,ny)
        real,    intent(in)          :: nodes(nOn,2)
        double precision, intent(in) :: T_final(nOn)
        integer                      :: i,j   

        open(unit=20,file='./data/result.tec')
        write(20,*) "VARIABLES = ", "X ", "Y ", "T"
        write(20,*) "ZONE  I=",  nx, ",J=",ny, ", F=POINT"
        do j=1,ny
            do i=1,nx
                write(20,*) nodes(nodes_number(i,j),1), nodes(nodes_number(i,j),2), T_final(nodes_number(i,j))
            end do
        end do
        close(20)
    end subroutine writeTec

end module writeResult