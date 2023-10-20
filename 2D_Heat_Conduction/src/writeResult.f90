module writeResult

    contains
    subroutine writeMatrix(nOn,KgBC,F,nONBCN,KnonBC,FnonBC,nodeVar,nonZ,Kval,Kcol_idx,Krow_ptr,csc_Kval,csc_Kcol_idx,csc_Krow_idx)
        integer, intent(in) :: nOn, nONBCN, nodeVar(nONBCN),nonZ,Kcol_idx(nonZ),Krow_ptr(nONBCN+1),csc_Kcol_idx(nonZ),csc_Krow_idx(nonZ)
        real,    intent(in) :: KgBC(nOn,nOn),F(nOn),KnonBC(nONBCN,nONBCN),FnonBC(nONBCN)
        double precision, intent(in) :: Kval(nonZ),csc_Kval(nonZ)

        ! open(unit=20,file='./data/matrix/K_FULL.dat')
        ! open(unit=21,file='./data/matrix/F_FULL.dat')
        ! open(unit=22,file='./data/matrix/K_NON_BOUNDARY.dat')
        ! open(unit=23,file='./data/matrix/F_NON_BOUNDARY.dat')
        ! open(unit=24,file='./data/matrix/NON_BOUNDARY_NODE_NUMBER.dat')
        open(unit=25,file='./data/matrix/CSR_VAL.dat')
        open(unit=26,file='./data/matrix/CSR_COL.dat')
        open(unit=27,file='./data/matrix/CSR_ROW.dat')
        open(unit=28,file='./data/matrix/CSC_VAL.dat')
        open(unit=29,file='./data/matrix/CSC_COL.dat')
        open(unit=30,file='./data/matrix/CSC_ROW.dat')
        ! do i=1, nOn
        !     do j=1, nOn
        !         write(20,"(3f8.3)", advance="no"), KgBC(i,j)
        !     end do
        !     write(20,"(3f8.3)", advance="yes")
        !     write(21,"(3f8.3)", advance="yes"), F(i)
        ! end do

        ! do i=1, nONBCN
        !     do j=1, nONBCN
        !         write(22,"(3f8.3)", advance="no"), KnonBC(i,j)
        !         write(22,"(a)", advance="no"), ","
        !     end do
        !     write(22,"(3f8.3)", advance="yes")
        !     write(23,"(3f8.3)", advance="no"), FnonBC(i)
        !     write(23,"(a)", advance="no"), ","
        !     write(24,"(i4)", advance="no"), nodeVar(i)
        !     write(24,"(a)", advance="no"), ","
        !     write(27,"(i4)", advance="no"), Krow_ptr(i)
        !     write(27,"(a)", advance="no"), ","
        ! end do
        !     write(27,"(i4)", advance="no"), Krow_ptr(nONBCN+1)

        do i=1, nonZ
            write(25,"(3f8.3)", advance="no"), Kval(i)
            write(25,"(a)", advance="no"), ","
            write(26,"(i4)", advance="no"), Kcol_idx(i)
            write(26,"(a)", advance="no"), ","
                        
            write(28,"(3f8.3)", advance="no"), csc_Kval(i)
            write(28,"(a)", advance="no"), ","
            write(29,"(i4)", advance="no"), csc_Kcol_idx(i)
            write(29,"(a)", advance="no"), ","
            write(30,"(i4)", advance="no"), csc_Krow_idx(i)
            write(30,"(a)", advance="no"), ","
        end do


        ! close(20)
        ! close(21)
        ! close(22)
        ! close(23)
        ! close(24)
        close(25)
        close(26)
        close(27)
        close(28)
        close(29)
        close(30)
    end subroutine writeMatrix

    subroutine writeTec(nx,ny,nOn,boundNodes,nodes,nodes_number,T1,T2,T3,T4)
        integer, intent(in) :: nx,ny,nOn,boundNodes(nOn),nodes_number(nx,ny)
        real,    intent(in) :: nodes(nOn,2),T1,T2,T3,T4
        integer             :: i,j
        
        open(unit=19,file='./data/matrix/T_ON_BOUNDARY.dat')
            write(19,"(3f8.3)", advance="no"), T1
            write(19,"(a)", advance="no"), ","
            write(19,"(3f8.3)", advance="no"), T2
            write(19,"(a)", advance="no"), ","
            write(19,"(3f8.3)", advance="no"), T3
            write(19,"(a)", advance="no"), ","
            write(19,"(3f8.3)", advance="no"), T4
        close(19)

        ! open(unit=20,file='./data/result.tec')
        ! write(20,*) "VARIABLES = ", "X ", "Y ", "T"
        ! write(20,*) "ZONE  I=",  nx, ",J=",ny, ", F=POINT"
        ! do j=1,ny
        !     do i=1,nx
        !         if(boundNodes(nodes_number(i,j)).eq.1) then 
        !             write(20,*) nodes(nodes_number(i,j),1), nodes(nodes_number(i,j),2), T1
        !         else if(boundNodes(nodes_number(i,j)).eq.2) then 
        !             write(20,*) nodes(nodes_number(i,j),1), nodes(nodes_number(i,j),2), T2
        !         else if(boundNodes(nodes_number(i,j)).eq.3) then 
        !             write(20,*) nodes(nodes_number(i,j),1), nodes(nodes_number(i,j),2), T3
        !         else if(boundNodes(nodes_number(i,j)).eq.4) then 
        !             write(20,*) nodes(nodes_number(i,j),1), nodes(nodes_number(i,j),2), T4
        !         else if(boundNodes(nodes_number(i,j)).eq.0) then 
        !             write(20,*) nodes(nodes_number(i,j),1), nodes(nodes_number(i,j),2)
        !         end if
        !     end do
        ! end do
        ! close(20)
    end subroutine writeTec

end module writeResult