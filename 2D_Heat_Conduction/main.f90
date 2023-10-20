program main
    use globalVariables
    use meshGenerator
    use boundaryIdentify
    use gaussQuad
    use assembly
    use boundaryCondition
    use storeNonBoundaryData
    use storeCompressedMatrix
    use solvers
    use writeResult

    integer i,j
    Kg(:,:) = 0.d0
    KgBC(:,:) = 0.d0
    F(:) = 0.d0
    call create_nodes(nx,ny,lengthx,lengthy,nodes,nnum)
    call create_elements(nx,ny,nONPe,nnum,elements)
    open(unit=20,file='./mesh/nodes.dat')
    open(unit=21,file='./mesh/elements.dat')
    open(unit=22,file='./mesh/mesh.tec')
        call write_mesh_data(nx,ny,nodes,nnum,nONPe,elements,20,21,22)
    close(20)
    close(21)
    close(22)

    call create_boundary(lengthx,lengthy,nx,ny,nOn,nodes,nnum,boundNodes)
    open(unit=23,file='./mesh/boundary.dat')
        call export_boundary(nx,ny,boundNodes,nnum,23)
    close(23)

    call setGauss(xi,w,nOGp)
    call assemFullMatrix(elements,nOe,nONPe,nodes,nOn,Kg)

    ! do i=1, nOn
    !     do j=1, nOn
    !         write(*,"(3f8.3)", advance="no"), Kg(i,j)
    !     end do
    !     print*
    ! end do
    ! print*

    call BCsetValues(nOn,boundNodes,Kg,T1,T2,T3,T4,KgBC,F)

    ! do i=1, nOn
    !     do j=1, nOn
    !         write(*,"(3f8.3)", advance="no"), KgBC(i,j)
    !     end do
    !     print*
    ! end do
    ! print*

    ! do i=1, nOn
    !     write(*,"(3f8.3)", advance="no"), F(i)
    !     print*
    ! end do
    ! print*

    call storeNonBC(nOn,nONBCN,KgBC,F,boundNodes,KnonBC,FnonBC,nodeVar)

    ! do i=1, nONBCN
    !     do j=1, nONBCN
    !         write(*,"(3f8.3)", advance="no"), KnonBC(i,j)
    !     end do
    !      print*
    ! end do
    ! print*

    ! do i=1, nONBCN
    !     write(*,"(3f8.3)", advance="yes"),  FnonBC(i)
    ! end do

    ! do i=1, nONBCN
    !     print*, nodeVar(i)
    ! end do

    call writeTec(nx,ny,nOn,boundNodes,nodes,nnum,T1,T2,T3,T4)

    call calNumberOfNonzeros(nONBCN,KnonBC,nonZ)

    print*
    print*, nonZ

    ! allocate(Kval(nonZ))
    ! allocate(Kcol_idx(nonZ))
    ! allocate(Krow_ptr(nONBCN+1))
    ! call compressedRow(nONBCN,KnonBC,nonZ,Kval,Kcol_idx,Krow_ptr)

    allocate(csc_Kval(nonZ))
    allocate(csc_Kcol_idx(nonZ))
    allocate(csc_Krow_idx(nonZ))
    ! call compressedColumn(nONBCN,KnonBC,nonZ,csc_Kval,csc_Kcol_idx,csc_Krow_idx)
    ! do j=1, nonZ
    !     write(*,"(3f8.3)", advance="no"), Kval(j)
    ! end do
    ! print*

    ! do j=1, nonZ
    !     write(*,"(i4)", advance="no"), Kcol_idx(j)
    ! end do
    ! print*

    ! do j=1, nONBCN+1
    !     write(*,"(i4)", advance="no"), Krow_ptr(j)
    ! end do
    ! print*
    ! print*

    call writeMatrix(nOn,KgBC,F,nONBCN,KnonBC,FnonBC,nodeVar,nonZ,Kval,Kcol_idx,Krow_ptr,csc_Kval,csc_Kcol_idx,csc_Krow_idx)
    ! call gauSeidal(nONBCN,nonZ,Kval,Kcol_idx,Krow_ptr,FnonBC,iter,T)

    ! do j=1, nONBCN
    !     print*, T(j)
    ! end do
    ! print*

    ! call solv(nONBCN,nonZ,128,Krow_ptr,Kcol_idx,Kval)

    ! deallocate(Kval)
    ! deallocate(Kcol_idx)
    ! deallocate(Krow_ptr)

    deallocate(csc_Kval)
    deallocate(csc_Kcol_idx)
    deallocate(csc_Krow_idx)
end program