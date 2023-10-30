program main
    use globalVariables
    use preProcess
    use meshGenerator
    use boundaryIdentify
    use gaussQuad
    use nodesAdjacent
    use assembly
    use boundaryCondition
    use storeNonBoundaryData
    use storeCompressedMatrix
    use solvers
    use postProcess
    use writeResult

!___READ INPUT FILE AND ALLOCATE MEMORY FOR VARIABLES______________________
    call readInput(iter,prtn_iter,nx,ny,nONPe,nOGp,nOn,nOe,nONBCN,lengthx,lengthy,TBound,minimum_tol)
    call alloc_vari
!__________________________________________________________________________


!___CREATE MESH (NODES, ELEMENTS)__________________________________________
    call create_nodes(nx,ny,lengthx,lengthy,nodes,nnum)                  
    call create_elements(nx,ny,nONPe,nnum,elements)                      
    call write_mesh_data(nx,ny,nodes,nnum,nONPe,elements)
!__________________________________________________________________________
    

!___SET BOUNDARY___________________________________________________________
    call create_boundary(lengthx,lengthy,nx,ny,nOn,nodes,nnum,boundNodes)  
    call export_boundary(nx,ny,boundNodes,nnum)
!__________________________________________________________________________


!___SET NODE ADJACENT______________________________________________________
    call nodes_neighbor(nx,ny,nnum,nodes_adj,nonZ)
!__________________________________________________________________________
    
    
!___SET GAUSS QUADRATURE POINT_____________________________________________
    call setGauss(xi,w,nOGp)
!__________________________________________________________________________


!___ALLOCATE MEMORY FOR COMPRESSED COLUMN STORAGE__________________________
    allocate(csc_Kval(nonZ))
    allocate(csc_Kcol_idx(nonZ))
    allocate(csc_Krow_idx(nonZ))
    csc_Kval(:) = 0.
!__________________________________________________________________________


!___ESTIMATE THE NON-ZERO VALUES AND SET THEIR POSITION INTO COLUMN AND ROW
    call setUpCSC(nOn,nonZ,nodes_adj,csc_Kcol_idx,csc_Krow_idx)
!__________________________________________________________________________


!___ASSEMBLY K MATRIX AND STORE ONLY NON-ZERO VALUES INTO VALUES ARRAY______
    call assemFullMatrix(elements,nOe,nONPe,nodes,nOn,nonZ,csc_Kcol_idx,csc_Krow_idx,csc_Kval,nOGp,xi,w)
!___________________________________________________________________________


!___SET BOUNDARY CONDITION FOR NODES, COUNT THE REAL NUMBER OF NON-ZERO NODE
    call BCsetValues(nOn,boundNodes,TBound,nonZ,csc_Krow_idx,csc_Kcol_idx,csc_Kval,csc_Kval,nONBCN,FnonBC,nodesNonBC,nodeVar)
!___________________________________________________________________________


!___ALLOCATE MEMORY FOR NEW ARRAY OF VALUES, COLUMN AND ROW_________________
    allocate(csc_Kval_BC(nodesNonBC))
    allocate(csc_Kcol_idx_BC(nodesNonBC))
    allocate(csc_Krow_idx_BC(nodesNonBC))
!___________________________________________________________________________


!___STORE THE NON-ZERO VALUES_______________________________________________
    call reshapeCSCmatrix(nONBCN,nodesNonBC,nonZ,nodeVar,csc_Kval,csc_Kcol_idx,csc_Krow_idx,csc_Kval_BC,csc_Kcol_idx_BC,csc_Krow_idx_BC)
!___________________________________________________________________________


!___FREE MEMORY FOR OLD VALUES______________________________________________
    deallocate(csc_Kval)
    deallocate(csc_Kcol_idx)
    deallocate(csc_Krow_idx)
!____________________________________________________________________________


!___WRITE MATRIX TO FILE_____________________________________________________
    print*, "Writing matrix to file..."
    call writeMatrix(FnonBC,nONBCN,nodesNonBC,csc_Kval_BC,csc_Kcol_idx_BC,csc_Krow_idx_BC,TBound,nodeVar)
!_____________________________________________________________________________


!___SOLVE THE MATRIX IN CSC FORMAT____________________________________________
    call solv(iter,prtn_iter,minimum_tol,nONBCN,nodesNonBC,csc_Krow_idx_BC,csc_Kcol_idx_BC,csc_Kval_BC,FnonBC,T)
!_____________________________________________________________________________


!___FREE MEMORY_______________________________________________________________
    deallocate(csc_Kval_BC)
    deallocate(csc_Kcol_idx_BC)
    deallocate(csc_Krow_idx_BC)
!_____________________________________________________________________________


!___POST PROCESS, COMBINE ALL NODE(BOUNDARY NODES AND INNER NODES)____________
    call completeT(T,T_final,nONBCN,nOn,TBound,boundNodes)
!_____________________________________________________________________________


!___WRITE FILE TO TEC FILE FOR DISPLAY________________________________________
    print*, "Writing tec file..."
    call writeTec(nx,ny,nOn,nodes,nnum,T_final)
    print*, "Done!"
!_____________________________________________________________________________

    call dealloc_vari
end program