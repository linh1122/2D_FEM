module globalVariables
    integer                                        :: iter,prtn_iter,nx,ny,nONPe,nOGp
    integer                                        :: nONBCN,nOe,nOn,nonZ,nodesNonBC
    real                                           :: lengthx,lengthy,TBound(4)
    double precision                               :: minimum_tol
    integer         ,   dimension (:,:), allocatable :: nnum,elements,nodes_adj
    integer         ,   dimension (:)  , allocatable :: boundNodes,nodeVar
    real            ,   dimension (:,:), allocatable :: nodes,xi
    real            ,   dimension (:)  , allocatable :: w,FnonBC
    double precision,   dimension (:)  , allocatable :: T, T_final

    double precision,   dimension (:), allocatable :: Kval, csc_Kval, csc_Kval_BC
    integer,            dimension (:), allocatable :: Kcol_idx,Krow_ptr,csc_Kcol_idx,csc_Krow_idx,csc_Kcol_idx_BC,csc_Krow_idx_BC
end module globalVariables