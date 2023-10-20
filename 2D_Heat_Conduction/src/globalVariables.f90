module globalVariables
    integer, parameter ::  iter=100,nx=120,ny=120,nOn=nx*ny,nOe=(nx-1)*(ny-1),nONPe=4,nOGp=9,nONBCN=nOn-(2*nx+2*(ny-2))
    real,    parameter ::  lengthx=2., lengthy=4, T1=293., T2=193., T3=473., T4=473.
    integer            ::  stat,nnum(nx,ny), elements(nOe,nONPe), boundNodes(nOn), nodeVar(nONBCN), nonZ
    real               ::  nodes(nOn,2), Kg(nOn,nOn), KgBC(nOn,nOn), xi(nOGp,2), w(nOGp), F(nOn),KnonBC(nONBCN,nONBCN),FnonBC(nONBCN),T(nONBCN)
    double precision,    dimension (:), allocatable :: Kval
    integer, dimension (:), allocatable :: Kcol_idx,Krow_ptr

    double precision,    dimension (:), allocatable :: csc_Kval
    integer, dimension (:), allocatable :: csc_Kcol_idx,csc_Krow_idx
end module globalVariables