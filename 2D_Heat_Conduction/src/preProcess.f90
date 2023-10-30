module preProcess
    use globalVariables

    contains
    subroutine readInput(iter,prtn_iter,nx,ny,nONPe,nOGp,nOn,nOe,nONBCN,lengthx,lengthy,TBound,minimum_tol)
        integer, intent(out)          :: iter,prtn_iter,nx,ny,nONPe,nOGp,nOn,nOe,nONBCN
        real,    intent(out)          :: lengthx, lengthy, TBound(4)
        double precision, intent(out) :: minimum_tol

        character(len=255)  char1,char2
        open(unit=10,file='./input.dat',status = 'old')
            read(10,*) char1,char2,iter
            read(10,*) char1,char2,prtn_iter
            read(10,*) char1,char2,minimum_tol
            read(10,*) char1,char2,lengthx
            read(10,*) char1,char2,lengthy
            read(10,*) char1,char2,nx
            read(10,*) char1,char2,ny
            read(10,*) char1,char2,nONPe
            read(10,*) char1,char2,nOGp
            read(10,*) char1,char2,TBound(1)
            read(10,*) char1,char2,TBound(2)
            read(10,*) char1,char2,TBound(3)
            read(10,*) char1,char2,TBound(4)
        close(10)

        nOn=nx*ny
        nOe=(nx-1)*(ny-1)
        nONBCN=nOn-(2*nx+2*(ny-2))

    end subroutine readInput

    subroutine alloc_vari()
        allocate(nnum(nx,ny))
        allocate(elements(nOe,nONPe))
        allocate(boundNodes(nOn))
        allocate(nodeVar(nONBCN))
        allocate(nodes_adj(nOn,9))
        allocate(nodes(nOn,2))
        allocate(xi(nOGp,2))
        allocate(w(nOGp))
        allocate(FnonBC(nONBCN))
        allocate(T(nONBCN))
        allocate(T_final(nOn))
    end subroutine alloc_vari

    subroutine dealloc_vari()
        deallocate(nnum)
        deallocate(elements)
        deallocate(boundNodes)
        deallocate(nodeVar)
        deallocate(nodes_adj)
        deallocate(nodes)
        deallocate(xi)
        deallocate(w)
        deallocate(FnonBC)
        deallocate(T)
        deallocate(T_final)
    end subroutine dealloc_vari

end module preProcess