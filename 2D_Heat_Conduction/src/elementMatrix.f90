module elementMatrix
    use gaussQuad

    contains
    subroutine elementK(elementNodes,nONPe,nodes,nOn,Ke,nOGp,xi,w)
        integer, intent(in) :: nONPe,nOn,nOGp,elementNodes(nONPe)
        real,    intent(in) :: nodes(nOn,2),xi(nOGp,2),w(nOGp)
        real,    intent(out):: Ke(nONPe,nONPe)
        integer             :: i,j

        do i=1, nONPe
            do j=1, nONPe
                call calKGauss(elementNodes,nONPe,nodes,nOGp,nOn,i,j,Ke(i,j),xi,w)
            end do
        end do
    end subroutine elementK
end module elementMatrix