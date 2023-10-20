module assembly

    use elementMatrix

    contains
    subroutine assemFullMatrix(elements,nOe,nONPe,nodes,nOn,Kg)
        integer, intent(in) :: nOe,nONPe,nOn
        real,    intent(in) :: nodes(nOn,2)
        integer,    intent(in) :: elements(nOe,nONPe)
        real,    intent(out):: Kg(nOn,nOn)
        integer             :: elementNodes(nONPe),i,j,k,n
        real                :: Ke(nONPe,nONPe)

        Ke(:,:) = 0.d0
        do k=1, nOe
            elementNodes(:) = elements(k,:)

            call elementK(elementNodes,nONPe,nodes,nOn,Ke)
            do i=1, nONPe
                do j=1, nONPe
                    Kg(elementNodes(i),elementNodes(j)) = Kg(elementNodes(i),elementNodes(j)) + Ke(i,j)
                end do
            end do
        end do
        
    end subroutine assemFullMatrix

end module assembly