module assembly

    use elementMatrix
    use storeCompressedMatrix

    contains
    subroutine assemFullMatrix(elements,nOe,nONPe,nodes,nOn,nonZ,csc_Kcol_idx,csc_Krow_idx,csc_Kval,nOGp,xi,w)
        integer, intent(in)    :: nOe,nONPe,nOn,nonZ,nOGp,csc_Kcol_idx(nonZ),csc_Krow_idx(nonZ)
        real,    intent(in)    :: nodes(nOn,2),xi(nOGp,2),w(nOGp)
        integer,    intent(in) :: elements(nOe,nONPe)
        double precision,    intent(out) :: csc_Kval(nonZ)
        integer             :: elementNodes(nONPe),i,j,k,n,m
        real                :: Ke(nONPe,nONPe)

        Ke(:,:) = 0.d0
        do k=1, nOe
            elementNodes(:) = elements(k,:)

            call elementK(elementNodes,nONPe,nodes,nOn,Ke,nOGp,xi,w)
            do i=1, nONPe
                do j=1, nONPe
                    call findLocation(nonZ,csc_Kcol_idx,csc_Krow_idx,elementNodes(i),elementNodes(j),m)
                    if(m.ne.-1) then
                        csc_Kval(m) = csc_Kval(m) + Ke(i,j)
                    end if
                end do
            end do
        end do
        
    end subroutine assemFullMatrix

end module assembly