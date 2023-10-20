module boundaryCondition

    contains
    subroutine BCsetValues(nOn,boundNodes,Kg,T1,T2,T3,T4,Kg_out,F)
        integer, intent(in) :: nOn,boundNodes(nOn)
        real,    intent(in) :: Kg(nOn,nOn),T1,T2,T3,T4
        real,    intent(out):: Kg_out(nOn,nOn),F(nOn)
        integer             :: i,j
        Kg_out(:,:) = Kg(:,:)
        do i=1, nOn
            F(i)=0.
            do j=1, nOn
                if(boundNodes(j).eq.1) then 
                    Kg_out(i,j) = 0.
                    F(i) = F(i) - Kg(i,j)*T1
                else if(boundNodes(j).eq.2) then 
                    Kg_out(i,j) = 0.
                    F(i) = F(i) - Kg(i,j)*T2
                else if(boundNodes(j).eq.3) then 
                    Kg_out(i,j) = 0.
                    F(i) = F(i) - Kg(i,j)*T3
                else if(boundNodes(j).eq.4) then 
                    Kg_out(i,j) = 0.
                    F(i) = F(i) - Kg(i,j)*T4
                end if
            end do
            if(boundNodes(i).ne.0) then
                F(i)=0.
                Kg_out(i,:) = 0.
            end if
        end do

    end subroutine BCsetValues

end module boundaryCondition