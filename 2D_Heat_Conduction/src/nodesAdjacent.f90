module nodesAdjacent

    contains
    subroutine nodes_neighbor(nx,ny,nodes_number,nodes_adj,nonZ)
        integer, intent(in) :: nx, ny, nodes_number(nx,ny)
        integer, intent(out):: nodes_adj(nx*ny,9),nonZ

        integer             :: i,j
        nodes_adj(:,:) = -1
        nonZ = 0
        do i=2,nx-1
            do j=2, ny-1
                nodes_adj(nodes_number(i,j),1)=nodes_number(i-1,j-1)
                nodes_adj(nodes_number(i,j),2)=nodes_number(i-1,j)
                nodes_adj(nodes_number(i,j),3)=nodes_number(i-1,j+1)
                nodes_adj(nodes_number(i,j),4)=nodes_number(i,j-1)
                nodes_adj(nodes_number(i,j),5)=nodes_number(i,j)
                nodes_adj(nodes_number(i,j),6)=nodes_number(i,j+1)
                nodes_adj(nodes_number(i,j),7)=nodes_number(i+1,j-1)
                nodes_adj(nodes_number(i,j),8)=nodes_number(i+1,j)
                nodes_adj(nodes_number(i,j),9)=nodes_number(i+1,j+1)
                nonZ = nonZ+9
            end do
        end do
    end subroutine nodes_neighbor

end module nodesAdjacent