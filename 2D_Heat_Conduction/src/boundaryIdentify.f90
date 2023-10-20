module boundaryIdentify

    contains
    subroutine create_boundary(lengthx,lengthy,nx,ny,nOn,nodes,nodes_number,boundNodes)
        integer, intent(in)  :: nx, ny, nOn, nodes_number(nx,ny)
        real   , intent(in)  :: nodes(nx*ny,2),lengthx,lengthy
        integer, intent(out) :: boundNodes(nOn)
        integer i,j
        
        do i=1, nx 
            do j=1, ny
                if(nodes(nodes_number(i,j),1).eq.0.)           then
                    boundNodes(nodes_number(i,j)) = 1
                else if(nodes(nodes_number(i,j),1).eq.lengthx) then
                    boundNodes(nodes_number(i,j)) = 2
                else if(nodes(nodes_number(i,j),2).eq.0.)      then 
                    boundNodes(nodes_number(i,j)) = 3
                else if(nodes(nodes_number(i,j),2).eq.lengthy) then
                    boundNodes(nodes_number(i,j)) = 4
                else
                    boundNodes(nodes_number(i,j)) = 0
                end if
            end do
        end do
    end subroutine create_boundary

    subroutine export_boundary(nx,ny,boundNodes,nodes_number,boundary_file)
        integer, intent(in)  :: nx, ny, boundNodes(nx*ny), nodes_number(nx,ny), boundary_file

        integer i,j
        do i=1, nx
            do j=1,ny
                write(boundary_file,*) nodes_number(i,j), ",", boundNodes(nodes_number(i,j))
            end do
        end do
    end subroutine export_boundary

end module boundaryIdentify