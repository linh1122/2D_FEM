module meshGenerator


    contains

    subroutine create_nodes(nx,ny,lengthx,lengthy,nodes,nodes_number)

        integer, intent(in)  :: nx, ny
        real   , intent(in)  :: lengthx, lengthy
        integer, intent(out) :: nodes_number(nx,ny)
        real   , intent(out) :: nodes(nx*ny,2)

        integer i, j, count
        real dy, dx

        count = 0
        dy = 1./(ny-1)
        dx = 1./(nx-1)
        do i=1, nx
            do j=1, ny
                count = count+1
                nodes(count,1) = (i-1)*dx*lengthx
                nodes(count,2) = (j-1)*dy*lengthy
                nodes_number(i,j) = count
            end do
        end do 
    end subroutine create_nodes

    subroutine create_elements(nx,ny,nodesPerElement,nodes_number,elements)

        integer, intent(in)  :: nx, ny
        integer, intent(in)  :: nodesPerElement, nodes_number(nx,ny)
        integer, intent(out) :: elements((nx-1)*(ny-1),nodesPerElement)

        integer i, j, count, k
        real    nex, ney
        count = 0
        nex = nx-1
        ney = ny-1

        numberOfelements = nex*ney

        do i=1, nex
            do j=1, ney
                count = count+1
                elements(count,1) = nodes_number(i,j)
                elements(count,2) = nodes_number(i+1,j)
                elements(count,3) = nodes_number(i+1,j+1)
                elements(count,4) = nodes_number(i,j+1)
            end do
        end do
    end subroutine create_elements

    subroutine write_mesh_data(nx,ny,nodes,nodes_number,nodesPerElement,elements,nodes_file,elements_file,tec_file)
        integer, intent(in)  :: nx, ny, nodes_number(nx,ny), elements((nx-1)*(ny-1),nodesPerElement), nodesPerElement, nodes_file, elements_file, tec_file
        real   , intent(in)  :: nodes(nx*ny,2)

        integer :: i,j,count_e
        count_e = 0
        write(elements_file,*)  (nx-1)*(ny-1)
        write(tec_file,*) "VARIABLES = ", "X ", "Y "
        write(tec_file,*) "ZONE  I=",  nx, ",J=",ny, ", F=POINT"
        open(unit=100,file='./mesh/dimensions.dat')
            write(100,"(i4)", advance="no"), nx
            write(100,"(a)", advance="no"), ","
             write(100,"(i4)", advance="no"), ny
        close(100)

        open(unit=100,file='./mesh/nodes_number.dat')
        do i=1, nx
            do j=1, ny
                write(nodes_file,*) nodes(nodes_number(i,j),1),",", nodes(nodes_number(i,j),2)
                write(100,"(i5)", advance="no") nodes_number(i,j)
                write(100,"(a)", advance="no"), ","
                if((i.ne.nx).and.(j.ne.ny)) then
                    count_e = count_e + 1
                    write(elements_file,*) count_e, elements(count_e,1), elements(count_e,2), elements(count_e,3), elements(count_e,4)
                end if
            end do
        end do
        close(100)

        do j=1, ny
            do i=1, nx
                write(tec_file,*) nodes(nodes_number(i,j),1), nodes(nodes_number(i,j),2)
            end do
        end do
    end subroutine write_mesh_data
end module meshGenerator