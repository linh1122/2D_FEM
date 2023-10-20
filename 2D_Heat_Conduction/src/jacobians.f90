module jacobians
    use shapeFunctions

    contains
    subroutine jacobian(xi,eta,elementNodes,nONPe,nodes,nON,Je,xxi,xeta,yxi,yeta)
        integer, intent(in) :: nONPe,nON,elementNodes(nONPe)
        real,    intent(in) :: nodes(nOn,2),xi,eta
        real,    intent(out):: Je,xxi,xeta,yxi,yeta
        real                :: dpsi(nONPe,2)

        call dShapeFunc(xi,eta,nONPe,dpsi)
        call xyDerivative(elementNodes,nONPe,nodes,nON,dpsi,xxi,xeta,yxi,yeta)

        Je = xxi*yeta - xeta*yxi
    end subroutine jacobian

    subroutine xyDerivative(elementNodes,nONPe,nodes,nON,dpsi,xxi,xeta,yxi,yeta)
        integer, intent(in) :: nONPe,nON,elementNodes(nONPe)
        real,    intent(in) :: nodes(nOn,2),dpsi(nONPe,2)
        real,    intent(out):: xxi,xeta,yxi,yeta
        integer             :: i
        xxi =0.d0
        xeta=0.d0
        yxi =0.d0
        yeta=0.d0

        do i=1, nONPe
            xxi = xxi + nodes(elementNodes(i),1)*dpsi(i,1)
            xeta= xeta+ nodes(elementNodes(i),1)*dpsi(i,2)
            yxi = yxi + nodes(elementNodes(i),2)*dpsi(i,1)
            yeta= yeta+ nodes(elementNodes(i),2)*dpsi(i,2)
        end do
    end subroutine xyDerivative

    subroutine xietaDerivative(Je,xxi,xeta,yxi,yeta,xix,xiy,etax,etay)
        real, intent(in) :: Je,xxi,xeta,yxi,yeta
        real, intent(out):: xix,xiy,etax,etay

        xix =  (1.d0/Je)*yeta
        xiy = -(1.d0/Je)*xeta
        etax= -(1.d0/Je)*yxi
        etay=  (1.d0/Je)*xxi
    end subroutine xietaDerivative
end module jacobians