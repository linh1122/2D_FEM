module gaussQuad
    use shapeFunctions
    use jacobians

    contains
    subroutine setGauss(xi,w,nOGp)
        integer, intent(in)  :: nOGp
        real   , intent(out) :: xi(nOGp,2)
        real   , intent(out) :: w(nOGp)
        xi(1,1)=-dsqrt(3.d0/5.d0)
        xi(1,2)=xi(1,1)
        xi(2,1)=0.d0
        xi(2,2)=xi(1,1)
        xi(3,1)=-xi(1,1)
        xi(3,2)=xi(1,1)
        xi(4,1)=xi(1,1)
        xi(4,2)=0.d0
        xi(5,1)=0.d0
        xi(5,2)=0.d0
        xi(6,1)=-xi(1,1)
        xi(6,2)=0.d0
        xi(7,1)=xi(1,1)
        xi(7,2)=-xi(1,1)
        xi(8,1)=0.d0
        xi(8,2)=-xi(1,1)
        xi(9,1)=-xi(1,1)
        xi(9,2)=-xi(1,1)
        w(1)=25.d0/81.d0
        w(2)=40.d0/81.d0
        w(3)=w(1)
        w(4)=w(2)
        w(5)=64.d0/81.d0
        w(6)=w(2)
        w(7)=w(1)
        w(8)=w(2)
        w(9)=w(1)
    end subroutine setGauss

    subroutine calKGauss(elementNodes,nONPe,nodes,nOGp,nOn,i,j,Ke_ij,xi,w)
        integer, intent(in) :: nOGp,nONPe,nOn,i,j,elementNodes(nONPe)
        real,    intent(in) :: nodes(nOn,2),xi(nOGp,2),w(nOGp)
        real,    intent(out):: Ke_ij
        integer             :: k
        real                :: a,b,c,d,dpsi(nONPe,2),Je,xxi,xeta,yxi,yeta
        Ke_ij = 0
        do k=1, nOGp
            call jacobian(xi(k,1),xi(k,2),elementNodes,nONPe,nodes,nON,Je,xxi,xeta,yxi,yeta)
            ! print*, Je
            call xietaDerivative(Je,xxi,xeta,yxi,yeta,xix,xiy,etax,etay)
            call dShapeFunc(xi(k,1),xi(k,2),nONPe,dpsi)
            a = dpsi(i,1)*xix + dpsi(i,2)*etax
            b = dpsi(j,1)*xix + dpsi(j,2)*etax
            c = dpsi(i,1)*xiy + dpsi(i,2)*etay
            d = dpsi(j,1)*xiy + dpsi(j,2)*etay

            Ke_ij = Ke_ij + (a*b+c*d)*w(k)*Je
        end do
    end subroutine calKGauss

end module gaussQuad