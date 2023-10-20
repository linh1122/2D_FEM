module shapeFunctions


    contains
    subroutine shapeFunc(xi,eta,numberOfNodePerElement,psi)
        integer, intent(in) :: numberOfNodePerElement
        real   , intent(in) :: xi,eta
        real   , intent(out):: psi(numberOfNodePerElement)

        if(numberOfNodePerElement.eq.4) then
            psi(1)=0.25d0*(1.d0-xi)*(1.d0-eta)
            psi(2)=0.25d0*(1.d0+xi)*(1.d0-eta)
            psi(3)=0.25d0*(1.d0+xi)*(1.d0+eta)
            psi(4)=0.25d0*(1.d0-xi)*(1.d0+eta)
        end if
    end subroutine shapeFunc

    subroutine dShapeFunc(xi,eta,numberOfNodePerElement,dpsi)
        integer, intent(in) :: numberOfNodePerElement
        real   , intent(in) :: xi,eta
        real   , intent(out):: dpsi(numberOfNodePerElement,2)

        if(numberOfNodePerElement.eq.4) then
            dpsi(1,1)=0.25d0*(-1.d0+eta)
            dpsi(1,2)=0.25d0*(-1.d0+xi)
            dpsi(2,1)=0.25d0*( 1.d0-eta)
            dpsi(2,2)=0.25d0*(-1.d0-xi)
            dpsi(3,1)=0.25d0*( 1.d0+eta)
            dpsi(3,2)=0.25d0*( 1.d0+xi)
            dpsi(4,1)=0.25d0*(-1.d0-eta)
            dpsi(4,2)=0.25d0*( 1.d0-xi)
        end if
    end subroutine dShapeFunc

end module shapeFunctions