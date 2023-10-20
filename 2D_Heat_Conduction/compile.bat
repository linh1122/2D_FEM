cd ./include
    del *.obj
    del *.mod 
    ifort -c ../src/globalVariables.F90 
    ifort -c ../src/meshGenerator.F90 
    ifort -c ../src/boundaryIdentify.F90 
    ifort -c ../src/shapeFunctions.F90 
    ifort -c ../src/jacobians.F90 
    ifort -c ../src/gaussQuad.F90 
    ifort -c ../src/elementMatrix.F90 
    ifort -c ../src/assembly.F90 
    ifort -c ../src/boundaryCondition.F90 
    ifort -c ../src/storeNonBoundaryData.F90 
    ifort -c ../src/storeCompressedMatrix.F90 
    ifort -c ../src/solvers.F90 
    ifort -c ../src/writeResult.F90 
    ifort /Qmkl-ilp64 ../main.f90 *.obj -o ./main.exe 
    @REM ifort /Qmkl-ilp64 ../fgmres_st_criterion.f90 *.obj -o ./fgmres_st_criterion.exe
    move main.exe ../
    @REM move fgmres_st_criterion.exe ../
    cd..