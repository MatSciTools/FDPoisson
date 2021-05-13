program fdpoisson

   use InputHandler, only : getInputData, getDimension
   use OutputHandler, only : output1DSolution, output2DSolution
   use OneDimSolver, only : init1DSolver, solve1DSystem
   use TwoDimSolver, only : init2DSolver, solve2DSystem

   integer, parameter :: int_val = kind(1)
   integer, parameter :: real_val = kind(1.0d0) 
   integer (kind=int_val) :: dval
   real (kind=real_val), pointer :: sol1D(:), sol2D(:,:)
   real (kind=real_val), pointer :: x(:), y(:)
   real (kind=real_val), pointer :: Ex(:)

   call getInputData()
   dval = getDimension()

   write(*,*) "FDPoisson : ", dval, " Dimensional Calculation"
   if (dval == 1) then
!    1D Poisson Equation needs to be solved
     call init1DSolver()
     call solve1DSystem(x, sol1D, Ex)
     call output1DSolution(x, sol1D, Ex) 
   else if (dval == 2) then
!    2D Poisson Equation needs to be solved
     call init2DSolver()
     call solve2DSystem(x, y, sol2D)
     call output2DSolution(x, y, sol2D)
   endif
 
   write(*,*) "Potential calculated succesfully and stored in file: potential"

end program fdpoisson
