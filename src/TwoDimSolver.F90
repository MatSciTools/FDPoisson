module TwoDimSolver

public :: init2DSolver, &
          solve2DSystem

private
   integer, parameter :: int_val = kind(1)
   integer, parameter :: real_val = kind(1.0d0)
   integer (kind=int_val) :: nx, ny, mtx, mty, matsize, fxchoice
   real (kind=real_val) :: dx, dy
   real (kind=real_val), allocatable, target :: x(:)
   real (kind=real_val), allocatable, target :: y(:)
   real (kind=real_val), allocatable, target :: phi(:,:)
   real (kind=real_val), allocatable, target :: Ex(:,:)
   real (kind=real_val), allocatable, target :: Ey(:,:)
   real (kind=real_val), allocatable :: bcx(:,:), bcy(:,:)
   real (kind=real_val), allocatable :: fxy(:), bcvecx(:), bcvecy(:)
   real (kind=real_val), allocatable :: Coeff(:,:), CoeffInv(:,:)

contains
!  ----------------------------------------------------------
   subroutine init2DSolver()
!  ----------------------------------------------------------
!  Initialize variables   

   use InputHandler, only : get2DParameters 
   
   integer (kind=int_val) :: i, j
   real (kind=real_val) :: xmin, xmax, ymin, ymax
   
   call get2DParameters(nx, ny, xmin, xmax, ymin, ymax, fxchoice)
   
   mtx = nx - 2
   mty = ny - 2
   matsize = mtx*mty
   dx = (xmax - xmin)/(nx - 1.0)
   dy = (ymax - ymin)/(ny - 1.0)
   allocate(x(nx), y(ny), phi(ny, nx), Ex(ny, nx), &
    Ey(ny, nx), fxy(matsize), bcx(2, nx), bcy(2, ny), &
    bcvecx(matsize), bcvecy(matsize), Coeff(matsize, matsize), &
    CoeffInv(matsize, matsize))
   
   phi = 0
   bcx = 0; bcvecx = 0
   bcy = 0; bcvecy = 0; fxy = 0
   Ex = 0; Ey = 0
   x(1) = xmin
   do i = 2, nx-1
     x(i) = x(i-1) + dx
   enddo
   x(nx) = xmax
   y(1) = ymin
   do i = 2, ny-1
     y(i) = y(i-1) + dy
   enddo
   y(ny) = ymax

   call calfxy(fxchoice)
   call calBCFunction("X")
   call calBCFunction("Y")

   do i = 2, nx - 1
     phi(1, i) = bcx(1, i)
     phi(ny, i) = bcx(2, i)
   enddo

   do i = 1, ny
     phi(i, 1) = bcy(1, i)
     phi(i, nx) = bcy(2, i)
   enddo
  
   call calBCVector(dx, dy)
   call calCoeffMatrix(dx, dy)
   CoeffInv = Coeff

   end subroutine init2DSolver
!  ----------------------------------------------------------
   subroutine calfxy(fxchoice)
!  ----------------------------------------------------------
!  Get polynomial from input data and populate fxy matrix

   use InputHandler, only : getPolynomialParams
   integer (kind=int_val), intent(in) :: fxchoice
   integer (kind=int_val) :: deg, xdeg, ydeg, iter1, iter2, iter3
   real (kind=real_val), allocatable :: xyc(:), xc(:), yc(:)
   real (kind=real_val) :: tmpx, tmpy
 
   if (fxchoice == 0) then
   ! 2D Laplace Equation. Do nothing, as fxy is already 0
   else if (fxchoice == 1) then
   ! 2D Poisson Equation with rho = sum_i c_i x^i y^(n-i)
     call getPolynomialParams("XYDegree", "XYPCoeff", deg, xyc)
     length = degree + 1
     do iter1 = 1, length
       temp_coeff = xyc(iter1)
       do iter2 = 1, mtx
         do iter3 = 1, mty
           fxy((iter3 - 1)*mtx + iter2) = fxy((iter3 - 1)*mtx + iter2) &
          - temp_coeff*(x(iter2+1)**(iter1 - 1))*(y(iter3+1)**(deg - iter1 + 1))
         enddo
       enddo
     enddo
   else if (fxchoice == 2) then
   ! 2D Poisson Equation with rho = (sum_i c_i x^i)*(sum_j c_j y^j)
     call getPolynomialParams("XDegree", "XPCoeff", xdeg, xc)
     call getPolynomialParams("YDegree", "YPCoeff", ydeg, yc)
     do iter2 = 1, mtx
       do iter3 = 1, mty
         tmpx = 0; tmpy = 0
         length = xdeg + 1
         do iter1 = 1, length
           temp_coeff = xc(iter1)
           tmpx = tmpx - temp_coeff*(x(iter2+1)**(iter1 - 1))
         enddo
         length = ydeg + 1
         do iter1 = 1, length
           temp_coeff = yc(iter1)
           tmpy = tmpy - temp_coeff*(y(iter3+1)**(iter1 - 1))
         enddo
         fxy((iter3 - 1)*mtx + iter2) = tmpx*tmpy
       enddo
     enddo
   endif
   
   end subroutine calfxy
!  ----------------------------------------------------------
   subroutine calBCFunction(dir)
!  ----------------------------------------------------------
!  Get the functions associated with the Boundary Conditions

   use InputHandler, only : getPolynomialParams

   character (len=*), intent(in) :: dir
   character (len=200) :: istr(2)
   integer (kind=int_val) :: i, iter1, iter2, degree, length
   real (kind=real_val) :: temp_coeff
   real (kind=real_val), allocatable :: pcoeff(:)

   do i = 1, 2
     write(istr(i),"(I1)") i
     call getPolynomialParams("BC" // dir // trim(istr(i)) // "Degree", &
                   "BC" // dir // trim(istr(i)) // "Coeff", degree, pcoeff)
     length = degree + 1
     do iter1 = 1, length
       temp_coeff = pcoeff(iter1)
       if (dir .eq. "X") then
         do iter2 = 1, nx
           bcx(i, iter2) = bcx(i, iter2) + temp_coeff*(x(iter2))**(iter1 - 1)
         enddo
       else
         do iter2 = 1, ny
           bcy(i, iter2) = bcy(i, iter2) + temp_coeff*(y(iter2))**(iter1 - 1)
         enddo
       endif
     enddo
     deallocate(pcoeff)
   enddo
 
   end subroutine calBCFunction
!  ----------------------------------------------------------
   subroutine calBCVector(dx, dy)
!  ----------------------------------------------------------
!  Calculate the Boundary Condition Vectors used in the calculation

   real (kind=real_val), intent(in) :: dx, dy
   integer (kind=int_val) :: iter

   do iter = 1, mtx
     bcvecx(iter) = (1.0/dy**2)*bcx(1, iter+1)
     bcvecx(mtx*(mty - 1) + iter) = (1.0/dy**2)*bcx(2, iter+1)
   enddo
  
   do iter = 1, mty
     bcvecy((iter - 1)*mtx + 1) = (1.0/dx**2)*bcy(1, iter+1)
     bcvecy(iter*mtx) = (1.0/dx**2)*bcy(2, iter+1)
   enddo

   end subroutine calBCVector
!  ----------------------------------------------------------
   subroutine calCoeffMatrix(dx, dy)
!  ----------------------------------------------------------
!  Calculation Finite Difference Coefficient Matrix

   real (kind=real_val), intent(in) :: dx, dy
   integer (kind=int_val) :: iter1, iter2
   real (kind=real_val), allocatable :: A(:,:), iden(:,:)

   allocate(A(mtx, mtx), iden(mtx, mtx))
   A = 0; iden = 0;
   do iter1 = 1, mtx-1
     iden(iter1, iter1) = 1.0/(dy**2)
     A(iter1, iter1) = -2.0/(dx**2) - 2.0/(dy**2)
     A(iter1, iter1+1) = 1.0/(dx**2)
     A(iter1+1, iter1) = 1.0/(dx**2)
   enddo
   A(mtx, mtx) = -2.0/(dx**2) - 2.0/(dy**2)
   iden(mtx, mtx) = 1.0/(dy**2)

   do iter1 = 1, mty - 1
     Coeff((iter1 - 1)*mtx + 1:iter1*mtx, (iter1 -1)*mtx + 1:iter1*mtx) = A
     Coeff((iter1 - 1)*mtx + 1:iter1*mtx, iter1*mtx + 1: (iter1+1)*mtx) = iden
     Coeff(iter1*mtx + 1:(iter1+1)*mtx, (iter1 - 1)*mtx + 1:iter1*mtx) = iden
   enddo

   Coeff((mty - 1)*mtx + 1: mty*mtx, (mty - 1)*mtx + 1: mty*mtx) = A

   end subroutine calCoeffMatrix
!  ----------------------------------------------------------
   subroutine solve2DSystem(xp, yp, solp, Fieldx, Fieldy)
!  ----------------------------------------------------------
!  Invert matrix, find potential and field

   real (kind=real_val), intent(out), pointer :: xp(:), yp(:)
   real (kind=real_val), intent(out), pointer :: solp(:,:)
   real (kind=real_val), intent(out), pointer :: Fieldx(:,:), Fieldy(:,:)
   real (kind=real_val) :: sol(matsize)
   integer (kind=int_val) :: info
   integer (kind=int_val) :: nb = 16
   integer (kind=int_val) :: wsize, i, j
   integer (kind=int_val) :: ipiv(matsize)
   real (kind=real_val), allocatable :: work(:)

   xp => x
   yp => y
   wsize = nb*matsize
   allocate(work(wsize))  
   sol = 0
   fxy = fxy - bcvecx - bcvecy  

   call dgetrf(matsize, matsize, CoeffInv, matsize, ipiv, info)
   call dgetri(matsize, CoeffInv, matsize, ipiv, work, wsize, info)
   
   do j = 1, matsize
     do i = 1, matsize
       sol(i)  = sol(i) + CoeffInv(i, j)*fxy(j)
     enddo
   enddo

   do j = 1, mtx
     do i = 1, mty
       phi(i+1, j+1) = sol((i -1)*mtx + j)
     enddo 
   enddo

   do j = 1, ny
     Ex(j,1) = (0.5/dx)*(4*phi(j,2) - 3*phi(j,1) - phi(j,3))
     do i = 2, nx-1
       Ex(j,i) = (0.5/dx)*(phi(j,i+1) - phi(j,i-1))
     enddo
     Ex(j,nx) = (0.5/dx)*(phi(j,nx-2) + 3*phi(j,nx) - 4*phi(j,nx-1))
   enddo

   do j = 1, nx
     Ey(1, j) = (0.5/dy)*(4*phi(2,j) - 3*phi(1,j) - phi(3, j))
     do i = 2, ny-1
       Ey(i, j) = (0.5/dy)*(phi(i+1,j) - phi(i-1,j))
     enddo
     Ey(ny, j) = (0.5/dy)*(phi(ny-2,j) + 3*phi(ny,j) - 4*phi(ny-1,j))
   enddo

   solp => phi
   Fieldx => Ex
   Fieldy => Ey

   end subroutine solve2DSystem
!  ----------------------------------------------------------
end module TwoDimSolver
