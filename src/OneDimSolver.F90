module OneDimSolver 

public :: init1DSolver, &
          solve1DSystem, &

private
   integer, parameter :: int_val = kind(1)
   integer, parameter :: real_val = kind(1.0d0)
   integer (kind=int_val) :: matsize, totalsize
   real (kind=real_val) :: d
   real (kind=real_val), allocatable, target :: x(:)
   real (kind=real_val), allocatable :: bc(:)
   real (kind=real_val), allocatable :: fx(:)
   real (kind=real_val), allocatable, target :: sol(:)
   real (kind=real_val), allocatable, target :: dsol(:)
   real (kind=real_val), allocatable :: Coeff(:,:)
   real (kind=real_val), allocatable :: CoeffInv(:,:)

contains
!  -------------------------------------------------------------
   subroutine init1DSolver()
!  -------------------------------------------------------------
   
   use InputHandler, only : get1DParameters
   
   integer (kind=int_val) :: n, fxchoice, iter
   real (kind=real_val) :: xmin, xmax, bcxmin, bcxmax
 
   call get1DParameters(n, xmin, xmax, bcxmin, bcxmax, fxchoice)
 
   d = (xmax - xmin)/(n - 1.0)
   totalsize = n
   matsize = n - 2
   
   allocate(x(totalsize), bc(matsize), fx(matsize), &
    Coeff(matsize, matsize), CoeffInv(matsize, matsize), &
    sol(totalsize), dsol(totalsize))
   Coeff = 0
   sol = 0
   dsol = 0
   bc = 0
   fx = 0

   x(1) = xmin
   x(totalsize) = xmax
   bc(1) = bcxmin/(d**2)
   bc(matsize) = bcxmax/(d**2)
   sol(1) = bcxmin
   sol(totalsize) = bcxmax

   do iter = 2, matsize + 1
     x(iter) = x(iter - 1) + d
   enddo
   
   call calfx(fxchoice)
   call calCoeffMatrix(d)
   CoeffInv = Coeff

   end subroutine init1DSolver
!  -------------------------------------------------------------
   subroutine calfx(fxchoice)
!  -------------------------------------------------------------

   use InputHandler, only : getPolynomialParams

   integer (kind=int_val), intent(in) :: fxchoice
   integer (kind=int_val) :: iter1, iter2, degree, length
   real (kind=real_val), allocatable :: pcoeff(:)
   real (kind=real_val) :: temp_coeff

   if (fxchoice == 0) then
!    Zero charge density. Laplace equation scenario
     do iter = 1, matsize
       fx(iter) = 0
     enddo
   else if (fxchoice == 1) then
     call getPolynomialParams(degree, pcoeff)
     length = degree + 1
     do iter1 = 1, length
       temp_coeff = pcoeff(iter1)
       do iter2 = 1, matsize
         fx(iter2) = fx(iter2) - temp_coeff*(x(iter2+1))**(iter1 - 1)
       enddo
     enddo
   endif

   end subroutine calfx
!  -------------------------------------------------------------
   subroutine calCoeffMatrix(d)
!  -------------------------------------------------------------

   real (kind=real_val), intent(in) :: d
   integer (kind=int_val) :: iter

   do iter = 1, matsize - 1
     Coeff(iter, iter) = -2.0/(d**2)
     Coeff(iter + 1, iter) = 1.0/(d**2)
     Coeff(iter, iter + 1) = 1.0/(d**2)
   enddo
   Coeff(matsize, matsize) = -2.0/(d**2)
     
   end subroutine calCoeffMatrix
!  -------------------------------------------------------------
   subroutine solve1DSystem(xout, onedimsol, efield)
!  -------------------------------------------------------------

   real (kind=real_val), intent(out), pointer  :: xout(:)
   real (kind=real_val), intent(out), pointer :: onedimsol(:)
   real (kind=real_val), intent(out), pointer :: efield(:)
   integer (kind=int_val) :: info
   integer (kind=int_val) :: nb = 16
   integer (kind=int_val) :: wsize, i, j
   integer (kind=int_val) :: ipiv(matsize)
   real (kind=real_val), allocatable :: work(:)

   wsize = nb*matsize
   allocate(work(wsize))

   call dgetrf(matsize, matsize, CoeffInv, matsize, ipiv, info)
   call dgetri(matsize, CoeffInv, matsize, ipiv, work, wsize, info)
   fx = fx - bc
   do j = 1, matsize
     do i = 1, matsize
       sol(i + 1) = sol(i + 1) + CoeffInv(i, j)*fx(j)
     enddo
   enddo

   onedimsol => sol
   xout => x

   dsol(1) = (0.5/d)*(4*sol(2) - 3*sol(1) - sol(3))
   do i = 2, matsize + 1
     dsol(i) = (0.5/d)*(sol(i + 1) - sol(i - 1))
   enddo
   dsol(totalsize) = (0.5/d)*(sol(totalsize - 2) + 3*sol(i) - &
                              4*sol(totalsize - 1))

   efield => dsol

   end subroutine solve1DSystem
!  -------------------------------------------------------------
end module OneDimSolver
