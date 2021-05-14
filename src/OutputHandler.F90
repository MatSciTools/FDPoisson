module OutputHandler

public :: output1DSolution, &
          output2DSolution

private
   integer, parameter :: int_val = kind(1)
   integer, parameter :: real_val = kind(1.0d0)

contains
!  ------------------------------------------------------------
   subroutine output1DSolution(x, sol, Ex)
!  ------------------------------------------------------------

   real (kind=real_val), intent(in), pointer :: x(:)
   real (kind=real_val), intent(in), pointer :: sol(:)
   real (kind=real_val), intent(in), pointer :: Ex(:)
   integer (kind=int_val) :: i, n
   character (len=20) :: outfile

   n = size(x)
   outfile = "potential"
   open(2, file=outfile, action='write')
 
   write(2,*) "           x            ", &
              "           Phi(x)       ", &
              "           E(x)         "
   do i = 1, n
     write(2,*) x(i), sol(i), Ex(i)
   enddo 
   close(2)

   end subroutine output1DSolution
!  -------------------------------------------------------------
   subroutine output2DSolution(x, y, phi, Ex, Ey)
!  -------------------------------------------------------------

   real (kind=real_val), intent(in), pointer :: x(:)
   real (kind=real_val), intent(in), pointer :: y(:)
   real (kind=real_val), intent(in), pointer :: phi(:,:)
   real (kind=real_val), intent(in), pointer :: Ex(:,:), Ey(:,:)
   integer (kind=int_val) :: i, j, nx, ny
   character (len=20) :: outfile   

   nx = size(x)
   ny = size(y)
   outfile = "potential"
   open(2, file=outfile, action='write')
   write(2,*) "          x             ", &
              "          y             ", &
              "         Phi            ", &
              "          Ex            ", &
              "          Ey            "

   do j = 1, ny
     do i = 1, nx
       write(2,*) x(i), y(j), phi(j, i), Ex(j,i), Ey(j,i)
     enddo
   enddo

   end subroutine output2DSolution
!  -------------------------------------------------------------
end module OutputHandler
