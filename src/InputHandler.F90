module InputHandler

public :: getInputData, &
          getDimension, &
          get1DParameters, &
          get2DParameters, &
          getPolynomialParams
  
      interface getKeyValue
          module procedure getKeyValue_r
          module procedure getKeyValue_i
          module procedure getKeyValue_str
      end interface getKeyValue

private
   integer, parameter :: int_val = kind(1)
   integer, parameter :: real_val = kind(1.0d0)
   integer, parameter :: line_limit = 100
   integer, parameter :: letter_limit = 200
   integer (kind=int_val) :: file_length
   character(len=letter_limit), allocatable :: fdata(:,:)
 

contains
!  -------------------------------------------------------
   subroutine getInputData()
!  --------------------------------------------------------

   character(len=letter_limit) :: filename
   character(len=letter_limit), allocatable :: dump(:,:)
   integer (kind=int_val) :: io, i, iter

   allocate(dump(line_limit, 2))

   call getarg(1, filename)
   open (unit=1, file=filename, status='old',action='read')
   i = 0
   do
     i = i + 1
     read(1,*,iostat=io) dump(i,1), dump(i,2)
     if (io < 0) then 
       exit
     endif
   enddo
   close(1)

   file_length = i
   allocate(fdata(i, 2))
   

   do iter = 1, i
     fdata(iter, 1) = trim(dump(iter, 1))
     fdata(iter, 2) = trim(dump(iter, 2))
   enddo
   deallocate(dump)

   end subroutine getInputData
!  ---------------------------------------------------------
   subroutine getKeyValue_r(key_string, val)
!  ---------------------------------------------------------
  
   character(len=*), intent(in) :: key_string
   real (kind=real_val), intent(out) :: val
   integer (kind=int_val) :: iter

   do iter = 1, file_length
     if(fdata(iter,1) .eq. key_string) then
       read(fdata(iter,2),*) val
       exit
     endif
   enddo

   end subroutine getKeyValue_r
!  ---------------------------------------------------------
   subroutine getKeyValue_i(key_string, val)
!  ---------------------------------------------------------

   character(len=*), intent(in) :: key_string
   integer (kind=int_val), intent(out) :: val
   integer (kind=int_val) :: iter

   do iter = 1, file_length
     if(fdata(iter,1) .eq. key_string) then
       read(fdata(iter,2),*) val
       exit
     endif
   enddo

   end subroutine getKeyValue_i
!  ---------------------------------------------------------
   subroutine getKeyValue_str(key_string, val)
!  ---------------------------------------------------------

   character(len=*), intent(in) :: key_string
   character(len=*), intent(out) :: val
   integer (kind=int_val) :: iter

   do iter = 1, file_length
     if(fdata(iter,1) .eq. key_string) then
       read(fdata(iter,2),*) val
       exit
     endif
   enddo

   end subroutine getKeyValue_str
!  ---------------------------------------------------------
   function getDimension() result(dimval)
!  ---------------------------------------------------------

   integer (kind=int_val) :: dimval
   real (kind=real_val) :: keyval

   call getKeyValue("Dimension", keyval)
   dimval = int(keyval)

   end function getDimension
!  ------------------------------------------------------------------
   subroutine get1DParameters(n, xmin, xmax, bcmin, bcmax, fxchoice)
!  ------------------------------------------------------------------

   integer (kind=int_val), intent(out) :: n, fxchoice
   real (kind=real_val), intent(out) :: xmin, xmax, bcmin, bcmax
 
   call getKeyValue("NX", n)
   call getKeyValue("Xmin", xmin)
   call getKeyValue("Xmax", xmax)
   call getKeyValue("BCmin", bcmin)
   call getKeyValue("BCmax", bcmax)
   call getKeyValue("FxType", fxchoice)
 
   end subroutine get1DParameters
!  ------------------------------------------------------------------
   subroutine get2DParameters(nx, ny, xmin, xmax, ymin, ymax, fxchoice)
!  ------------------------------------------------------------------

   integer (kind=int_val), intent(out) :: nx, ny, fxchoice
   real (kind=real_val), intent(out) ::  xmin, xmax, ymin, ymax

   call getKeyValue("NX", nx)
   call getKeyValue("NY", ny)
   call getKeyValue("Xmin", xmin)
   call getKeyValue("Xmax", xmax)
   call getKeyValue("Ymin", ymin)
   call getKeyValue("Ymax", ymax)
   call getKeyValue("FxType", fxchoice)

   end subroutine get2DParameters
!  ------------------------------------------------------------------
   subroutine getPolynomialParams(str_deg, str_coeff,degree,pcoeff)
!  ------------------------------------------------------------------
 
   character (len=*), intent(in) :: str_deg, str_coeff
   real (kind=real_val), allocatable, intent(out) :: pcoeff(:)
   integer (kind=int_val), intent(out) :: degree
   character (len=letter_limit) :: raw_pcoeff
   integer (kind=int_val) :: idx, i, length

   call getKeyValue(str_deg, degree)
   call getKeyValue(str_coeff, raw_pcoeff)

   length = degree + 1
   allocate(pcoeff(length))
   do i = 1, length
     idx = index(raw_pcoeff,"|")
     if (idx == 0) then
       read(raw_pcoeff,*) pcoeff(i)
     else
       read(raw_pcoeff(:idx - 1), *) pcoeff(i)
     endif
     raw_pcoeff = raw_pcoeff(idx + 1:)
   enddo

   end subroutine getPolynomialParams
!  ------------------------------------------------------------------
end module InputHandler
