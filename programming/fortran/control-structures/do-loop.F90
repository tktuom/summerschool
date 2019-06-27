program loops
  implicit none
  ! TODO define parameters nx and ny
  integer, parameter :: nx=10, ny=10
  ! TODO: define real-valued array A
  real :: A(nx,ny)
  integer :: i, j, n

  ! TODO initialize array A here
  do i=1,nx
     do j=1,ny
     	A(i,j)=(i/10.)**2+(j/10.)**2
     end do
  end do	

  ! TODO control structures for if
  j=-2
  i=10
  n=0
  if (j<0) then
!     write(*,*) j
  end if
  if (i>100) then
!     write(*,*) "Why?"
  else
     i=i+100
!     write(*,*) i
  end if
  if (N==0) then
!     write(*,*) 'Cool'
  end if

  !--------------------------------------------------
  ! Print out the array
  ! the ':' syntax means the whole row, see the Fortran arrays lecture
  !do i = 1, nx
  !   write(*, '(12F6.2)') A(i,:)
  !end do

  ! TODO Fibonacci
  j=0
  n=1
  i=0
  do while (i<=100)
     i=j+n
     write(*,*) i
     j=n
     n=i
  end do

end program loops
