program laplacian
  use iso_fortran_env, only : REAL64
  implicit none

  integer, parameter :: dp = REAL64
  real(dp), dimension(:,:), allocatable :: A, L
  real(dp) :: dx, dy, x, y
  integer :: nx, ny, i, j, alloc_stat

  write (*,*)  'Give number of rows and columns for matrix A:'
  read (*,*) nx, ny
  ! Grid spacing
  dx = 1.0/real(nx)
  dy = 1.0/real(ny)
  ! TODO: allocate matrices
  allocate(A(nx,ny), stat=alloc_stat)
  if(alloc_stat/=0) stop

  allocate(L(nx,ny), stat=alloc_stat)
  if(alloc_stat/=0) stop

  ! initialize array A(x,y) = (x^2 + y^2) in the domain [0:1,0:1]
  y = 0.0
  do j = 1, ny
     x = 0.0
     do i = 1, nx
        A(i,j) =  x**2 + y**2
        x = x + dx
     end do
     y = y + dy
  end do

  ! TODO: Compute Laplacian of A and save it to array L
  do i=1,nx
     if((i-1)==0 .or. (i+1)==nx+1)then
          cycle
        end if
     do j=1,ny
     	if((j-1)==0 .or. (j+1)==nx+1)then
          cycle
	end if
        L(i,j)=((A(i-1,j)-2*A(i,j)+A(i+1,j))/dx**2)+((A(i,j-1)-2*A(i,j)+A(i,j+1))/dy**2)

     end do
  end do
  ! TODO: Printing of the arrays
  write(*,*) "Original array:"
  do i=1,nx
     write(*,'(12F6.2)') A(i,:)
  end do

  write(*,*) "Laplacian of the array:"
  do i=1,nx
     write(*,'(12F6.2)') L(i,:)
  end do


  ! Analytically, the Laplacian of the function is nabla^2 A(x,y) = 4


end program laplacian
