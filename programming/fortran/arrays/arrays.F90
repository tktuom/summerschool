program arrays
  implicit none
  ! TODO: Define the array A
  real, allocatable :: A(:,:)
  real, allocatable :: idx(:)
  real :: x, y, dx, dy
  integer :: nx, ny, i, j, alloc_stat

  write (*,*) 'Give number of rows and columns for matrix A:'
  read (*,*) nx, ny
  dx = 1.0/real(nx-1)
  dy = 1.0/real(ny-1)

  ! TODO: allocate the array A
  allocate (A(0:nx-1,0:ny-1), stat=alloc_stat)
  if (alloc_stat /=0) stop

  allocate (idx(ny), stat=alloc_stat)
  if (alloc_stat /=0) stop
  
  idx(1:ny)=[(i,i=0,ny-1)]
  write(*,'(12F6.2)') idx(:)

  ! TODO: initalize the array A
  do i=0,nx-1
     A(i,:)=(i*dx)**2+(idx(:)*dy)**2
  end do 

  ! TODO: Print out the array

  do i=0,nx-1
     write(*,'(12F6.2)') A(i,:)
  end do

end program arrays
