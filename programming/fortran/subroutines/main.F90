program subroutines
  use laplacian_mod
  implicit none
  ! TODO: define the arrays
  real, dimension(:,:), allocatable :: previous, current
  integer :: nx, ny

  write (*,*) 'Give number of rows and columns for matrix A:'
  read (*,*) nx, ny

  allocate(previous(nx,ny), current(nx,ny))

  ! initialize the array
  call initialize(previous)

  write(*,*) 'Array initialised:'
  call write_field(previous)

  ! compute the Laplacian
  call laplacian_edges(current, previous)
  write(*,*) 'Laplacian calculated without edges:'
  ! print the result array
  call write_field(current)

  ! compute the Laplacian
  call laplacian(current, previous)
  write(*,*) 'Laplacian calculated for periodic grid:'
  ! print the result array
  call write_field(current)


end program subroutines
