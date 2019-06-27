module heat_mod
  use iso_fortran_env, only : real64
  implicit none
  integer, parameter :: rp=real64
  type field
       integer :: nx,ny          !Number of gridpoint
       real (kind=rp) :: dx, dy     !Grid spacing
       real (kind=rp), dimension(:,:), allocatable :: points    !Array of gridpoints
  end type field


  contains
    
    subroutine initialise(grid)
      implicit none
      type(field), intent(out) :: grid
      integer :: alloc_stat, i, j

      allocate(grid % points(grid % nx,grid % ny), stat=alloc_stat)
      if (alloc_stat /= 0) stop
      grid % points(1,1) = 0


      do i=2, grid%nx
      	 grid % points(i,1) = grid % points(i-1,1) + grid % dx
         do j=2, grid%ny
	    grid % points(i,j) = grid % points(i,j-1) + grid % dy
	 end do
      end do

    end subroutine

    subroutine write_out(grid)
      implicit none
      type(field), intent(in) :: grid
      integer :: i

      do i=1, grid%nx
         write(*,'(12f6.3)') grid % points(i,:)
      end do

    end subroutine 

end module heat_mod 
