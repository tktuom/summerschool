! Main solver routines for heat equation solver
module core

contains

  ! Compute one time step of temperature evolution
  ! Arguments:
  !   curr (type(field)): current temperature values
  !   prev (type(field)): values from previous time step
  !   a (real(dp)): update equation constant
  !   dt (real(dp)): time step value
  subroutine evolve(curr, prev, a, dt)

    use heat
    implicit none

    type(field), intent(inout) :: curr, prev
    real(dp) :: a, dt, step, stepy, temp
    integer :: i, j, nx, ny

    nx = curr%nx
    ny = curr%ny

    step = prev%dx
    stepy = prev%dy

    ! TODO: implement the heat equation update
    do i = 1 , nx
       if((i-1)==0 .or. (i+1)==nx+1)then
          cycle
       end if
       do j = 1, ny
       	  if((j-1)==0 .or. (j+1)==ny+1)then
             cycle
          end if
	  temp=((prev%data(i-1,j)-2*prev%data(i,j)+prev%data(i+1,j))/step**2) + &
	       ((prev%data(i,j-1)-2*prev%data(i,j)+prev%data(i,j+1))/stepy**2)

	  curr%data(i,j) = prev%data(i,j) + dt*a*temp
       end do
    end do

  end subroutine evolve

end module core
