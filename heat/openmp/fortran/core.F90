! Main solver routines for heat equation solver
module core
  use heat

contains

  ! Exchange the boundary data between MPI tasks
  subroutine exchange(field0, parallel,req)
    use mpi_f08

    implicit none

    type(field), intent(inout) :: field0
    type(parallel_data), intent(in) :: parallel
    
    integer :: ierr
    type(mpi_datatype) :: column
    type(mpi_request), intent(inout) :: req(2)
    type(mpi_request) :: uest(2)
    type(mpi_status) :: status(2)

    integer :: tag

    tag= parallel%rank 

    !From left to right
    call mpi_isend(field0%data(0,field0%ny),2+field0%nx,mpi_double_precision,parallel%nright,tag,&
                   mpi_comm_world,uest(1),ierr)

    call mpi_irecv(field0%data,2+field0%nx,mpi_double_precision,parallel%nleft,tag-1,& 
		   mpi_comm_world, req(1), ierr)

    !From right to left
    call mpi_isend(field0%data(0,1),2+field0%nx,mpi_double_precision,parallel%nleft,tag,&
                   mpi_comm_world, uest(2), ierr)
    call mpi_irecv(field0%data(0,field0%ny+1),2+field0%nx,mpi_double_precision,&
                   parallel%nright,tag+1,mpi_comm_world, req(2) , ierr)
    




  end subroutine exchange

  ! Compute one time step of temperature evolution
  ! Arguments:
  !   curr (type(field)): current temperature values
  !   prev (type(field)): values from previous time step
  !   a (real(dp)): update equation constant
  !   dt (real(dp)): time step value
  subroutine evolve(curr, prev, a, dt,req)
    use mpi_f08
    implicit none

    type(field), intent(inout) :: curr, prev
    real(dp) :: a, dt
    integer :: i, j, nx, ny, ierr
    type(mpi_status) :: status(2)
    type(mpi_request), intent(inout) :: req(2)
    
    nx = curr%nx
    ny = curr%ny

    do j = 2, ny-1
       do i = 1, nx
          curr%data(i, j) = prev%data(i, j) + a * dt * &
               & ((prev%data(i-1, j) - 2.0 * prev%data(i, j) + &
               &   prev%data(i+1, j)) / curr%dx**2 + &
               &  (prev%data(i, j-1) - 2.0 * prev%data(i, j) + &
               &   prev%data(i, j+1)) / curr%dy**2)
       end do
    end do

    call mpi_waitall(2,req,status,ierr)

    do i = 1, nx
       curr%data(i, 1) = prev%data(i, 1) + a * dt * &
	     & ((prev%data(i-1, 1) - 2.0 * prev%data(i, 1) + &
             &   prev%data(i+1, 1)) / curr%dx**2 + &
             &  (prev%data(i, 0) - 2.0 * prev%data(i, 1) + &
             &   prev%data(i, 0)) / curr%dy**2)
     end do

     do i = 1, nx
       curr%data(i, ny) = prev%data(i, ny) + a * dt * &
             & ((prev%data(i-1, ny) - 2.0 * prev%data(i, ny) + &
             &   prev%data(i+1, ny)) / curr%dx**2 + &
             &  (prev%data(i, ny-1) - 2.0 * prev%data(i, ny) + &
             &   prev%data(i, ny+1)) / curr%dy**2)
     end do

  end subroutine evolve

end module core
