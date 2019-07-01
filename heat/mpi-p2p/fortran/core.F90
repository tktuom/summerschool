! Main solver routines for heat equation solver
module core
  use heat

contains

  ! Exchange the boundary data between MPI tasks
  subroutine exchange(field0, parallel)
    use mpi_f08

    implicit none

    type(field), intent(inout) :: field0
    type(parallel_data), intent(in) :: parallel
    
    integer :: ierr
    type(mpi_datatype) :: column
    type(mpi_request) :: req(2), uest
    type(mpi_status) :: status

    integer :: tag

    tag= parallel%rank 

    !From left to right
    call mpi_sendrecv(field0%data(0,field0%ny),2+field0%nx,mpi_double_precision,parallel%nright,tag,&
                      field0%data,2+field0%nx,mpi_double_precision,parallel%nleft,tag-1,& 
		      mpi_comm_world, status)

    !From right to left
    call mpi_sendrecv(field0%data(0,1),2+field0%nx,mpi_double_precision,parallel%nleft,tag,&
                      field0%data(0,field0%ny+1),2+field0%nx,mpi_double_precision,&
                      parallel%nright,tag+1,mpi_comm_world, status)
    
    ! TODO start: implement halo exchange

!    call mpi_type_vector(field0%nx+2,1,field0%nx+2, mpi_integer, column, ierr)
!    call mpi_type_commit(column,ierr)

    ! Send to left, receive from right
!    if (parallel%rank > 0) then
!       call mpi_isend(field0%data(1,1), 1, column,parallel%nleft,tag,mpi_comm_world,req,ierr)     
!       call mpi_isend(field0%data(0,1), field0%nx+2,
!    mpi_integer,parallel%nleft,tag,mpi_comm_world,uest,ierr)     
!    end if
!    if (parallel%rank < parallel%size-1) then
!       call mpi_irecv(field0%data(1,int(field0%ny-1)), 1, column,parallel%nright,int(tag+1),mpi_comm_world,req,ierr)
!       call mpi_irecv(field0%data(0,int(field0%ny+1)), field0%nx+2, mpi_integer,parallel%nright,int(tag+1),mpi_comm_world,req(1),ierr)
!    end if
    ! Send to right, receive from left

!    if (parallel%rank < parallel%size -1) then
!       call mpi_isend(field0%data(1,int(field0%ny-1)), 1,column,parallel%nright,tag,mpi_comm_world,req,ierr)
!       call mpi_isend(field0%data(0,int(field0%ny)), field0%nx+2, mpi_integer,parallel%nright,tag,mpi_comm_world,uest,ierr)
!    end if
!    if (parallel%rank > 0) then
!       call mpi_irecv(field0%data(1,1), 1,column,parallel%nleft,int(tag-1),mpi_comm_world,req,ierr)
!       call mpi_irecv(field0%data(0,0), field0%nx+2, mpi_integer,parallel%nleft,int(tag-1),mpi_comm_world,req(2),ierr)
!    end if
  
!    if (parallel%rank<parallel%size-1) then 
!       call mpi_wait(req(1),status(1),ierr)
!    end if
!    if (parallel%rank >0) then
!       call mpi_wait(req(2),status(2),ierr)
!    end if
!    call mpi_type_free(column,ierr)
    ! TODO end

  end subroutine exchange

  ! Compute one time step of temperature evolution
  ! Arguments:
  !   curr (type(field)): current temperature values
  !   prev (type(field)): values from previous time step
  !   a (real(dp)): update equation constant
  !   dt (real(dp)): time step value
  subroutine evolve(curr, prev, a, dt)

    implicit none

    type(field), intent(inout) :: curr, prev
    real(dp) :: a, dt
    integer :: i, j, nx, ny

    nx = curr%nx
    ny = curr%ny

    do j = 1, ny
       do i = 1, nx
          curr%data(i, j) = prev%data(i, j) + a * dt * &
               & ((prev%data(i-1, j) - 2.0 * prev%data(i, j) + &
               &   prev%data(i+1, j)) / curr%dx**2 + &
               &  (prev%data(i, j-1) - 2.0 * prev%data(i, j) + &
               &   prev%data(i, j+1)) / curr%dy**2)
       end do
    end do
  end subroutine evolve

end module core
