program datatype1
  use mpi_f08
  implicit none

  integer, dimension(8,8) :: array
  integer, dimension(4) :: disp, blocklen
  integer :: rank, ierr
  !TODO: declare variable for datatype
  type(mpi_datatype) :: rowtype, indx
  integer :: i, j
  type(mpi_status) :: status

  call mpi_init(ierr)
  call mpi_comm_rank(MPI_COMM_WORLD, rank ,ierr)

  ! initialize arrays
  if (rank == 0) then

     do i=1,8
        do j=1,8
           array(i,j) = i*10 + j
        end do
     end do
  else
     array(:,:) = 0
  end if

  do i=1,4
     disp(i)=16*(i-1)+(i-1)
     blocklen(i)=i
  end do

  if (rank == 0) then
     write(*,*) 'Data in rank 0'
     do i=1,8
        write(*,'(8I3)') array(i, :)
     end do
  end if


  !TODO: create datatype describing one row, use mpi_type_vector

  call mpi_type_vector(8,1,8,mpi_integer, rowtype, ierr)
  call mpi_type_commit(rowtype, ierr)
  call mpi_type_indexed(4, blocklen,disp,mpi_integer,indx,ierr)
  call mpi_type_commit(indx, ierr)

  !TODO: send first row of matrix from rank 0 to 1
  if (rank==0)then
     call mpi_send(array(2,1),1,rowtype, 1,1,mpi_comm_world,ierr)
  else
     call mpi_recv(array(2,1),1,rowtype,0,1,mpi_comm_world,status,ierr)
  end if

  ! Print out the result
  if (rank == 1) then
     write(*,*) 'Received data'
     do i=1,8
        write(*,'(8I3)') array(i, :)
     end do
  end if

! initialize arrays
  if (rank == 0) then

     do i=1,8
        do j=1,8
           array(i,j) = i*10 + j
        end do
     end do
  else
     array(:,:) = 0
  end if


  if (rank==0)then
     call mpi_send(array,1,indx, 1,1,mpi_comm_world,ierr)
  else
     call mpi_recv(array,1,indx,0,1,mpi_comm_world,status,ierr)
  end if

  ! Print out the result
  if (rank == 1) then
     write(*,*) 'Received data'
     do i=1,8
        write(*,'(8I3)') array(i, :)
     end do
  end if


  !TODO free datatype
  call mpi_type_free(rowtype,ierr)
  call mpi_type_free(indx,ierr)

  call mpi_finalize(ierr)

end program datatype1
