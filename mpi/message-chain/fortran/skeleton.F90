program basic
  use mpi_f08
  use iso_fortran_env, only : REAL64

  implicit none
  integer, parameter :: msgsize = 10000000
  integer :: rc, myid, ntasks, source, tag, dest
  integer :: message(msgsize)
  integer :: receiveBuffer(msgsize)
  type(mpi_status) :: status(2)
  type(mpi_request) :: req(2)

  real(REAL64) :: t0, t1

  call mpi_init(rc)
  call mpi_comm_rank(MPI_COMM_WORLD, myid, rc)
  call mpi_comm_size(MPI_COMM_WORLD, ntasks, rc)

  message = myid

  write(*,*) 'Myid=', myid, 'Message=   ', message(1)

  ! Start measuring the time spent in communication
  call mpi_barrier(mpi_comm_world, rc)
  t0 = mpi_wtime()

  tag = myid+1  

  if (myid < ntasks-1) then
     dest = myid+1
  else
     dest = MPI_PROC_NULL
  end if

  if(myid == ntasks-1) then
     write(*,*) ntasks-1, dest
  end if

  if (myid == 0) then
     source = MPI_PROC_NULL
  else
     source = myid-1
  end if
  ! TODO: Send and receive as defined in the assignment
  !if (myid < ntasks-1) then
  call mpi_isend(message,msgsize,MPI_INTEGER,dest,tag,MPI_COMM_WORLD,req(1),rc)
  !   if (myid >0) then
  call mpi_irecv(receiveBuffer,msgsize,MPI_INTEGER,source,myid,MPI_COMM_WORLD,req(2),rc)
  !   end if

  write(*,'(A10,I3,A20,I8,A,I3,A,I3)') 'Sender: ', myid, &
          ' Sent elements: ', msgsize, &
          '. Tag: ', tag, '. Receiver: ', dest

  !else
  !call mpi_irecv(receiveBuffer,msgsize,MPI_INTEGER,source,myid,MPI_COMM_WORLD,status,rc)
  !end if

  call mpi_waitall(2,req, status,rc)

  if (myid > 0) then
    
     write(*,'(A10,I3,A,I3)') 'Receiver: ', myid, &
          ' First element: ', receiveBuffer(1)
  end if

  ! Finalize measuring the time and print it out
  t1 = mpi_wtime()
  call mpi_barrier(mpi_comm_world, rc)
  call flush(6)

  write(*, '(A20, I3, A, F6.3)') 'Time elapsed in rank', myid, ':', t1-t0

  call mpi_finalize(rc)

end program basic
