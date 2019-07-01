program hello
  use mpi_f08
  implicit none
  integer :: rc, rank, size
  
  call mpi_init(rc)
  call mpi_comm_rank(MPI_COMM_WORLD, rank, rc)
  call mpi_comm_size(MPI_COMM_WORLD, size, rc)
  if (rank == 0) then
    write(*,*) 'Wohoo, the rank is=   ',rank, '  out of n. processes=  ', size
  else
    write(*,*) 'Wohoo, the rank is=   ',rank
  end if

  call mpi_finalize(rc)

end program hello
