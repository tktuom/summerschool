program hello
  use omp_lib
  use mpi_f08
  implicit none
  integer :: my_id, tid, rc
  integer :: provided, required=MPI_THREAD_FUNNELED

  ! TODO: Initialize MPI with thread support.
  call mpi_init_thread(required,provided,rc)
  ! TODO: Find out the MPI rank and thread ID of each thread and print
  !       out the results.
  call mpi_comm_rank(mpi_comm_world,my_id,rc)
!$omp parallel private(tid)
  tid = omp_get_thread_num()
  print *, 'Hello world, I am thread', tid, 'in rank', my_id

!$omp end parallel

  ! TODO: Investigate the provided thread support level.
  print* , provided
  call MPI_Finalize(rc)
end program hello
