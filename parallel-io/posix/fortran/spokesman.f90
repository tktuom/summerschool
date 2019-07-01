program pario
  use mpi_f08
  use, intrinsic :: iso_fortran_env, only : error_unit, output_unit
  implicit none

  integer, parameter :: datasize = 64, writer_id = 0
  integer :: rc, my_id, ntasks, localsize, i
  integer, dimension(:), allocatable :: localvector
  integer, dimension(datasize) :: fullvector

  call mpi_init(rc)
  call mpi_comm_size(mpi_comm_world, ntasks, rc)
  call mpi_comm_rank(mpi_comm_world, my_id, rc)

  if (ntasks > 64) then
     write(error_unit, *) 'Maximum number of tasks is 64!'
     call mpi_abort(MPI_COMM_WORLD, -1, rc)
  end if

  if (mod(datasize, ntasks) /= 0) then
     write(error_unit,*) 'Datasize (64) should be divisible by number of tasks'
     call mpi_abort(MPI_COMM_WORLD, -1, rc)
  end if

  localsize = datasize / ntasks
  allocate(localvector(localsize))

  localvector = [(i + my_id * localsize, i=1,localsize)]

  call single_writer(fullvector,localvector)

  deallocate(localvector)
  call mpi_finalize(rc)

contains

  subroutine single_writer(fullvector,localvector)
    implicit none
    integer, dimension(:) ,intent(in) :: fullvector,localvector
!    integer :: locvc
    
!    locvc = size(localvector)
    ! TODO: Implement a function that writers the whole array of elements
    !       to a file so that single process is responsible for the file io
!    call mpi_gather(localvector,locvc,mpi_integer,fullvector,locvc,mpi_integer,writer_id,mpi_comm_world,rc)
!    if (my_id == writer_id) then
    open(my_id+11,file='dada'//char(my_id)//'.dat')  !,status='replace')
    write(my_id+11,*) localvector
    close(my_id+11)
!    end if

  end subroutine single_writer

end program pario
