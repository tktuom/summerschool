program datatype_struct
  use mpi_f08
  use iso_fortran_env, only : real64
  implicit none

  type particle
     real :: coords(3)
     integer :: charge
     character(len=2) :: label
  end type particle

  integer, parameter :: n = 1000
  integer :: i, ierror,  myid,  ntasks
  type(particle) :: particles(n)

  type(mpi_status) :: status
  integer, parameter :: cnt = 3
  type(mpi_datatype) :: particle_mpi_type, temp_type, types(cnt)
  integer :: blocklen(cnt)
  integer(KIND=MPI_ADDRESS_KIND) :: disp(cnt)
  integer(KIND=MPI_ADDRESS_KIND) :: lb, extent
  real(real64) :: t1, t2

  call MPI_INIT(ierror)
  call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierror)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, ntasks, ierror)

  ! insert some data for the particle struct
  if(myid == 0) then
     do i = 1, n
        call random_number(particles(i)%coords)
        particles(i)%charge = 54
        particles(i)%label = 'Xe'
     end do
  end if

  types = [mpi_real, mpi_integer, mpi_character]

  blocklen=[3,1,1]
  call mpi_get_address(particles(1)%coords, disp(1))
  call mpi_get_address(particles(1)%charge, disp(2))
  call mpi_get_address(particles(1)%label, disp(3))
  disp(3) = disp(3)-disp(1)
  disp(2) = disp(2)-disp(1)
  disp(1) = 0
  lb = disp(1)

  ! TODO: define the datatype for type particle
  call mpi_type_create_struct(3,blocklen,disp,types,particle_mpi_type,ierror)
  call mpi_type_commit(particle_mpi_type, ierror)


  ! TODO: Check extent.
  ! (Not really neccessary on most systems.)
  call mpi_type_get_extent(particle_mpi_type,lb,extent)


  write(*,*) 'Extent=   ', extent
  write(*,*) 'Displacement=    ', disp(3)-disp(1)+2

  ! TODO: resize the particle_mpi_type if needed
  if(extent /= disp(3) - disp(1) +2) then
     ! TODO: resize the particle_mpi_type if needed
     write(*,*) 'Resizing the extent'
     temp_type = particle_mpi_type
     call mpi_type_create_resized(temp_type,lb,extent,particle_mpi_type,ierror)
     call mpi_type_commit(particle_mpi_type, ierror)
  end if

  t1 = MPI_WTIME()
  if(myid == 0) then
     do i = 1, 1000
        call MPI_SEND(particles, n, particle_mpi_type, 1, i, &
             & MPI_COMM_WORLD, ierror)
     end do
  else if(myid == 1) then
     do i = 1, 1000
        call MPI_RECV(particles, n, particle_mpi_type, 0, i, &
             & MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
     end do
  end if
  t2 = MPI_WTIME()

  write(*,*) "Time: ", myid, (t2-t1) / 1000d0
  write(*,*) "Check:", myid, particles(n)%coords(1)

  if(myid==1) then
     particles(:)%coords(1) = 0
     particles(:)%coords(2) = 0
     particles(:)%coords(3) = 0
     particles(:)%charge = 0
     particles(:)%label = 'Co'
  end if

  t1 = mpi_wtime()
  if (myid == 0) then
     call mpi_send(particles, int(n*extent), mpi_byte, 1, 123, MPI_COMM_WORLD,ierror)
  else
     call mpi_recv(particles, int(n*extent), mpi_byte, 0, 123,MPI_COMM_WORLD,&
                  status, ierror)
  end if
  t2 = mpi_wtime()

  write(*,*) "Time: ", myid, (t2-t1) / 1000d0
  write(*,*) "Check:", myid, particles(n)%coords(1)

  call MPI_TYPE_free(particle_mpi_type, ierror)
  call MPI_FINALIZE(ierror)

end program datatype_struct
