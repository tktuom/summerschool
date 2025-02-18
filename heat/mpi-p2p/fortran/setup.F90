! Setup routines for heat equation solver
module setup
  use heat

contains

  subroutine initialize(previous, current, nsteps, parallel)
    use utilities
    use io

    implicit none

    type(field), intent(out) :: previous, current
    integer, intent(out) :: nsteps
    type(parallel_data), intent(out) :: parallel

    integer :: rows, cols
    logical :: using_input_file
    character(len=85) :: input_file, arg  ! Input file name and command line arguments


    ! Default values for grid size and time steps
    rows = 2000
    cols = 2000
    nsteps = 500
    using_input_file = .false.

    ! Read in the command line arguments and
    ! set up the needed variables
    select case(command_argument_count())
    case(0) ! No arguments -> default values
    case(1) ! One argument -> input file name
       using_input_file = .true.
       call get_command_argument(1, input_file)
    case(2) ! Two arguments -> input file name and number of steps
       using_input_file = .true.
       call get_command_argument(1, input_file)
       call get_command_argument(2, arg)
       read(arg, *) nsteps
    case(3) ! Three arguments -> rows, cols and nsteps
       call get_command_argument(1, arg)
       read(arg, *) rows
       call get_command_argument(2, arg)
       read(arg, *) cols
       call get_command_argument(3, arg)
       read(arg, *) nsteps
    case default
       call usage()
       stop
    end select

    ! Initialize the fields according the command line arguments
    if (using_input_file) then
       call read_field(previous, input_file, parallel)
       call copy_fields(previous, current)
    else
       call parallel_setup(parallel, rows, cols)
       call set_field_dimensions(previous, rows, cols, parallel)
       call set_field_dimensions(current, rows, cols, parallel)
       call generate_field(previous, parallel)
       call copy_fields(previous, current)
    end if

  end subroutine initialize

  ! Generate initial the temperature field.  Pattern is disc with a radius
  ! of nx_full / 6 in the center of the grid.
  ! Boundary conditions are (different) constant temperatures outside the grid
  subroutine generate_field(field0, parallel)
    use heat

    implicit none

    type(field), intent(inout) :: field0
    type(parallel_data), intent(in) :: parallel

    real(dp) :: radius2
    integer :: i, j, ds2

    ! The arrays for field contain also a halo region
    allocate(field0%data(0:field0%nx+1, 0:field0%ny+1))

    ! Square of the disk radius
    radius2 = (field0%nx_full / 6.0_dp)**2

    do j = 0, field0%ny + 1
       do i = 0, field0%nx + 1
          ds2 = int((i - field0%nx_full / 2.0_dp + 1)**2 + &
               & (j + parallel%rank * field0%ny - field0%ny_full / 2.0_dp + 1)**2)
          if (ds2 < radius2) then
             field0%data(i,j) = 5.0_dp
          else
             field0%data(i,j) = 65.0_dp
          end if
       end do
    end do

    ! Boundary conditions
    if (parallel % rank == 0) then
       field0%data(:,0) = 20.0_dp
    else if (parallel % rank == parallel%size - 1) then
       field0%data(:,field0%ny+1) = 70.0_dp
    end if
    field0%data(0,:) = 4.0_dp
    field0%data(field0%nx+1,:) = 5.0_dp

  end subroutine generate_field


  ! Clean up routine for field type
  ! Arguments:
  !   field0 (type(field)): field variable to be cleared
  subroutine finalize(field0, field1)
    use heat

    implicit none

    type(field), intent(inout) :: field0, field1

    deallocate(field0%data)
    deallocate(field1%data)

  end subroutine finalize

  ! Helper routine that prints out a simple usage if
  ! user gives more than three arguments
  subroutine usage()
    implicit none
    character(len=256) :: buf

    call get_command_argument(0, buf)
    write (*,'(A)') 'Usage:'
    write (*,'(A, " (default values will be used)")') trim(buf)
    write (*,'(A, " <filename>")') trim(buf)
    write (*,'(A, " <filename> <nsteps>")') trim(buf)
    write (*,'(A, " <rows> <cols> <nsteps>")') trim(buf)
  end subroutine usage

end module setup
