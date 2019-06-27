program gridmaker
  use heat_mod
  use comline_mod
  implicit none
  type(field) :: grid

  CALL read_command_line(grid%nx, grid%ny)
  !grid%nx=10
  !grid%ny=10
  grid%dx=0.1
  grid%dy=0.1

  CALL initialise(grid)

  CALL write_out(grid)

end program gridmaker
