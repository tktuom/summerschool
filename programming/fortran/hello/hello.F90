program hello
  implicit none
  real :: x,y,z
  write (*,*) 'Hello world from Fortran!'

  x=5
  y=7
  z=x*y
  write(*,*) z, sin(z)

end program hello
