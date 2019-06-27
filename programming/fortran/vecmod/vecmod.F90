module vector_algebra
  use iso_fortran_env, only : REAL64
  implicit none
  type vector_t
     real(REAL64) :: x, y, z
  end type vector_t

  ! TODO: overload operators needed by the parser

  interface operator(+)
     module procedure vector_sum
  end interface operator(+)

  interface operator(-)
     module procedure vector_sub
  end interface operator(-)

  interface abs
     module procedure vector_abs
  end interface abs

  interface operator(*)
     module procedure vector_multpl
  end interface operator(*)

  interface operator(.x.)
     module procedure vector_cross
  end interface operator(.x.)

  ! ...

contains
  ! TODO: implement the corresponding functions

  function vector_sum(v1, v2) result(v3)
    !    ...
    implicit none
    type(vector_t), intent(in) :: v1,v2
    type(vector_t) :: v3

    v3 % x = v1 % x + v2 % x
    v3 % y = v1 % y + v2 % y
    v3 % z = v1 % z + v2 % z

  end function vector_sum

  function vector_sub(v1, v2) result(v3)
    
    implicit none
    type(vector_t), intent(in) :: v1,v2
    type(vector_t) :: v3

    v3 % x = v1 % x - v2 % x
    v3 % y = v1 % y - v2 % y
    v3 % z = v1 % z - v2 % z
  end function vector_sub

  function vector_multpl(v1,v2) result(v3)

    implicit none
    type(vector_t), intent(in) :: v1,v2
    real :: v3

    v3 = (v1%x * v2%x) + (v1%y * v2%y) + (v1%z * v2%z)
  end function vector_multpl

  function vector_abs(v) result(foo)
    implicit none
    type(vector_t), intent(in) :: v
    real :: foo

    foo = sqrt( v%x**2 + v%y**2 + v%z**2)

  end function vector_abs

  function vector_cross(v1,v2) result(v3)
    implicit none
    type(vector_t), intent(in) :: v1,v2
    type(vector_t) :: v3

    v3 % x = v1 % y * v2 % z - v1%z * v2%y
    v3 % y = v1 % x * v2 % z - v1%z * v2%x
    v3 % z = v1 % x * v2 % y - v1%y * v2%x

  end function vector_cross

end module vector_algebra
