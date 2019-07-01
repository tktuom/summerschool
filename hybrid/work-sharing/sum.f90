program vectorsum
  implicit none
  integer, parameter :: rk = kind(1d0)
  integer, parameter :: ik = selected_int_kind(9)
  integer, parameter :: nx = 102400

  real(kind=rk), dimension(nx) :: vecA, vecB, vecC
  real(kind=rk)    :: sum
  integer(kind=ik) :: i

  ! Initialization of vectors
  do i = 1, nx
     vecA(i) = 1.0_rk/(real(nx - i + 1, kind=rk))
     vecB(i) = vecA(i)**2
  end do

  ! TODO:
  !   Implement here the parallelized version of vector addition,
  !   vecC = vecA + vecB
!$omp parallel private(i)
  !$omp do schedule(dynamic)
  do i=1,nx
     vecC(i) = vecA(i) + vecB(i)
  end do
  !$omp end do
!$omp end parallel


  ! Compute the check value
  write(*,*) vecC(nx), '=', vecA(nx),'+',vecB(nx)
  write(*,*) 'Reduction sum: ', sum(vecC)

end program vectorsum
