program vectorsum
  use iso_fortran_env, only: int64
  implicit none
  integer, parameter :: ik = int64
  integer(kind=ik), parameter :: nx = 102400_ik

  integer(kind=ik), dimension(nx) :: vecA
  integer(kind=ik) :: sum, psum, sumex
  integer(kind=ik) :: i
  integer :: tid

  ! Initialization of vector
  do i = 1, nx
     vecA(i) = i
  end do

  sumex = nx*(nx+1_ik)/2_ik
  write(*,*) 'Arithmetic sum formula (exact):                  ', sumex

  sum = 0
  ! TODO: Parallelize the computation
!$omp parallel private(i,psum,tid)
  tid = omp_get_thread_num()
  psum = sum
  !$omp do schedule(static)
  do i = 1, nx
     psum = psum + vecA(i)
  end do
  !$omp end do
  sum = sum + psum
  write(*,*) 'I am thread', tid,', I got psum, sum:', psum, sum
!$omp end parallel
  write(*,*) 'Sum: ', sum
end program vectorsum
