Program vectorsum
#ifdef _OPENACC
  use openacc
#endif
  implicit none
  integer, parameter :: rk = selected_real_kind(12)
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

! TODO
! Implement vector addition on device with OpenACC
! vecC = vecA + vecB

!$acc parallel loop copyout(vecC)
   do i=1,nx
      vecC(i) = vecA(i) + vecB(i)
   end do
!$acc end parallel loop
#ifdef _OPENACC
  write(*,*) 'Loop was done with OpenACC'
#else
  write(*,*) 'Loop was done serially on the CPU'
#endif

  ! Compute the check value
  write(*,*) 'Reduction sum: ', sum(vecC)

end program vectorsum
