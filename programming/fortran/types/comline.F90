module comline_mod

  contains
    subroutine read_command_line(x,y)
      implicit none
      integer, intent(out) :: x,y
      character(len=10) :: args(2)
      integer :: n_args, i

      n_args = command_argument_count()
      if (n_args /= 2) then
         write(*,*) 'Input size of the grid: ./exe nx ny'
	 stop
      end if

      do i=1,2
      	 CALL get_command_argument(i,args(i))
	 !Next line not absolutely necessary
	 args(i) = trim(adjustl(args(i)))
      end do
      
      read(args(1),*) x
      read(args(2),*) y
      write(*,*) 'Number of rows, columns:   ', x, y
    end subroutine read_command_line

end module comline_mod
