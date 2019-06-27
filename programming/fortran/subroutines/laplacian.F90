module laplacian_mod
  implicit none
  real, parameter :: dx = 0.1, dy = 0.1

contains

  subroutine initialize(field0)
    ! TODO: implement a subroutine that initializes the input array
    implicit none
    integer :: i,j, xsize, ysize
    real :: x,y
    real, intent(out) :: field0(:,:)
    
    xsize=int(size(field0,1))
    ysize=size(field0,2)

    y = 0.0
    do j = 1, xsize
        x = 0.0
     	do i = 1, ysize
           field0(i, j) = x**2 + y**2
           x = x + dx
     	end do
     	y = y + dy
    end do
  end subroutine initialize

  subroutine laplacian(curr, prev)
    ! TODO: insert a subroutine that computes a laplacian of the
    ! array "prev" and returns it as an array "curr"
    implicit none
    integer :: i,j,a,b,c,d,nx,ny
    real, dimension(:,:),intent(out) :: curr
    real, dimension(:,:), intent(in) :: prev
    nx=size(prev,1)
    ny=size(prev,2)
    print *, nx, ny
    
    do i=1,nx
	 a=i-1
	 b=i+1
         if(a==0) then   !Turn the grid periodic
	   a=nx
 	 elseif (b==nx+1)then
    	   b=1
         end if
     	 do j=1,ny
	    c=j-1
	    d=j+1
            if(c==0) then    !Again, for periodicity
	       c=ny
	    elseif(d==ny+1)then
	       d=1
            end if
            curr(i,j)=((prev(a,j)-2*prev(i,j)+prev(b,j))/dx**2)+((prev(i,c)-2*prev(i,j)+prev(i,d))/dy**2)
	 end do
    end do
        
  end subroutine laplacian

  subroutine laplacian_edges(curr, prev)
    ! TODO: insert a subroutine that computes a laplacian of the
    ! array "prev" and returns it as an array "curr"
    implicit none
    integer :: i,j,a,b,c,d,nx,ny
    real, dimension(:,:),intent(out) :: curr
    real, dimension(:,:), intent(in) :: prev
    nx=size(prev,1)
    ny=size(prev,2)
    print *, nx, ny

    do i=1,nx
         a=i-1
         b=i+1
         
         do j=1,ny
            c=j-1
            d=j+1
            if(a==0 .or. c==0) then    !Correct edges
            	 cycle
            elseif(b==nx+1 .or. d==ny+1)then
                 cycle
            end if
            curr(i,j)=((prev(a,j)-2*prev(i,j)+prev(b,j))/dx**2)+((prev(i,c)-2*prev(i,j)+prev(i,d))/dy**2)
         end do
    end do

  end subroutine laplacian_edges

  subroutine write_field(array)
    ! TODO: write a subroutine that prints "array" on screen
    implicit none
    real, intent(in) :: array(:,:)
    integer :: i, nx
    
    nx=size(array,1)
    do i=1,nx
       write(*,'(12F6.2)') array(i,:)
    end do

  end subroutine write_field

end module laplacian_mod
