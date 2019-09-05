!projection on the simplex splace
! update project method on 2019-Aug-3: based on the paper
! Fast projection onto the simplex and the l1 ball
! Laurent Condat, 2016
    subroutine simplex_projection(x,z,d,n)
    implicit none
    integer,intent(in)::n
    real*8,parameter::project_eps = 0.0
    real*8,intent(in)::d
    integer,dimension(n)::isSelect
    real*8,dimension(n),intent(in)::z
    real*8,dimension(n),intent(out)::x
    real*8::pivot
    real*8:: sum_k,get_sum
    real*8,dimension(n)::tempz
    integer::i,now,pre,counter
    logical st,loop
    real*8 dnor
    integer,dimension(n)::set_i
    x=0.0
    pivot=(sum(z)-d)/n
    loop =  .true.
    isSelect = 1
    counter = 0
    do while(loop)
        pre = sum(isSelect)
        isSelect = 0
        tempz=0
        do i=1,n
            if(z(i)>pivot+project_eps) then 
                isSelect(i) = 1
                tempz(i) = z(i)
            end if
        end do
        pivot = (sum(tempz)-1+(n-sum(isSelect))*project_eps)/sum(isSelect)
        if (sum(isSelect).eq.pre) then 
            loop = .false.
        else
            loop = .true.
        end if
        counter=counter+1
        if (counter>100) then 
            write(*,*)  "warning on the projection method and can not get of loop"
            pause
        end if 
    end do 
    do i =1, n
        x(i) = max(z(i)-pivot,project_eps)
    end do
    return 

!     do i=1,n
!     !	x(i)=z(i)+(d-sum(z(:)))/(n*1.0)
!         x(i)=z(i)+(d-get_sum(z(1:n),n))/(n*1.0)
!     end do 
		
! 10	set_i=0
!     st=.true.
!     sum_k=0.0
!     do i=1,n
!         !if (x(i)>0.0) then
!         if (x(i) > project_eps) then
!             set_i(i) = 1
!             sum_k = sum_k + x(i)
!         endif 
!         !if (x(i)<0.0) then 
!         if (x(i) < project_eps) then 
!             st =.false.
!             sum_k = sum_k + project_eps
!         end if 
!     end do
!     if (st) then
!         return
!     end if 
!     dnor = (sum(set_i(:))*1d0)
!     do i = 1,n
!         if (set_i(i)==0) then 
!              !x(i)=0.0
!             x(i) = project_eps
!         else if (set_i(i) == 1) then 
!             x(i) = x(i) + (d-sum_k)/dnor
!         end if 
!     end do 
!     go to 10 

    end subroutine



		real*8 function get_sum(x,n)
		implicit none 
		integer n
		real*8 x(n)
		integer i
		get_sum=0.0
		do i=1,n
			get_sum=get_sum+x(i)
		end do 
		return 
		end 
