!projection on the simplex splace
    subroutine simplex_projection(x,z,d,n)
    implicit none
    integer,intent(in)::n
    real*8,intent(in)::d
    real*8,dimension(n),intent(in)::z
    real*8,dimension(n),intent(out)::x
    real*8:: sum_k,get_sum
    integer::i
    logical st
    real*8 dnor
    integer,dimension(n)::set_i
    x=0.0
	
    do i=1,n
    !	x(i)=z(i)+(d-sum(z(:)))/(n*1.0)
        x(i)=z(i)+(d-get_sum(z(1:n),n))/(n*1.0)
    end do 
		
10	set_i=0
    st=.true.
    sum_k=0.0
    do i=1,n
        if (x(i)>0.0) then
            set_i(i)=1
            sum_k=sum_k+x(i)
        endif 
        if (x(i)<0.0) then 
            st=.false.
        end if 
    end do
    if (st) then
        return
    end if 
    dnor = (sum(set_i(:))*1d0)
    do i=1,n
        if (set_i(i)==0) then 
            x(i)=0.0
        else if (set_i(i)==1) then 
            x(i)=x(i)+(d-sum_k)/dnor
        end if 
    end do 
    go to 10 

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
