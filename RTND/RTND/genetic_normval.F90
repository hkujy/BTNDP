!this function is to calculate the norm value
real*8 function norm_value(vector,n)
implicit none
integer,intent(in)::n
real*8,dimension(n)::vector
real*8:: results
integer::i
results=0.0
do i=1,n
results=results+vector(i)*vector(i)
end do 
norm_value=sqrt(results)

return

end function

real*8 function norm_value2(v1,v2,n1,n2)
implicit none
integer,intent(in)::n1,n2
real*8,dimension(n1,n2)::v1,v2
real*8:: results
integer::i,j
results=0.0
do i=1,n1
	do j=1,n2
	results=results+(v1(i,j)-v2(i,j))**2
	end do 
end do 
norm_value2=sqrt(results)

return

end function

real*8 function norm_value0(vector,n1,n2)
implicit none
integer,intent(in)::n1,n2
real*8,dimension(n1,n2)::vector
real*8:: results
integer::i,j
results=0.0
do i=1,n1
	do j=1,n2
	results=results+vector(i,j)**2
	end do 
end do 
norm_value0=sqrt(results)

return

end function



