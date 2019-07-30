		subroutine rsp(r,link_time,firstout,lastout,pa,backbnode,backtoforward)
		use constpara
		implicit none 
         
		integer i, n1, n2
	!   do loop index
		integer p, nd
		integer,intent(in)::r
		integer,intent(in)::firstout(nn),lastout(nn)
		integer,intent(inout)::pa(nn)
		integer,intent(in)::backbnode(nl)
		integer,intent(in)::backtoforward(nl)
        
	!   root, intermediate node
		integer arc
	!   link number
		integer q(nn)
	!   queue of nodes to be scanned
		integer endq
	!   mark the last element of the queue array
		real*8 d1, dp
	!   node distance
		integer ilarge
	!   a number larger than nn
		real*8 xlarge

	!   a large number
	!	real*8 dist(nn)    
	!	node lables
	!	integer pa(nn)
	!	predecessor arc	
	!	integer imax
!		integer destination
	!	integer np
		real*8,intent(in)::link_time(nl)
	!	call read_data
	!	r=1		! original node
	!	imax=30		! maximum number of iteration for double sweeping
	!	call  dswp(r,imax) 
	!	call trace(r,destination,ksp,np)
	!	xlarge=1e+9
		xlarge = large
		ilarge = nn+1
		pa = 0
      	
   !   	firstout = ffout(:,r)
  !    	lastout=flout(:,r)
	!     initialize labels

		  do i = 1, nn
			 dist(i) = xlarge
			 q(i) = 0
		  enddo

	!     set labels for root node    
		  dist(r) = 0.0
		  q(r) = ilarge 
		  endq = r
		  p = r
      
	!     start of main algorithm

	100   continue

		  n1 = firstout(p)
		  if (n1.eq.0) goto 201
		  n2 = lastout(p)
		  dp = dist(p)
		  do 10 arc = n1, n2
	!		 nd = bnode(arc)
			 nd = backbnode(arc)
	!		 d1 = dp + link_time(arc)
			 d1 = dp + link_time(backtoforward(arc))
			 if (d1.ge.dist(nd)) go to 10
	!
	!     change distance and label of node nd
	!
			 pa(nd) = arc
			 dist(nd) = d1
			 if (q(nd)) 30, 20, 10
			! if i>0,  10
			! if i=0,  20,30,10 
			! if i=-1 30,10 
	!     if nd has never been scanned insert it at the end of the queue
	!
	20       q(endq) = nd
			 endq = nd
			 q(nd) = ilarge
			 go to 10
	!
	!     if nd has already been scanned add it at the beginning of the
	!     queue after node p
	!
	30       q(nd) = q(p)
			 q(p) = nd
          
			 if (endq.eq.p) endq = nd
         
	10    continue
	!
	!     get next node from the top of the queue
	!
	201   n1 = q(p)

	!     flag p as having been scanned

		  q(p) = -1
		  p = n1


	!     if the queue is not empty go back to scan next node

		 if (p.lt.ilarge) go to 100
      
!		write(*,*) "finsih"
		
  end subroutine 
    
    
	subroutine sp(r,link_time,firstout,lastout,pa,bnode)
	use constpara
	implicit none 
         
	integer i, n1, n2
	!   do loop index
	integer p, nd
	integer,intent(in)::r
	integer,intent(in)::firstout(nn),lastout(nn)
	integer,intent(inout)::pa(nn)
	integer,intent(in)::bnode(nl)
        
!   root, intermediate node
	integer arc
!   link number
	integer q(nn)
!   queue of nodes to be scanned
	integer endq
!   mark the last element of the queue array
	real*8 d1, dp
!   node distance
	integer ilarge
!   a number larger than nn
	real*8 xlarge

	real*8,intent(in)::link_time(nl)
	!	call  dswp(r,imax) 
	!	call trace(r,destination,ksp,np)
	!	xlarge=1e+9
	xlarge = large
	ilarge = nn+1
	pa = 0
      	
   !   	firstout = ffout(:,r)
  !    	lastout=flout(:,r)
	!     initialize labels

	do i = 1, nn
		dist(i) = xlarge
		q(i) = 0
	enddo

!     set labels for root node    
	dist(r) = 0.0
	q(r) = ilarge 
	endq = r
	p = r
      
	!     start of main algorithm

	100   continue

		  n1 = firstout(p)
		  if (n1.eq.0) goto 201
		  n2 = lastout(p)
		  dp = dist(p)
		  do 10 arc = n1, n2
			 nd = bnode(arc)
			 d1 = dp + link_time(arc)
			 if (d1.ge.dist(nd)) go to 10
	!
	!     change distance and label of node nd
	!
			 pa(nd) = arc
			 dist(nd) = d1
			 if (q(nd)) 30, 20, 10
			! if i>0,  10
			! if i=0,  20,30,10 
			! if i=-1 30,10 
	!     if nd has never been scanned insert it at the end of the queue
	!
20     q(endq) = nd
			 endq = nd
			 q(nd) = ilarge
			 go to 10
	!
	!     if nd has already been scanned add it at the beginning of the
	!     queue after node p
	!
	30   q(nd) = q(p)
			 q(p) = nd
          
			 if (endq.eq.p) endq = nd
         
	10    continue
	!
	!     get next node from the top of the queue
	!
	201   n1 = q(p)

	!     flag p as having been scanned

		  q(p) = -1
		  p = n1


	!     if the queue is not empty go back to scan next node

		 if (p.lt.ilarge) go to 100
      
!		write(*,*) "finsih"
		
		end subroutine 
