 	! the purpose of this subroutin is to check whether all the nodes has been selected once in one chrosome.
	! input variables 
	!				1. chorosome 
	!				2, bee_number  !bee number/ or solution number	
	!
	!out put variables
	!				1, chorosome. certainly that every node is covered at least once.


	module subs  ! check and update chorosome

	implicit none
	include 'def.f90'
	contains


	subroutine check_chromsome(chromsome,BN,node_type,stop_number)

	integer BN
	integer i,j,node,miss,replace,position,insert,flag,route
	integer chromsome(initial_bees,chromsome_length)
	integer node_type(num_nodes)
	integer,allocatable :: stop_num(:),node_count(:)
	integer stop_number(initial_bees,max_routes),start,over
	real*8 ran


	allocate(node_count(num_tsw_nodes),stop_num(max_routes))
	node_count=0
	stop_num=0
	
	do i=1,chromsome_length
		node=chromsome(BN,i)
		if (node/=0) then 
			if (node_type(node)/=2) then 
				node_count(node)=node_count(node)+1
			end if
		end if 
	end do 

	do i=1,max_routes
		start=(i-1)*route_length+1
		over=i*route_length
		do j=start,over
			if (chromsome(BN,j)/=0) then
				stop_num(i)=stop_num(i)+1
			end if 
		end do 
		stop_num(i)=stop_num(i)-2      !minus start and end nodes
	end do 

	flag=1                    ! if flag=1 then add the nodes from stop
	do i=1,max_routes
		if (stop_num(i)<max_stops) then 
		flag=0						! there exit routes not fully used
		end if 
	end do 

if (flag==1) then 

	write(8,*) "subroutine check_chromsome,flag==1,replace node"	
	do i=1,num_tsw_nodes
		if (node_count(i)==0) then
			miss=i
			call random_number(ran)
			node=int(ran*(num_tsw_nodes)+1)

			do while(node_count(node)<=2.or.(node_type(node)==1))
				call random_number(ran)
				node=int(ran*(num_tsw_nodes)+1)
			end do 

			replace=node
			call random_number(ran)
			position=int(ran*(node_count(replace))+1)
		
			insert=0
			do j=1,chromsome_length
					node=chromsome(BN,j)
					if (node==replace) then
						insert=insert+1
						if (insert==position) then 
						chromsome(BN,j)=miss
						node_count(replace)=node_count(replace)-1
						node_count(miss)=node_count(miss)+1
					end if 
				end if 
			end do 
		end if 
	end do 

else if (flag==0) then 
	do i=1,num_tsw_nodes
		if (node_count(i)==0) then
			miss=i 
			call random_number(ran)
			route=int(ran*max_routes+1)
			do while (stop_num(route)==max_stops)
				call random_number(ran)
				route=int(ran*max_routes+1)
			end do 
			stop_num(route)=stop_num(route)+1
			position=(route-1)*route_length+stop_num(route)+1
			chromsome(BN,position+1)=chromsome(BN,position)
			chromsome(BN,position)=miss
			node_count(miss)=node_count(miss)+1
		end if 
	end do 

end if 		
	stop_number(BN,:)=stop_num(:)




	deallocate(node_count,stop_num)

	end subroutine

!****************************************************************************************
! the following subroutine is to calculate the route cost
! input variable :
!				1: route chorome,only the ten demension varable.
!				2: link cost(:)
!				3: stop number
!output
!			1,link cost 				
!            poblem  missing the TLT interchange

	subroutine get_route_cost(route_gen,link_cost,sum,N,tlt_e_cost,route_D_cost)  ! this is to calcuate the cost with in TSW 
	implicit none

	integer,intent(in)::route_gen(route_length)
	real*8 link_cost(num_nodes,num_nodes)  
	real*8 sum
	real*8  tlt_e_cost(num_destination_nodes)
	integer D_index(num_destination_nodes)
	integer i,j,N,D_node
	real*8 route_D_cost

	D_index(:)=(/24,25,26,27,28/)

	sum=0
	
	do i=1,N+1
		sum=sum+link_cost(route_gen(i),route_gen(i+1))      ! from the last node to interchange TLT	
	end do 

	sum=sum+stop_time*N                           ! include stop time 
	
	route_D_cost=sum						! the cost of a route to destination
	
	D_node=route_gen(2+N)			! destination node
	

	do i=1,num_destination_nodes						
		if (D_node==D_index(i)) then				
		sum=sum-tlt_e_cost(i)					! total cost- cost from tlt to destination 
		exit
		end if 
	end do 


!	if (sum.lt.zero) then 
!	write(8,*) "route_cost less than 0"
!	end if 

	end subroutine

!*******************************************************
! find the optimal sequence
! the purpose is to use decent method to find the optimal intermide sequence to reduce route_cost
!input variables
!				1. a certain route_gen
!				2. number of route_stops
!				3. route cost
!out put :
!				1. a optimal route_gen
!				2. a new route_cost

	subroutine optimal_sequence(route_gen,route_cost,N,link_cost,route_D_cost,BN,route)
	implicit none 
	
	integer::N  ! number of stops
    integer,intent(inout)::route_gen(route_length)
	real*8,intent(in)::link_cost(num_nodes,num_nodes)  
	real*8,intent(inout)::route_cost
	real*8 temp_cost,temp_route_D_cost
	real*8 route_D_cost(initial_bees,max_routes)
	integer i,j,temp,BN,route

	do i=2,N
		do j=N+1,i+1,-1
			call swap(route_gen(i),route_gen(j))	! echange first
			call get_route_cost(route_gen,link_cost,temp_cost,N,tlt_e_cost,temp_route_D_cost)
			if (temp_cost<route_cost) then         ! if time reduce
				route_cost=temp_cost       
				route_D_cost(BN,route)=temp_route_D_cost
			else 
			call swap(route_gen(i),route_gen(j))   ! else change back
			end if 
		end do 
	end do 

	end subroutine

!  this subroutine is repair operator purpose is to find the route which is larger than the max travel time  
!  input   : route_gen
!			: route_cost ---------used to compare with the max travel time
!           : stop number    ------------ used to check every stop which give the max reduction in trip time
!			: link cost
!  out put : chromosome link cost.	

	recursive subroutine  repair_operator(chromsome,route_cost,link_cost,node_count,stop_number,BN,route,node_type,tlt_e_cost,route_D_cost)
	implicit none		

	integer::stop_number(initial_bees,max_routes)
	INTEGER::chromsome(initial_bees,chromsome_length)				! number of stops
    integer::route_gen(route_length)
	integer temp_gen(route_length),final_gen(route_length)
	real*8,intent(in)::	link_cost(num_nodes,num_nodes)  
	integer remove_node,temp_node,remove_OK
	integer i,j,flag,node,candy_count,route,BN,start,over,r
	real*8  reduce_cost,temp_reduce,final_cost,final_D_cost
	real*8  route_cost(initial_bees,max_routes)
	integer,intent(inout)::node_count(initial_bees,num_tsw_nodes)
	integer check,insert_node,insert_flag,remove_insert_ok
	integer candy_list(max_stops)
	integer temp_stop_number(initial_bees,max_routes)
	integer node_type(num_nodes)
	real*8,allocatable ::temp_cost(:,:),temp_route_D_cost(:,:)
	real*8 route_D_cost(initial_bees,max_routes)
	real*8 tlt_e_cost(num_destination_nodes)



	allocate(temp_cost(initial_bees,max_routes),temp_route_D_cost(initial_bees,max_routes))
	
	if (route_cost(BN,route).le.max_tsw_cost) then 
		return
	end if 

	temp_route_D_cost(BN,:)=route_D_cost(BN,:)
	temp_cost(BN,:)=route_cost(BN,:)
	temp_stop_number(BN,:)=stop_number(BN,:)

	remove_ok=0

	start=(route-1)*route_length+1
	over=route*route_length
	route_gen(:)=chromsome(BN,start:over)
	

	if  (stop_number(BN,route)==1) then 
		call swap_repair(chromsome,BN,route,stop_number,node_count,route_cost,node_type,link_cost,route_D_cost,tlt_e_cost)
		return
	end if 
	
	flag=0
	candy_list=0
	candy_count=0
	do i=2,1+stop_number(BN,route)
		node=route_gen(i)
		if (node_count(BN,node)>=2) then 
			flag=1                          ! find a node appear 2 twice
			candy_count=candy_count+1
			candy_list(i-1)=1
		end if 
	end do 
	
	if (candy_count==0) then 
		call insert_repair(chromsome,BN,route,stop_number,node_count,route_cost,node_type,link_cost,route_D_cost)
		deallocate(temp_cost,temp_route_D_cost)		
		return 
	end if
	
if (flag==0) then
	write(8,*) "repair flag bug =", flag

else if (flag==1) then 
	temp_cost=0.0
	reduce_cost=0
	do i=1,max_stops
		temp_gen=0	
		if (candy_list(i)/=0) then 
			temp_node=i+1               ! temp remove node
			do j=1,temp_node-1
				temp_gen(j)=route_gen(j)
			end do 
			do j=temp_node,route_length-1
			temp_gen(j)=route_gen(j+1)
			end do 
			temp_stop_number(BN,route)=stop_number(BN,route)-1		
			call get_route_cost(temp_gen,link_cost,temp_cost(BN,route),temp_stop_number(BN,route),tlt_e_cost,temp_route_D_cost(BN,route))
			call optimal_sequence(temp_gen,temp_cost(BN,route),temp_stop_number(BN,route),link_cost,temp_route_D_cost,BN,route)	
				temp_reduce=route_cost(BN,route)-temp_cost(BN,route)
			if (temp_reduce>reduce_cost) then
				reduce_cost=temp_reduce
				remove_node=route_gen(temp_node)
				final_gen(:)=temp_gen(:)
				final_cost=temp_cost(BN,route)
				final_D_cost=temp_route_D_cost(BN,route)
				remove_ok=1
				end if 
		end if 
	end do 
end if 
! return and out put the final gen and final cost		

!	if (final_cost==0) then
!		write(*,*) "repair failed,final_cost==0"
!		stop
!	end if 
	
	if (remove_ok==1) then 
		node_count(BN,remove_node)=node_count(BN,remove_node)-1
		route_gen(:)=final_gen(:)
		route_cost(BN,route)=final_cost
		route_D_cost(BN,route)=final_D_cost
		stop_number(BN,route)=stop_number(BN,route)-1
		chromsome(BN,start:over)=route_gen(:)
	
		if (route_cost(BN,route)>max_tsw_cost) then 

			CALL repair_operator(chromsome,route_cost,link_cost,node_count,stop_number,BN,route,node_type,tlt_e_cost,route_D_cost)
	
		END IF 
	end if 


	deallocate(temp_cost,temp_route_D_cost)
	end subroutine

! this subroutine is to generate buses and  get frequency
! the input varaiable is 
!					1.the chromosome
!					2. total demand  _ check the minal buses
!					3. d_node
!					4.node_tpye
!the output is 
!					1. bus allocation
!					2.frequency
! genera procedure
!				step 1. check the minimum requirement for each destinaion 
!				step 2.  allcate the minimum stops to each destiatinon
!				step 3 . allocate the residual buses.


	subroutine bus_allocation(bus_string,BN,min_fre_buses,route_D,min_D_buses)
	implicit none

	integer BN
	integer i,j,r,D,node,start,over,residual_buses
	integer bus_string(max_routes)
	integer min_fre_buses(initial_bees,max_routes)
	real*8  ran
	integer sum1(num_destination_nodes),D_residual
	integer count,line

	integer	min_D_buses(num_destination_nodes)          
	integer route_group(num_destination_nodes,el_max)	
	integer route_D(initial_bees,max_routes)

	

	bus_string(:)=min_fre_buses(BN,:)

	route_group(:,:)=0	
	
	do i=1,num_destination_nodes		
		count=1
		do j=1,max_routes
			if (route_D(BN,j)==i) then 
				route_group(i,count)=j
				count=count+1
			end if 
		end do 
	end do  
	
	sum1(:)=0
	do i=1,num_destination_nodes
		do j=1,el_max
			if (route_group(i,j)==0) exit
			line=route_group(i,j)
			sum1(i)=sum1(i)+bus_string(route_group(i,j))			! sum1 is total number of buses for each destination
		end do 
	end do 

	do i=1,num_destination_nodes
		if (sum1(i).lt.min_D_buses(i)) then 
			D_residual=min_D_buses(i)-sum1(i)
			do j=1,D_residual
					call random_number(ran)
					line=int(ran*el_max+1)			! line=1,2,3
					do while (route_group(i,line)==0) 
						call random_number(ran)
						line=int(ran*el_max+1)
					end do 
					bus_string(route_group(i,line))=bus_string(route_group(i,line))+1		! assign buses
			end do 
		end if 
	end do 


	residual_buses=max_buses-sum(bus_string(:))
	
	do i=1,residual_buses
		call random_number(ran)
		r=int(ran*max_routes+1)
		bus_string(r)=bus_string(r)+1
	end do 
	
					
	end subroutine


! subroutine  get index
! this subroutine is to get index the certain route.
! input variable  
!					1. chromsome
!
!
!out put:		1.NR_ie
!				2.RT_ik
!				3.RT_ie_n
!these variables will be used in the assignment part and get the objective value part

	subroutine get_index(chromsome,BN,NR_ie,RT_ikn,RT_ien,route_D,node_type,RT_ke)
	implicit none


	integer chromsome(initial_bees,chromsome_length),BN
	integer NR_ie(num_tsw_nodes,num_destination_nodes),RT_ikn(num_tsw_nodes,max_routes),RT_ien(num_tsw_nodes,num_destination_nodes,max_routes)
	integer route_D(initial_bees,max_routes) 
	integer RT_ke(num_destination_nodes,max_routes)
	integer i,j, r,node,start,over,D
	integer node_type(num_nodes) 

	NR_ie=1
	RT_ikn=0
	RT_ien=0
	RT_ke=0
	do r=1,max_routes
		start=(r-1)*route_length+1
		over=r*route_length
		RT_ke(route_D(BN,r),r)=1
		do j=start,over
				node=chromsome(BN,j)
				if (node/=0) then 
					D=route_D(BN,r)
					if (node_type(node)/=2) then 
						NR_ie(node,D)=0					! direcet service to e 
						RT_ikn(node,r)=1				! direcet serive to k
						RT_ien(node,D,r)=1				! node through route r to destination D
					end if
				end if  
		end do 
	end do 				

	
	end subroutine 



	subroutine get_frequency(bus_string,frequency,BN,route_D_cost)
	implicit none		

	integer bus_string(max_routes)
	real*8 frequency(max_routes)
	integer i,BN
	real*8 route_time(max_routes)
	real*8 route_D_cost(initial_bees,max_routes)

	route_time=route_D_cost(BN,:)

	frequency=0.0

	do i=1,max_routes
		frequency(i)=0.5*bus_string(i)/(route_time(i)/60.0)			 ! 60 is convert miniute to hour
	end do 
	

	end subroutine

! this subroutine is to get T_ien,which is the time from each node to the destination or to the middle transfer point	
! intput variable
!					1,chromsome : check the sequence
!					2.route_D : check the destination of each route and each node
!					3.stop_number	
!	
!output result  :
!				1.T_ikn : node to transfer through n
!				2,T_ien : node to certain destination




	subroutine get_T_ien(chromsome,BN,link_cost,route_D,stop_number,T_ien,T_ikn,tlt_e_cost)
	implicit none

	real*8  T_ien(num_tsw_nodes,num_destination_nodes,max_routes),T_ikn(num_tsw_nodes,max_routes)
	integer chromsome(initial_bees,chromsome_length)
	integer route_D(initial_bees,max_routes) 
	integer stop_number(initial_bees,max_routes)  
	integer BN								! BN is number of initial_bee, N,is the number of stops
	real*8 link_cost(num_nodes,num_nodes),tlt_e_cost(num_destination_nodes)
	integer i,k,r,start,over,D,j,node,next_node
	real*8  sum


	T_ien=0
	T_ikn=0
	do r=1,max_routes
		start=(r-1)*route_length+1
		over=start+stop_number(BN,r)			! over is the end of tsw node
		D=route_D(BN,r)
		do j=start,over
			do k=j,over
				if (k/=over) then 
				node=chromsome(BN,k)
				next_node=chromsome(BN,k+1)

				T_ien(chromsome(BN,j),D,r)=T_ien(chromsome(BN,j),D,r)+link_cost(node,next_node)	
				T_ikn(chromsome(BN,j),r)=T_ikn(chromsome(BN,j),r)+link_cost(node,next_node)	
			
				T_ien(chromsome(BN,j),D,r)=T_ien(chromsome(BN,j),D,r)+stop_time
				T_ikn(chromsome(BN,j),r)=T_ikn(chromsome(BN,j),r)+stop_time

				end if 
				if (k==over) then       ! if k = the last node , not need to add stop time
					node=chromsome(BN,k)
					next_node=chromsome(BN,k+1)
					T_ien(chromsome(BN,j),D,r)=T_ien(chromsome(BN,j),D,r)+link_cost(node,next_node)	
					T_ikn(chromsome(BN,j),r)=T_ikn(chromsome(BN,j),r)+link_cost(node,next_node)-tlt_e_cost(D)	! total cost minus the cost TO d 						
					if (link_cost(node,next_node)-tlt_e_cost(D).lt.zero) then 
						write(*,*) node,next_node,link_cost(node,next_node)
						WRITE(*,*) "link_cost(node,next_node)-tlt_e_cost(D).lt.zero",stop_number(BN,r),D
						write(*,*) chromsome(BN,start:over+2),stop_number(BN,r)
			!		write(*,*) node,next_node,link_cost(node,next_node),tlt_e_cost(D)
						pause
					end if 
				end if 	
			end do
		end do 	  
 	end do 

! the fellowing output is to check whether every node has t_ikn	


	end subroutine


! the fellowing subroutine is to get T_ie, the time for each node to get to the destination
! HOW:   based on the formula
! input:				1: RT_ien,RT_ikn,NR_ie
!						2: frequency
!						3:T_ien,T_ikn
!output:			T_ie for each OD pairs.
	
	subroutine get_T_ie(T_ie,T_ien,T_ikn,frequency,tlt_e_cost,NR_ie,RT_ien,RT_ikn,RT_ke)
	implicit none

	real*8  T_ien(num_tsw_nodes,num_destination_nodes,max_routes),T_ikn(num_tsw_nodes,max_routes)
	real*8 frequency(max_routes)
	real*8  T_ie(num_tsw_nodes,num_destination_nodes)
	real*8 tlt_e_cost(num_destination_nodes)
	real*8 sum1,sum2,sum3,sum4
	integer NR_ie(num_tsw_nodes,num_destination_nodes),RT_ikn(num_tsw_nodes,max_routes),RT_ien(num_tsw_nodes,num_destination_nodes,max_routes)
	integer RT_ke(num_destination_nodes,max_routes)
	integer i,e,n

	do i=1,num_tsw_nodes
		do e=1,num_destination_nodes
			if (NR_ie(i,e)==0) then			! direct service
				sum1=0.0					! numerator				
				sum2=0.0					! denominator	
				do n=1,max_routes
					sum1=sum1+RT_ien(i,e,n)*frequency(n)*T_ien(i,e,n)
					sum2=sum2+RT_ien(i,e,n)*frequency(n)
				end do 
				if (sum2==0) then 
				write(*,*) "sum2==0",frequency(:)
				pause
				end if 

				T_ie(i,e)=(sum1+1.0)/(NR_ie(i,e)+sum2)
			else if (NR_ie(i,e)==1) then 
				sum1=0.0
				sum2=0.0
				sum3=0.0
				sum4=0.0
				do n=1,max_routes
					sum1=sum1+RT_ikn(i,n)*frequency(n)*T_ikn(i,n)
					sum2=sum2+RT_ikn(i,n)*frequency(n)
					sum3=sum3+RT_ke(e,n)*frequency(n)*tlt_e_cost(e)
					sum4=sum4+RT_ke(e,n)*frequency(n)
				end do 
			!	if (sum2==0) then 
				!	write(*,*) "NR_ie(i,e)==1,sum2==0"
			!		write(*,*) RT_ikn(i,:)
				!	pause
			!	end if 
			!	if (sum4==0) then 
				!	write(*,*) "NR_ie(i,e)==1,sum4==0"
			!		pause
			!	end if 
			!	T_ie(i,e)=(sum1+1)/DMAX1(sum2,ZERO)+(sum3+1)/DMAX1(sum4,ZERO)
				T_ie(i,e)=(sum1+1)/sum2+(sum3+1)/sum4
			end if 
		end do 
	end do 
	

	end subroutine 

! this subroutine is to get objective function value 
	subroutine get_object(objective,BN,demand,NR_ie,T_ie,frequency,bus_string,RT_ikn,RT_ien,RT_ke,alpha,beta,assignment_penalty)
	implicit none

	real*8 demand(num_tsw_nodes,num_destination_nodes) 
	real*8  T_ie(num_tsw_nodes,num_destination_nodes)
	integer NR_ie(num_tsw_nodes,num_destination_nodes)
	real*8 objective(initial_bees)
	real*8 assignment_penalty(initial_bees)
	real*8 frequency(max_routes)
	integer bus_string(initial_bees,max_routes),RT_ke(num_destination_nodes,max_routes)
	integer RT_ikn(num_tsw_nodes,max_routes),RT_ien(num_tsw_nodes,num_destination_nodes,max_routes)
	integer i,e,BN
	real*8 sum1,sum2
	real*8 alpha,beta

	sum1=0.0
!	sum2=0.0

	do i=1,num_tsw_nodes
		do e=1,num_destination_nodes
		sum1=sum1+demand(i,e)*NR_ie(i,e)
	!	sum2=sum2+demand(i,e)*T_ie(i,e)				!sum of travel time 
		end do 
	end do 

!	objective(BN)=B1*sum1+B2*sum2
	objective(BN)=B1*sum1

!	if (objective(BN).le.zero) then 
!	write(8,*) " get negetive objecitive"
!	end if 
	
	call flow_assignment(assignment_penalty,frequency,demand,NR_ie,bus_string,BN,RT_ikn,RT_ien,RT_ke,alpha,beta)

	objective(BN)=objective(BN)+alpha*assignment_penalty(BN)
	
	

	end subroutine



! optimal frequency finding. 
! this procedure is to find optimal frequency
! input		1: the lower bound for each destination connection
!			2: bus_string
!			3. route and node index
! ouput		1: final objective function 
!
!
!procedure  : 1: decent direction search 
!			    a. move one bus from i to j
!				b. get new frequency
!				c. get new objective 
!				d. compare objective
!				e. save the objective and frequency setting
		


		subroutine optimal_frequency(bus_string,BN,frequency,objective,demand,route_cost,T_ien,T_ikn,T_ie, tlt_e_cost,&
									 NR_ie,RT_ikn,RT_ien,RT_ke,min_fre_buses,alpha,beta,assignment_penalty,route_D_cost,route_D,min_D_buses)
		implicit none			

		integer BN
		integer bus_string(initial_bees,max_routes)
		real*8 frequency(max_routes)
		real*8 demand(num_tsw_nodes,num_destination_nodes),objective(initial_bees),route_cost(initial_bees,max_routes)
		real*8  T_ien(num_tsw_nodes,num_destination_nodes,max_routes),T_ikn(num_tsw_nodes,max_routes),T_ie(num_tsw_nodes,num_destination_nodes)
		real*8 tlt_e_cost(num_destination_nodes)
		integer NR_ie(num_tsw_nodes,num_destination_nodes),RT_ikn(num_tsw_nodes,max_routes),RT_ien(num_tsw_nodes,num_destination_nodes,max_routes)
		integer RT_ke(num_destination_nodes,max_routes)
		integer i,j,m,n,boundi,boundj,k
		integer temp_bus_string(initial_bees,max_routes)
		integer	min_fre_buses(initial_bees,max_routes)
		integer residual,sum
		integer route_D(initial_bees,max_routes),count,group1,group2
		real*8 alpha,beta
		real*8 route_D_cost(initial_bees,max_routes)
		real*8 assignment_penalty(initial_bees)

		integer	min_D_buses(num_destination_nodes)           
		integer route_group(num_destination_nodes,el_max)

		real*8,allocatable::temp_obj(:)
		real*8,allocatable::temp_fre(:)
		real*8,allocatable::temp_tie(:,:)
		allocate(temp_tie(num_tsw_nodes,num_destination_nodes))
		allocate(temp_obj(initial_bees))
		allocate(temp_fre(max_routes))


		route_group(:,:)=0
	
		do i=1,num_destination_nodes		
			count=1
			do j=1,max_routes
				if (route_D(BN,j)==i) then 
					route_group(i,count)=j
					count=count+1
				end if 
			end do 
		end do  
		
		temp_obj(:)=objective(:)
		temp_bus_string(BN,:)=bus_string(BN,:)
		do i=1,max_routes
			group1=route_D(BN,i)			
			do j=i+1,max_routes	
				group2=route_D(BN,j)
				
				if (group1==group2) then		! if theya are in the same group than OK.
					m=0		
					boundi=bus_string(BN,i)-min_fre_buses(BN,i)   ! the difference between min frequnency requirment
				end if
				
				if (group1.ne.group2) then 
					m=0
					residual=0
					sum=0			! total number of routes for bus 
					do k=1,el_max
						if (route_group(group1,k)==0) exit
							sum=sum+bus_string(BN,route_group(group1,k))
	 
					end do 
					residual=sum-bus_string(BN,i)
					residual=imax0((min_D_buses(group1)-residual),0)
					boundi=Imin0((bus_string(BN,i)-residual),(bus_string(BN,i)-min_fre_buses(BN,i)))  ! also cannot violate min frequency constraint
				end if 

				do while (m<boundi)
						m=m+1
					temp_bus_string(BN,i)=bus_string(BN,i)-1
					temp_bus_string(BN,j)=bus_string(BN,j)+1
					call get_frequency(temp_bus_string(BN,:),frequency,BN,route_D_cost)
					call get_T_ie(T_ie,T_ien,T_ikn,frequency,tlt_e_cost,NR_ie,RT_ien,RT_ikn,RT_ke)	
					call get_object(temp_obj,BN,demand,NR_ie,T_ie,frequency,temp_bus_string,RT_ikn,RT_ien,RT_ke,alpha,beta,assignment_penalty)
								
					if (assignment_penalty(BN).le.0.01) then 
					write(14,*) "1"
						end if 


						if (temp_obj(BN).gt.objective(BN)) then 
						
							if (group1==group2) then 
								n=0
								boundj=bus_string(BN,j)-min_fre_buses(BN,j)
							end if 
							
							if (group1.ne.group2) then 
								n=0
								residual=0
								sum=0		
									do k=1,el_max
										if (route_group(group2,k)==0) exit
											sum=sum+bus_string(BN,route_group(group2,k))
									end do 
								residual=sum-bus_string(BN,j)			! buses provided by other lines
								residual=imax0((min_D_buses(group2)-residual),0)   ! 
								boundj=Imin0((bus_string(BN,j)-residual),(bus_string(BN,j)-min_fre_buses(BN,j)))

							end if 

							do while (n<boundj)
									n=n+1
								temp_bus_string(BN,i)=bus_string(BN,i)+1
								temp_bus_string(BN,j)=bus_string(BN,j)-1
								call get_frequency(temp_bus_string(BN,:),frequency,BN,route_D_cost)
								call get_T_ie(T_ie,T_ien,T_ikn,frequency,tlt_e_cost,NR_ie,RT_ien,RT_ikn,RT_ke)	
								call get_object(temp_obj,BN,demand,NR_ie,T_ie,frequency,temp_bus_string,RT_ikn,RT_ien,RT_ke,alpha,beta,assignment_penalty)
								
								if (assignment_penalty(BN).le.0.01) then 
									write(14,*) "1"
								end if 	
									
								
								
									if (temp_obj(BN).gt.objective(BN)) then 
										exit
									else if (temp_obj(BN).lt.objective(BN)) then 
											objective(BN)=temp_obj(BN)
										bus_string(BN,i)=temp_bus_string(BN,i)
										bus_string(BN,j)=temp_bus_string(BN,j)
									end if 
							end do 	
							exit
						else if (temp_obj(BN).le.objective(BN)) then 
						!	write(*,*) "move i-1 to j+1",bus_string(BN,i),temp_bus_string(BN,i),bus_string(BN,j),temp_bus_string(BN,j)
							objective(BN)=temp_obj(BN)
							bus_string(BN,i)=temp_bus_string(BN,i)
							bus_string(BN,j)=temp_bus_string(BN,j)
						end if
				end do 
			
			!	temp_bus_string(BN,i)=bus_string(BN,i)
			!	temp_bus_string(BN,j)=bus_string(BN,j)
			
		!	if (bus_string(BN,i).le.zero) then 
			!	write(8,*) "bus_string.lt.zero",bus_string(BN,i)
			!end if 
			end do 
			end do 
			
			do i=1,max_routes
				if (bus_string(BN,i)==0) then 
				write(*,*) "bus==0,after optimal frequency"
				end if 
			end do 


			call get_frequency(bus_string(BN,:),frequency,BN,route_D_cost)
		
			do i=1,max_routes
				if (frequency(i)==0) then 
				write(*,*) "frequency=0 after optimal_sequency"
				stop 
				end if 
			end do 

			call get_T_ie(T_ie,T_ien,T_ikn,frequency,tlt_e_cost,NR_ie,RT_ien,RT_ikn,RT_ke)		
			call get_object(objective,BN,demand,NR_ie,T_ie,frequency,bus_string,RT_ikn,RT_ien,RT_ke,alpha,beta,assignment_penalty)	
	

		deallocate(temp_tie,temp_obj,temp_fre)


		end subroutine


! this subroutine is to do the assinment part and return the penalty for each objective function
!assumption:    all the lines passing one node is an attraction line
! input :		1, frequency 
!				2 index: NR_ie
!				3.demand
!				4.bus_string  _use to caculate the capacity of a certain line
!				5 use allocatable flow variables
! out put :     penalty value relate to line capacity 
!

		subroutine flow_assignment(assignment_penalty,frequency,demand,NR_ie,bus_string,BN,RT_ikn,RT_ien,RT_ke,alpha,beta)
		implicit none
		
		real*8 frequency(max_routes),demand(num_tsw_nodes,num_destination_nodes)
		integer NR_ie(num_tsw_nodes,num_destination_nodes),bus_string(initial_bees,max_routes),RT_ikn(num_tsw_nodes,max_routes)
		integer RT_ien(num_tsw_nodes,num_destination_nodes,max_routes),RT_ke(num_destination_nodes,max_routes)
		real*8,allocatable::v_ik_e(:,:,:),v_i_ke(:,:,:)
		integer i,e,p,r,BN
		real*8 sum_ik,sum_ie,sum_ke,sum1,sum2
		real*8 penalty_sum,tsw,tlt_e
		real*8 assignment_penalty(initial_bees)
		real*8 alpha,beta,capacity

		allocate(v_ik_e(num_tsw_nodes,num_destination_nodes,max_routes),v_i_ke(num_tsw_nodes,num_destination_nodes,max_routes))
		v_ik_e=0.0
		v_i_ke=0.0
		
	!	write(14,*)  "BN",Bn
		
		do i=1,num_tsw_nodes
			do e=1,num_destination_nodes
					sum_ie=0.0
					sum_ik=0.0
					sum_ke=0.0
					do r=1,max_routes
						sum_ie=sum_ie+frequency(r)*RT_ien(i,e,r)
						sum_ik=sum_ik+frequency(r)*RT_ikn(i,r)
						sum_ke=sum_ke+frequency(r)*RT_ke(e,r)
					end do 
		!  in case the fellowing sum might be zero
			

				do p=1,max_routes
					if (NR_ie(i,e)==1) then 
					!	v_ik_e(i,e,p)=demand(i,e)*NR_ie(i,e)*frequency(p)*RT_ikn(i,p)/dmax1(sum_ik,zero)
					!	v_i_ke(i,e,p)=demand(i,e)*NR_ie(i,e)*frequency(p)*RT_ke(e,p)/dmax1(sum_ke,zero)
						v_ik_e(i,e,p)=demand(i,e)*NR_ie(i,e)*frequency(p)*RT_ikn(i,p)/sum_ik
						v_i_ke(i,e,p)=demand(i,e)*NR_ie(i,e)*frequency(p)*RT_ke(e,p)/sum_ke
					else if (NR_ie(i,e)==0) then 
					!	v_ik_e(i,e,p)=demand(i,e)*frequency(p)*RT_ien(i,e,p)/dmax1(sum_ie,zero)
						v_ik_e(i,e,p)=demand(i,e)*frequency(p)*RT_ien(i,e,p)/sum_ie
						v_i_ke(i,e,p)=v_ik_e(i,e,p)
					!	if (v_i_ke(i,e,p)/=v_ik_e(i,e,p)) then 
					!	stop
					!	end if 
					end if 
				end do 
			end do 
		end do 

		assignment_penalty(BN)=0.0					
	
		do r=1,max_routes
			sum1=0.0
			sum2=0.0
			do i=1,num_tsw_nodes
				do e=1,num_destination_nodes		   
				   sum1=sum1+v_ik_e(i,e,r)                     ! sum flow within Tsw area
				   sum2=sum2+v_i_ke(i,e,r)						! sum flow from TLT to destination
				end do 
			end do 	
			 
			! penalty is the sum include tsw and tlt
			! use minimal funciont is because only the insufficient capacity is considered
			capacity=bus_string(BN,r)*num_seats
			tsw=dmax1((sum1-capacity),zero)
			tlt_e=dmax1((sum2-capacity),zero)
	
			assignment_penalty(BN)=assignment_penalty(BN)+tsw+tlt_e


	!		write(14,'(2(f6.2,1x))') tsw,tlt_e 
!
	
		end do 


	
		deallocate(v_ik_e,v_i_ke)
		end subroutine

!*******************************************************************************
!this subroutine is  to find the neighbourhodd solution
! input:     1: a certain chromsome 
!	         2: route_d
! output:     the new objective value and its corresponding bus string and many vairbles.
! variables changing :
!						1:stop_number(i,r)
!						2:node_count(nodes)
!						3 index : RT_ien(i,e,r),RT_ikn,NR_ie
!						4.route_cost
! step :    0 creat a indicator and random select case
!			1: insert mutation : random insert a stop node
!				a: random find a route
!				b: random find a insert stop position.
!			2:remove 
!			3:swap
!			4:tranfer
!	after the mutation    
!			1.repair operator
!			2.get route_cost
!			3.sequency optimal.
!			3.5. get_index
!			4.get_frequency
!			5.frequency-optimal
!			6.get objective value.
!			7.replace current solution and corresponding variables values.

		subroutine get_neighbour(objective,BN,chromsome,node_type,stop_number,NR_ie,RT_ikn,RT_ien,RT_ke,route_cost,link_cost,frequency,&
									tlt_e_cost,route_D,bus_string,update,node_count,alpha,beta,assignment_penalty,route_D_cost,min_D_buses,node_prob)
		implicit none 

		integer chromsome(initial_bees,chromsome_length),stop_number(initial_bees,max_routes) 
		integer node_type(num_nodes),route_D(initial_bees,max_routes)     
		integer NR_ie(num_tsw_nodes,num_destination_nodes),RT_ikn(num_tsw_nodes,max_routes),RT_ien(num_tsw_nodes,num_destination_nodes,max_routes)		
		integer RT_ke(num_destination_nodes,max_routes),route_gen(route_length),bus_string(initial_bees,max_routes)
		real*8 objective(initial_bees),route_cost(initial_bees,max_routes)
		real*8 link_cost(num_nodes,num_nodes),frequency(max_routes),tlt_e_cost(num_destination_nodes)
		real*8,allocatable::T_ie(:,:),T_ien(:,:,:),T_ikn(:,:),temp_route_D_cost(:,:)
		integer,allocatable::temp_NR_ie(:,:),temp_RT_ikn(:,:),temp_RT_ien(:,:,:),temp_stop_number(:,:),check_node(:),temp_chromsome(:,:),temp_route_D(:,:)
		integer node_count(initial_bees,num_tsw_nodes)
		real*8,allocatable::temp_route_cost(:,:),temp_frequency(:),temp_obj(:),temp_assignment_penalty(:)
		integer	min_D_buses(num_destination_nodes)
		real*8 ran,alpha,beta
		real*8 node_prob(num_tsw_nodes,num_destination_nodes)
		real*8 route_D_cost(initial_bees,max_routes)
		real*8 assignment_penalty(initial_bees)
		integer index,i,j,e,r,BN,position,start,over,node,D,update,r2,start2,over2,position2,typ
		integer r1_type_count(3),r2_type_count(3),r1_candy(3,route_length),r2_candy(3,route_length),count,r1_pool(route_length)
	!	integer check_stops(initial_bees,max_routes),flag
		integer candy_route(max_routes),candy_position(route_length),flag
		integer min_fre_buses(initial_bees,max_routes),cycle_count
		INTEGER check_node_count(num_tsw_nodes)
		integer,allocatable:: r1_to_r2(:),temp_node_count(:,:),temp_bus_string(:,:)
		allocate(temp_NR_ie(num_tsw_nodes,num_destination_nodes),temp_RT_ikn(num_tsw_nodes,max_routes),temp_RT_ien(num_tsw_nodes,num_destination_nodes,max_routes))
		allocate(temp_stop_number(initial_bees,max_routes),check_node(num_nodes),temp_chromsome(initial_bees,chromsome_length))
		allocate(temp_route_cost(initial_bees,max_routes),temp_frequency(max_routes),temp_obj(initial_bees),temp_route_D(initial_bees,max_routes))
		allocate(T_ie(num_tsw_nodes,num_destination_nodes),T_ien(num_tsw_nodes,num_destination_nodes,max_routes),T_ikn(num_tsw_nodes,max_routes))
		allocate(temp_node_count(initial_bees,num_tsw_nodes),temp_assignment_penalty(initial_bees),temp_route_D_cost(initial_bees,max_routes))
		allocate(temp_bus_string(initial_bees,max_routes))

		temp_route_cost(BN,:)=route_cost(BN,:)
		temp_node_count(BN,:)=node_count(BN,:)
		temp_route_D_cost(BN,:)=route_D_cost(BN,:)
		temp_bus_string(BN,:)=bus_string(BN,:)

		temp_stop_number(BN,:)=stop_number(BN,:)
		temp_chromsome(BN,:)=chromsome(BN,:)
			        ! 0, if the node not in a route. 1, if the node in the route
		temp_frequency=frequency
		temp_obj(BN)=objective(BN)
		temp_route_D(BN,:)=route_D(BN,:)

		call random_number(ran)
5		index=int(ran*4+1)

!		test			
	!	index=1
	!	write(*,*) "index=",index
		if (index==2) then 
			flag=0
			do r=1,max_routes
				start=(r-1)*route_length+1
				over=r*route_length
				if (stop_number(BN,r).gt.2) then 
					do i=start+1,over
						if (chromsome(BN,i)/=0) then 
							if (node_type(chromsome(BN,i))/=2) then 
								if (node_count(BN,chromsome(BN,i)).gt.2) then 
								flag=flag+1          !number of nodes can remove
								exit
								end if 
							end if 
						end if 
					end do 
				end if 
				if (flag.gt.0) exit
			end do 
			if (flag.le.0) then 
			write(*,*) "regenerate index=1"
			index=1					! if connot remove than insert a node
			end if 
		end if 

 !		write(*,*) "index=",index

		select case (index)
		
		case (1)	! insert mutation 
	
		index=0
		do r=1,max_routes
			if (stop_number(BN,r).lt.max_routes) then 
			index=1
			exit
			end if 
		end do 
		if (index==0) then 
		write(*,*) "can not insert a node to a route"
		goto 5
	!	stop 
		end if 
		
		
		call random_number(ran)
			r=int(ran*max_routes+1)
		do while (temp_stop_number(BN,r)==max_stops)
			call random_number(ran)
			r=int(ran*max_routes+1)
		end do 


		D=route_D(BN,r)
		temp_stop_number(BN,r)=stop_number(BN,r)+1			!insert a node

		start=(r-1)*route_length+1
		over=r*route_length

		check_node=0    
			
		do i=start,over
			node=chromsome(BN,i)
			if (node/=0) then 
				if (node_type(node)/=2) then 
					check_node(node)=1
				end if 
			end if 
		end do 

		call random_number(ran)
		position=int(ran*(stop_number(BN,r))+1)     
		position=start+position                   ! if p=3,then insert between 3/4, position=1+3=4
		
		call random_number(ran)
	!	node=int(ran*num_tsw_nodes+1)		! generate a node from 1-23
		
		call ran_select_node(node,ran,node_prob,D)
		
		do while(check_node(node)==1)
			call random_number(ran)
	!		node=int(ran*num_tsw_nodes+1)
			call ran_select_node(node,ran,node_prob,D)
		end do 
		
		temp_chromsome(BN,position)=node
		
		do i=position+1,over
			temp_chromsome(BN,i)=chromsome(BN,i-1)
		end do 
	
		temp_node_count(BN,node)=temp_node_count(BN,node)+1
	
		route_gen(:)=temp_chromsome(BN,start:over)
 	
		call get_route_cost(route_gen,link_cost,temp_route_cost(BN,r),temp_stop_number(BN,r),tlt_e_cost,temp_route_D_cost(BN,r))
				
		call optimal_sequence(route_gen,temp_route_cost(BN,r),temp_stop_number(BN,r),link_cost,temp_route_D_cost,BN,r)

		temp_chromsome(BN,start:over)=route_gen(:)	

		if (temp_route_cost(BN,r)>max_tsw_cost) then 

			call repair_operator(temp_chromsome,temp_route_cost,link_cost,temp_node_count,temp_stop_number,BN,r,node_type,tlt_e_cost,temp_route_D_cost)

		end if 
	
	case (2)		!remove operator

		candy_route=0
		index=0
		do r=1,max_routes
			if (stop_number(BN,r).gt.2) then
				start=(r-1)*route_length+1
				over=r*route_length
				do j=start+1,over
					if (chromsome(BN,j)/=0) then 
						if (node_type(chromsome(BN,j))/=2) then 
							if (node_count(BN,chromsome(BN,j)).ge.2) then 
							    candy_route(r)=1
								index=1
								exit
							end if 
						end if 
					end if 
				end do 
			end if 
		end do 
!		write(*,*) candy_route(:)
		if (index==0) then 
		write(*,*) " no route can remove a node"
		goto 5
	!	stop
		end if 
		
		call random_number(ran)
		r=int(ran*max_routes+1) 
		
		do while (candy_route(r)==0) 
			call random_number(ran)
			r=int(ran*max_routes+1) 
		end do 
	
		candy_position=0		
	
		start=(r-1)*route_length+1
		over=r*route_length
		
		do i=start+1,over
			if (chromsome(BN,i)/=0) then 
				if (node_type(chromsome(BN,i))/=2) then 
					if (node_count(BN,chromsome(BN,i)).ge.2) then 
						candy_position(i-start+1)=1
					end if 
				end if 
			end if 
		end do 
		
		call random_number(ran)
		position=int(ran*route_length+1)
		
		if (position.gt.route_length) then 
		write(*,*) " remove generate larger position number"
		end if 
		
		do while (candy_position(position)==0) 
			call random_number(ran)
			position=int(ran*route_length+1)
		end do 
			
			position=position+start-1

	
		temp_node_count(BN,chromsome(BN,position))=temp_node_count(BN,chromsome(BN,position))-1
		temp_stop_number(BN,r)=temp_stop_number(BN,r)-1
	
		do i=position,over-1
			temp_chromsome(BN,i)=chromsome(BN,i+1)
		end do 
			temp_chromsome(BN,over)=0
	
		ROUTE_GEN=0
		route_gen(:)=temp_chromsome(BN,start:over)
 		call get_route_cost(route_gen,link_cost,temp_route_cost(BN,r),temp_stop_number(BN,r),tlt_e_cost,temp_route_D_cost(BN,r))		
		call optimal_sequence(route_gen,temp_route_cost(BN,r),temp_stop_number(BN,r),link_cost,temp_route_D_cost,BN,r)
		
		temp_chromsome(BN,start:over)=route_gen(:)
		
		if (temp_route_cost(BN,r)>max_tsw_cost) then 
			call repair_operator(temp_chromsome,temp_route_cost,link_cost,temp_node_count,temp_stop_number,BN,r,node_type,tlt_e_cost,temp_route_D_cost)
		!		temp_chromsome(BN,start:over)=route_gen(:)
		!		WRITE(*,*)"AFTER",	temp_chromsome(BN,start:over)
		end if 
	

	case (3)			! swap mutation
		
		cycle_count=0
10		cycle_count=cycle_count+1
	!	write(*,*) "cycle_count=",cycle_count
		if  (cycle_count.gt.50) then 
			write(*,*) "swap reach max cycle"
		goto 5
	!	stop
		end if 
		r1_candy=0
		r2_candy=0
		r1_type_count=0
		r2_type_count=0
			
		call random_number(ran)
			r=int(ran*max_routes+1)
		call random_number(ran)
			r2=int(ran*max_routes+1)
		do while (r==r2)
			call random_number(ran)
			r2=int(ran*max_routes+1)
		end do
				
		start=(r-1)*route_length+1
		over=r*route_length
		start2=(r2-1)*route_length+1
		over2=r2*route_length
			
		do i=start+1,over
			if (chromsome(BN,i)/=0) then 
				index=0
				do j=start2,over2
					if (chromsome(BN,j)/=0) then 
						if (chromsome(BN,i)==chromsome(BN,j)) then 
						index=1				! if there exits same node then can not swap
						exit
						end if 
					end if 
				end do
			if (index==0) then 
				node=chromsome(BN,i)
				typ=node_type(node)
				r1_type_count(typ)=r1_type_count(typ)+1
				r1_candy(typ,r1_type_count(typ))=node
			end if 
		end if 
		end do 
	
		do i=start2+1,over2
			if (chromsome(BN,i)/=0) then 
				index=0
				do j=start,over	
					if (Chromsome(BN,j)/=0) then 
					if (chromsome(BN,i)==chromsome(BN,j)) then 
							index=1
							exit
					end if 
					end if 
				end do
			if (index==0) then 
				node=chromsome(BN,i)
				typ=node_type(node)
				r2_type_count(typ)=r2_type_count(typ)+1
				r2_candy(typ,r2_type_count(typ))=node
			end if 
		end if
		end do 
	
		index=0
		do i=1,3  ! three different type
			if (r1_type_count(i)/=0) then 
				if (r2_type_count(i)/=0) then 
					index=1						! exit the same type that can swap
					exit
				end if 
			end if 
		end do 
		
		if (index==0) then 
			go to 10
		end if 
		
! generate a type
		call random_number(ran)
		typ=int(ran*3+1)       ! type 1-3
		
		do while (r1_type_count(typ)==0.or.r2_type_count(typ)==0) 
			call random_number(ran)
			typ=int(ran*3+1) 
		end do 
	!	write(*,*) "ok after type generate"
! generate a node

	!	write(*,*) "stop",stop_number(BN,r),stop_number(BN,r2)
	
		call random_number(ran)
		position=int(ran*r1_type_count(typ)+1)
		
		call random_number(ran)
		position2=int(ran*r2_type_count(typ)+1)
		
	!	write(*,*) "position 1,2",position,position2	
	!	write(*,*) "r1_type cont,r2_type_count",r1_type_count(typ),r2_type_count(typ)
		
		do i=start+1,over
			node=r1_candy(typ,position)
			if (chromsome(BN,i)==node) then 
				position=i
			exit
			end if 
		end do 

		do i=start2+1,over2
			node=r2_candy(typ,position2)
			if (chromsome(BN,i)==node) then
				position2=i
			exit
			end if 
		end do 
		
	!	WRITE(*,*) "ok before swap"
						
		call swap(temp_chromsome(BN,position),temp_chromsome(BN,position2))

		if (typ==2) then 
			call swap(temp_route_D(BN,r),temp_route_D(BN,r2))
		end if 
		
!  next step is to check the route cost and sequence of the two swaped routes		


		route_gen(:)=temp_chromsome(BN,start:over)
 	
		call get_route_cost(route_gen,link_cost,temp_route_cost(BN,r),temp_stop_number(BN,r),tlt_e_cost,temp_route_D_cost(BN,r))
	
		call optimal_sequence(route_gen,temp_route_cost(BN,r),temp_stop_number(BN,r),link_cost,temp_route_D_cost,BN,r)
		temp_chromsome(BN,start:over)=route_gen(:)
	
		if (temp_route_cost(BN,r)>max_tsw_cost) then 		
			call repair_operator(temp_chromsome,temp_route_cost,link_cost,temp_node_count,temp_stop_number,BN,r,node_type,tlt_e_cost,temp_route_D_cost)
		end if 	
	
		route_gen(:)=temp_chromsome(BN,start2:over2)
		call get_route_cost(route_gen,link_cost,temp_route_cost(BN,r2),temp_stop_number(BN,r2),tlt_e_cost,temp_route_D_cost(BN,r2))		
		call optimal_sequence(route_gen,temp_route_cost(BN,r2),temp_stop_number(BN,r2),link_cost,temp_route_D_cost,BN,r2)
		temp_chromsome(BN,start2:over2)=route_gen(:)

		if (temp_route_cost(BN,r2)>max_tsw_cost) then 
			call repair_operator(temp_chromsome,temp_route_cost,link_cost,temp_node_count,temp_stop_number,BN,r2,node_type,tlt_e_cost,temp_route_D_cost)
	!		temp_chromsome(BN,start2:over2)=route_gen(:)
		end if 	
		
	!	write(6,*) typ
	!	do r=1,max_routes
	!		start=(r-1)*route_length+1
	!		over=r*route_length
	!		write(6,'(11(i2,1x))') temp_chromsome(BN,start:over),route_d(BN,r)
	!	end do 	


	case (4)  ! transfer insert		

		cycle_count=0
40		cycle_count=cycle_count+1
		
	!	write(*,*) "cycle_count=",cycle_count
		if  (cycle_count.gt.10) then 
			write(*,*) "transfer reach max cycle"
			goto 5
	!	stop
		end if 
		

		
		index=0
		do i=1,max_routes
			if (stop_number(BN,i).ge.2) then 
			index=1
			exit
			end if 
		end do 

		if (index==0) then 
			write(*,*) "all stop number le 2"
			do r=1,max_routes
				start=(r-1)*route_length+1
				over=r*route_length
				write(*,'(11(I2,1x))') chromsome(BN,start:over),stop_number(BN,r)
			end do 
			stop 
		end if 

		call random_number(ran)
			r=int(ran*max_routes+1)
		do while (stop_number(BN,r)<2) 
			call random_number(ran)
			r=int(ran*max_routes+1)
		end do 

	!	write(*,*) "r1 ok"
		
		index=0
		do r2=1,max_routes
			if (r2/=r) then 
			if (stop_number(BN,r2).lt.max_stops) then 
			index=1
			exit
			end if 
			end if 
		end do 
		
		if (index==0) then 
		
		write(*,*) " can not find a r2"
		
		go to 40
		end if 	

		call random_number(ran)
			r2=int(ran*max_routes+1)
		do while ((r==r2).or.(stop_number(BN,r2)==max_stops))
			call random_number(ran)
			r2=int(ran*max_routes+1)
		end do	
	
	!	write(*,*) "r2 ok"

		start=(r-1)*route_length+1			
		over=r*route_length

				
		start2=(r2-1)*route_length+1
		over2=r2*route_length

	
		allocate(r1_to_r2(route_length))  ! only consider the intermindia stops
			r1_to_r2=chromsome(BN,start:over)            ! r1_to_r2(1) is the terminal node
			r1_to_r2(1)=0		! start node can not insert
		
		do i=start+1,over
			if (chromsome(BN,i)==0) then
			exit
			else if (chromsome(BN,i)/=0) then 
			if (node_type(chromsome(BN,i))/=2) then 
					do j=start2,over2
						if (chromsome(BN,i)==chromsome(BN,j)) then 
							r1_to_r2(i-start+1)=0
						end if 
					end do 
				end if 
			end if
		end do 
	
		r1_pool=0
		count=0
		index=0
		do i=1,route_length
			if (r1_to_r2(i)/=0) then 
				if(node_type(r1_to_r2(i))/=2) then 
					index=1
					count=count+1
					r1_pool(count)=r1_to_r2(i)
				end if 
			end if 
		end do 
	
		if (index==0) then 
			deallocate(r1_to_r2)
			go to  40
		end if

		call random_number(ran)
			position=int(ran*count+1)  ! does not include the first terminal node
			node=r1_pool(position)				

		do i=start+1,over
			if (chromsome(BN,i)==node) then 
				position=i
			end if 
		end do 


	
		do i=position,over-1
			temp_chromsome(BN,i)=chromsome(BN,i+1)
		end do
			temp_chromsome(BN,over)=0

		call random_number(ran)
		position2=int(ran*(stop_number(BN,r2)+1))      
		position2=start2+position2                  

			temp_chromsome(BN,position2)=node
		
		do i=position2+1,over2
			temp_chromsome(BN,i)=chromsome(BN,i-1)
		end do 


		temp_stop_number(BN,r)=stop_number(BN,r)-1
		temp_stop_number(BN,r2)=stop_number(BN,r2)+1



	
		start=(r-1)*route_length+1
		over=r*route_length
		
		route_gen(:)=temp_chromsome(BN,start:over)
 	!	write(*,*) " before repair r1 ",temp_chromsome(BN,start:over),temp_stop_number(BN,r)
		call get_route_cost(route_gen,link_cost,temp_route_cost(BN,r),temp_stop_number(BN,r),tlt_e_cost,temp_route_D_cost(BN,r))		
		call optimal_sequence(route_gen,temp_route_cost(BN,r),temp_stop_number(BN,r),link_cost,temp_route_D_cost,BN,r)
			temp_chromsome(BN,start:over)=route_gen(:)	
		
		if (temp_route_cost(BN,r)>max_tsw_cost) then 
	
			call repair_operator(temp_chromsome,temp_route_cost,link_cost,temp_node_count,temp_stop_number,BN,r,node_type,tlt_e_cost,temp_route_D_cost)

		end if 	

		start=(r2-1)*route_length+1
		over=r2*route_length
		route_gen(:)=temp_chromsome(BN,start:over)

		call get_route_cost(route_gen,link_cost,temp_route_cost(BN,r2),temp_stop_number(BN,r2),tlt_e_cost,temp_route_D_cost(BN,r2))		
		call optimal_sequence(route_gen,temp_route_cost(BN,r2),temp_stop_number(BN,r2),link_cost,temp_route_D_cost,BN,r2)
	
		temp_chromsome(BN,start:over)=route_gen(:)
	
		if (temp_route_cost(BN,r2)>max_tsw_cost) then 
	
			call repair_operator(temp_chromsome,temp_route_cost,link_cost,temp_node_count,temp_stop_number,BN,r2,node_type,tlt_e_cost,temp_route_D_cost)

		end if 	
	
		deallocate(r1_to_r2)



	end select



! the fellowing part put at the end of all the four mutation operatorsl.
		
		call min_frequency_bus(temp_route_D_cost,BN,min_fre_buses)


		call bus_allocation(temp_bus_string(BN,:),BN,min_fre_buses,temp_route_D,min_D_buses)		
		
	
						
		call get_frequency(temp_bus_string(BN,:),temp_frequency,BN,temp_route_D_cost)

		
		call get_index(temp_chromsome,BN,temp_NR_ie,temp_RT_ikn,temp_RT_ien,temp_route_D,node_type,RT_ke)

				
		call get_T_ien(temp_chromsome,BN,link_cost,temp_route_D,temp_stop_number,T_ien,T_ikn,tlt_e_cost)

		
		call get_T_ie(T_ie,T_ien,T_ikn,temp_frequency,tlt_e_cost,temp_NR_ie,temp_RT_ien,temp_RT_ikn,RT_ke)

	

		call get_object(temp_obj,BN,demand,temp_NR_ie,T_ie,temp_frequency,bus_string,temp_RT_ikn,temp_RT_ien,RT_ke,alpha,beta,temp_assignment_penalty)
			
		call  optimal_frequency(temp_bus_string,BN,temp_frequency,temp_obj,demand,temp_route_cost,T_ien,T_ikn,T_ie, tlt_e_cost,&
									 temp_NR_ie,temp_RT_ikn,temp_RT_ien,RT_ke,min_fre_buses,alpha,beta,temp_assignment_penalty,temp_route_D_cost,temp_route_D,min_D_buses)
		

	
	! the solution will update if the new neighour is feasible or better
		update=0
	if (temp_assignment_penalty(BN).le.0.0001) then 
		if (assignment_penalty(BN).le.0.0001) then    ! current is feasible
			if (temp_obj(BN)<objective(BN)) then		! if both feasible then choose the better one
				update=1
				objective(BN)=temp_obj(BN)
				chromsome(BN,:)=temp_chromsome(BN,:)
				node_count(BN,:)=temp_node_count(BN,:)
				stop_number(BN,:)=temp_stop_number(BN,:)
				route_d(BN,:)=temp_route_d(BN,:)
				assignment_penalty(BN)=temp_assignment_penalty(BN)
				route_D_cost(BN,:)=temp_route_D_cost(BN,:)
				bus_string(BN,:)=temp_bus_string(BN,:)
			end if 
		else if (assignment_penalty(BN).gt.0.0001 ) then		! current is not feasible
				update=1
				objective(BN)=temp_obj(BN)
				chromsome(BN,:)=temp_chromsome(BN,:)
				node_count(BN,:)=temp_node_count(BN,:)
				stop_number(BN,:)=temp_stop_number(BN,:)
				route_d(BN,:)=temp_route_d(BN,:)
				assignment_penalty(BN)=temp_assignment_penalty(BN)
				route_D_cost(BN,:)=temp_route_D_cost(BN,:)
				bus_string(BN,:)=temp_bus_string(BN,:)
		end if 
	else if (temp_assignment_penalty(BN).gt.0.0001) then 
			if (temp_obj(BN)<objective(BN)) then 
				update=1
				objective(BN)=temp_obj(BN)
				chromsome(BN,:)=temp_chromsome(BN,:)
				node_count(BN,:)=temp_node_count(BN,:)
				stop_number(BN,:)=temp_stop_number(BN,:)
				route_d(BN,:)=temp_route_d(BN,:)
				assignment_penalty(BN)=temp_assignment_penalty(BN)
				route_D_cost(BN,:)=temp_route_D_cost(BN,:)
				bus_string(BN,:)=temp_bus_string(BN,:)
			end if 
	end if 

		deallocate(temp_NR_ie,temp_RT_ikn,temp_RT_ien)
		deallocate(temp_stop_number,check_node,temp_chromsome)
		deallocate(temp_route_cost,temp_frequency,temp_obj,temp_route_D,temp_bus_string)
		deallocate(T_ie,T_ien,T_ikn,temp_node_count,temp_assignment_penalty,temp_route_D_cost)
	



	end subroutine 


!************************************************************
! this subroutine is to select a initial bee solution to update.
! input variable :   objective values 
! output variable :  
!					1.index to determin which to update.
! subroutine 1 : get fitness probability 
! subroutine 2:  retrun a bee index
	
	subroutine get_fitness(obj,fitness,prob)
	implicit none

	real*8 fitness(initial_bees),prob(initial_bees),obj(initial_bees)
	real*8 sum 
	integer i

	sum=0
	do i=1,initial_bees
		fitness(i)=1/obj(i)
		sum=sum+fitness(i)
	end do 
	prob=0
	do i=1,initial_bees
		prob(i)=fitness(i)/sum
	end do 

	sum=0
	do i=1,initial_bees
		prob(i)=sum+prob(i)
		sum=prob(i)
	end do 
	
	end subroutine 
	

	subroutine roulette(prob,BN)
	implicit none
	
	real*8 prob(initial_bees)
	real*8 ran
	integer i,BN
	
	call random_number(ran)
	
	do i=1,initial_bees
		if (ran.le.prob(i)) then 
		BN=i
		exit
		end if 
	end do 

	end subroutine 
		
! subroutine generate a chromsome 
	subroutine get_chromsome(BN,chromsome,stop_number,route_D,node_type,o_node,d_node,node_count,node_prob)
	implicit none 


	integer,allocatable::d_select(:),o_select(:),route_node(:)
	integer sl_num(num_terminal_nodes)       ! record the number of starting lines and destination lines
	integer el_num(num_destination_nodes),node_type(num_nodes)
	integer stop_number(initial_bees,max_routes)
	integer chromsome(initial_bees,chromsome_length),route_D(initial_bees,max_routes) 
	integer i,j,r,BN,node,s,position,start,over
	integer o_node(num_terminal_nodes)
	integer d_node(num_destination_nodes)
	integer node_count(initial_bees,num_tsw_nodes)
	real*8 node_prob(num_tsw_nodes,num_destination_nodes)
	real*8 ran
!	integer check_stops(initial_bees,max_routes)
	allocate(d_select(num_destination_nodes),o_select(num_terminal_nodes),route_node(num_tsw_nodes))

	o_select=0
	d_select=0
	sl_num=0
	el_num=0
	stop_number(BN,:)=0

		do r=1,max_routes     

!			Generate original

			route_node=0			! this is only check one particular route.
		
			call random_number(ran)		
			node=int(ran*(num_terminal_nodes)+1)
			
			if (r<=num_terminal_nodes) then 
	! for the first terminals cover all the terminals.		
				do while (o_select(node)==1)
					call random_number(ran)		
					node=int(ran*(num_terminal_nodes)+1)
				end do
			else
				do while (sl_num(node)>=sl_max)
					call random_number(ran)		
					node=int(ran*(num_terminal_nodes)+1)
				end do 
			end if
			
			o_select(node)=1	! the terminal has been select once
			sl_num(node)=sl_num(node)+1				! one route start from this terminal
		
			node=o_node(node)
			position=(r-1)*route_length+1	
			chromsome(BN,position)=node

	!	node_select(node)=node_select(node)+1						! node has been selected.			
			route_node(node)=1							  ! the node has been selected in one route


!          generate number of stops for each route.
!		   minnimus stops is 2 and max is 8 stops 
			stop_number(BN,r)=0
			call random_number(ran)
			stop_number(BN,r)=int(min_initial_stops+ran*(max_stops-min_initial_stops)+1)   ! minial stop number is 3
	
!			generate destination nodes
				call random_number(ran)
				node=int(ran*(num_destination_nodes)+1)
			
			IF (r<=num_destination_nodes) then
		!for the first 5 destinations , cover all the destinations
				do while(d_select(node)==1)
					call random_number(ran)
					node=int(ran*(num_destination_nodes)+1)
				end do
			else 
				do while(el_num(node)>=el_max)
					call random_number(ran)
					node=int(ran*(num_destination_nodes)+1)
				end do 
			end if 	
				route_D(BN,r)=node
				d_select(node)=1
				el_num(node)=el_num(node)+1
				node=d_node(node)
				position=(r-1)*route_length+1+stop_number(BN,r)+1
				chromsome(BN,position)=node

!			generater stop numnber nodes 
				do s=1,stop_number(BN,r)
					call random_number(ran)
!					node=int(ran*(num_tsw_nodes)+1)
					call ran_select_node(node,ran,node_prob,route_D(BN,r))

					do while(route_node(node)==1)
						call random_number(ran)
		!				node=int(ran*(num_tsw_nodes)+1)
						call ran_select_node(node,ran,node_prob,route_D(BN,r))
					end do 

					position=(r-1)*route_length+1+s
					chromsome(BN,position)=node

!					node_select(node)=node_select(node)+1
					route_node(node)=1
				end do 


		
!               Give 0 to the rest of a route gene		
				if (stop_number(Bn,r)<max_stops) then
					do j=stop_number(BN,r)+1,max_stops
						position=(r-1)*route_length+1+j+1
						chromsome(BN,position)=0
					end do 
				end if
			
			end do		! finish generate routes nodes
	
		call check_chromsome(chromsome,BN,node_type,stop_number)



	node_count(BN,:)=0
	do i=1,chromsome_length
		if (chromsome(BN,i)/=0) then 
			if (node_type(chromsome(BN,i))/=2) then 
				node_count(BN,chromsome(BN,i))=node_count(BN,chromsome(BN,i))+1
			end if 
		end if 
	end do 




	deallocate(d_select,o_select,route_node)


	end subroutine 
!****************************************************************
! check minial frequency 
! the subrouting is to return a minal bus_allocation requirement for each 
! in put value 
!				: route_cost(BN,:)
!				: minial frequency 
! output value
!				: minimual bus_allocation requirment for each route
!				: a two dimention vairbale 
	subroutine min_frequency_bus(route_D_cost,BN,min_fre_buses)
	implicit none

	real*8 route_D_cost(initial_bees,max_routes)
	integer min_fre_buses(initial_bees,max_routes)
	integer BN,i,sum

								! route_D_cost is minites convet to hour.
	do i=1,max_routes
		min_fre_buses(BN,i)=int(2*(route_D_cost(BN,i)/60.0)*min_frequency+1)
	end do 

	end subroutine 
	
!**************************************************************
! repair_operator insert operation
! this subroutine is activied when the repair operator all node count==1
!general  procedure 
!					1: find a candy route to insert
!					2: after inert evluate the route cost
!					3: if the cost is higher than 35 
!		input : a node need to insert the other routes
!			  : current route  "can not selected to be insert
!			  : chromsome 
!			  : node_count


	recursive subroutine insert_repair(chromsome,BN,route,stop_number,node_count,route_cost,node_type,link_cost,route_D_cost)


	integer chromsome(initial_bees,chromsome_length),stop_number(initial_bees,max_routes),node_count(initial_bees,num_tsw_nodes)
	real*8 route_cost(initial_bees,max_routes),link_cost(num_nodes,num_nodes)
	integer BN,route,insert_node,node_type(num_nodes)
	real*8 temp_cost1,temp_cost2,tempD1,tempD2
	integer start,over,r,i,j,remove_insert_ok,index,k,temp1,temp2
	integer start2,over2
	real*8 route_D_cost(initial_bees,max_routes)
	real*8,allocatable:: temp_route_cost(:,:),temp_route_D_cost(:,:)
	integer,allocatable::temp_stop_number(:,:),route_gen1(:),route_gen2(:),temp_chromsome(:,:)
	allocate(temp_stop_number(initial_bees,max_routes),temp_route_cost(initial_bees,max_routes),temp_route_D_cost(initial_bees,max_routes))
	allocate(route_gen1(route_length),route_gen2(route_length),temp_chromsome(initial_bees,chromsome_length)) 

	if (stop_number(BN,route)==1) then 
		call swap_repair(chromsome,BN,route,stop_number,node_count,route_cost,node_type,link_cost,route_D_cost,tlt_e_cost)
	return 
	end if 

	start=(route-1)*route_length+1
	over=route*route_length
	temp_route_D_cost(BN,:)=route_D_cost(BN,:)
!	temp_node_count(BN,:)=node_count(BN,:)

	remove_insert_ok=0
	do i=start+1,over
		if (route_cost(BN,route).le.max_tsw_cost) exit
		route_gen1=0
		route_gen2=0
		if (chromsome(BN,i)/=0) then 
			if (node_type(chromsome(BN,i))/=2) then 
				temp_stop_number(BN,:)=stop_number(BN,:)
				temp_chromsome(BN,:)=chromsome(BN,:)
				temp_route_cost(BN,:)=route_cost(BN,:)
				
					insert_node=chromsome(BN,i)     ! insert node is take out the node from this route and insert other route
					do j=1,i-start
					route_gen1(j)=chromsome(BN,j+start-1)
					end do 	 
					do j=i-start+1,route_length-1
					route_gen1(j)=chromsome(BN,j+start)
					end do 
					temp_stop_number(BN,route)=stop_number(BN,route)-1

					call get_route_cost(route_gen1,link_cost,temp_cost1,temp_stop_number(BN,route),tlt_e_cost,temp_route_D_cost(BN,route))
					call optimal_sequence(route_gen1,temp_cost1,temp_stop_number(BN,route),link_cost,temp_route_D_cost,BN,route)
				
					temp_chromsome(BN,start:over)=route_gen1	
					
	!				if (temp_cost1.le.max_tsw_cost) then   ! if remove a node can reduce the cost to 35
					if (temp_cost1.le.temp_route_cost(BN,route)) then			! as long the total cost decrease is ok
						temp_route_cost(BN,route)=temp_cost1
						do r=1,max_routes
							if (r/=route) then 
								if (stop_number(BN,r).lt.max_stops) then 
									if (route_cost(BN,r).lt.max_tsw_cost) then 
										start2=(r-1)*route_length+1
										over2=r*route_length
										route_gen2(:)=chromsome(BN,start2:over2)
										route_gen2(stop_number(BN,r)+3)=route_gen2(stop_number(BN,r)+2)  ! shift the destinationnode
										route_gen2(stop_number(BN,r)+2)=insert_node				! insert the node
										temp_stop_number(BN,r)=stop_number(BN,r)+1
										call get_route_cost(route_gen2,link_cost,temp_cost2,temp_stop_number(BN,r),tlt_e_cost,temp_route_D_cost(BN,r))
										call optimal_sequence(route_gen2,temp_cost2,temp_stop_number(BN,r),link_cost,temp_route_D_cost,BN,r)
											if (temp_cost2.le.max_tsw_cost) then	! only accept when it is less than max
												remove_insert_OK=1
												stop_number(BN,r)=stop_number(BN,r)+1
												stop_number(BN,route)=stop_number(BN,route)-1
												chromsome(BN,start:over)=route_gen1(:)
												chromsome(BN,start2:over2)=route_gen2(:)
												route_cost(BN,route)=temp_cost1
												route_cost(BN,r)=temp_cost2
												route_D_cost(BN,route)=temp_route_D_cost(BN,route)
												route_D_cost(BN,r)=temp_route_D_cost(BN,r)
												exit
											else if (temp_cost2.gt.max_tsw_cost) then 								
												temp_route_cost(BN,:)=route_cost(BN,:)
												temp_chromsome(BN,:)=chromsome(BN,:)
												temp_stop_number(BN,:)=stop_number(BN,:)
												temp_stop_number(BN,route)=stop_number(BN,route)-1
												temp_chromsome(BN,start:over)=route_gen1(:)
														
												temp_route_cost(BN,route)=temp_cost1
												tempD1=temp_route_D_cost(BN,route)
										
												temp_route_D_cost(BN,:)=route_D_cost(BN,:)
												temp_route_D_cost(BN,route)=tempD1
											end if 
											end if 
										 end if 
									end if 
								end do 
								end if 
							end if 
						end if 
						end do 


		if (remove_insert_OK==0) then 
		!	write(9,*) " remove_insert operator failed"
		!	call  swap_start(chromsome,BN,route,stop_number,node_count,route_cost,node_type,link_cost,route_D_cost,tlt_e_cost)
			call  swap_repair(chromsome,BN,route,stop_number,node_count,route_cost,node_type,link_cost,route_D_cost,tlt_e_cost)
			deallocate(temp_stop_number,temp_route_cost,temp_route_D_cost)
			deallocate(route_gen1,route_gen2,temp_chromsome) 
									
			return 
		end if 
		
		if (remove_insert_OK==1) then 
			if (route_cost(BN,route).gt.max_tsw_cost) then 
				call insert_repair(chromsome,BN,route,stop_number,node_count,route_cost,node_type,link_cost,route_D_cost)
			!	call swap_repair(chromsome,BN,route,stop_number,node_count,route_cost,node_type,link_cost,route_D_cost,tlt_e_cost)			
			end if 
		end if 


	deallocate(temp_stop_number,temp_route_cost,temp_route_D_cost)
	deallocate(route_gen1,route_gen2,temp_chromsome) 
	end subroutine 
		


	subroutine swap(A,B)
	implicit none
	
	integer A,B,temp

	temp=A
	A=B
	B=temp

	END SUBROUTINE 
	
				
!**************************************************************************************************************************
!      this subroutine is called when it can not deleted/remove/

		recursive subroutine swap_repair(chromsome,BN,route,stop_number,node_count,route_cost,node_type,link_cost,route_D_cost,tlt_e_cost)
		implicit none
		
		
		integer:: chromsome(initial_bees,chromsome_length),stop_number(initial_bees,max_routes),node_count(initial_bees,num_tsw_nodes),node_type(num_nodes)
		real*8 :: route_cost(initial_bees,max_routes),route_D_cost(initial_bees,max_routes),link_cost(num_nodes,num_nodes)
		integer :: route,BN,r,i,j,node1,node2
		integer :: start1,over1,start2,over2,repair
		integer,allocatable ::route_gen1(:),route_gen2(:)     ! the two route_gen which will be swapped
		integer,allocatable:: temp_chromsome(:,:),temp_node_count(:,:),temp_stop_number(:,:)
		real*8,allocatable ::temp_route_cost(:,:),temp_route_D_cost(:,:)
		real*8 tlt_e_cost(num_destination_nodes)
		allocate(route_gen1(route_length),route_gen2(route_length),temp_chromsome(initial_bees,chromsome_length))
		allocate(temp_route_cost(initial_bees,max_routes),temp_route_D_cost(initial_bees,max_routes),temp_node_count(initial_bees,num_tsw_nodes))
		allocate(temp_stop_number(initial_bees,max_routes))

		temp_stop_number(BN,:)=stop_number(BN,:)
		temp_node_count(BN,:)=node_count(BN,:)
		temp_chromsome(BN,:)=chromsome(BN,:)
		temp_route_cost(BN,:)=route_cost(BN,:)
		temp_route_D_cost(BN,:)=route_D_cost(BN,:)
		

		start1=(route-1)*route_length+1
		over1=route*route_length
		route_gen1(:)=chromsome(BN,start1:over1)
		
		repair=0
		do r=1,max_routes
			if (route_cost(BN,route).le.max_tsw_cost) exit
			if (r/=route) then 
			start2=(r-1)*route_length+1
			over2=r*route_length
			route_gen2(:)=chromsome(BN,start2:over2)
				do i=2,stop_number(BN,route)+1
					if (route_cost(BN,route).le.max_tsw_cost) exit		
						do j=2,stop_number(BN,r)+1
							call swap(route_gen1(i),route_gen2(j))
							call get_route_cost(route_gen1,link_cost,temp_route_cost(BN,route),stop_number(BN,route),tlt_e_cost,temp_route_D_cost(BN,route))
							call optimal_sequence(route_gen1,temp_route_cost(BN,route),stop_number(BN,route),link_cost,temp_route_D_cost,BN,route) 
						!	if (temp_route_cost(BN,route).le.max_tsw_cost) then 
							if (temp_route_cost(BN,route).le.route_cost(BN,route)) then    ! less than current 
								call get_route_cost(route_gen2,link_cost,temp_route_cost(BN,r),stop_number(BN,r),tlt_e_cost,temp_route_D_cost(BN,r))
								call optimal_sequence(route_gen2,temp_route_cost(BN,r),stop_number(BN,r),link_cost,temp_route_D_cost,BN,r)	
								if (temp_route_cost(BN,r).le.max_tsw_cost) then 
									chromsome(BN,:)=temp_chromsome(BN,:)
									route_cost(BN,:)=temp_route_cost(BN,:)
									route_D_cost(BN,:)=temp_route_cost(BN,:)
									node_count(BN,:)=temp_node_count(BN,:)
									stop_number(BN,:)=temp_stop_number(BN,:)
							
							!	deallocate(route_gen1,route_gen2,temp_chromsome)
							!	deallocate(temp_route_cost,temp_route_D_cost,temp_node_count)
							!	deallocate(temp_stop_number)
									repair=1
									exit
																							
								else if (temp_route_cost(BN,r).gt.max_tsw_cost) then 
							 		call swap(route_gen1(i),route_gen2(j))
									temp_chromsome(BN,:)=chromsome(BN,:)
									temp_stop_number(BN,:)=stop_number(BN,:)
									temp_route_cost(BN,:)=route_cost(BN,:)
									temp_route_D_cost(BN,:)=route_D_cost(BN,:)
									temp_node_count(BN,:)=node_count(BN,:)
								end if 
						!	else if (temp_route_cost(BN,route).gt.max_tsw_cost) then 
								else if (temp_route_cost(BN,route).gt.route_cost(BN,route)) then 
									call swap(route_gen1(i),route_gen2(j))
									temp_route_cost(BN,route)=route_cost(BN,route)
									temp_route_D_cost(BN,route)=route_D_cost(BN,route)
							end if 
						end do 
					end do 
				end if
			end do 

		if (repair==0) then 
		!	call swap_repair(chromsome,BN,route,stop_number,node_count,route_cost,node_type,link_cost,route_D_cost,tlt_e_cost)
			 call swap_start(chromsome,BN,route,stop_number,node_count,route_cost,node_type,link_cost,route_D_cost,tlt_e_cost)
			return 
		end if 
	



		deallocate(route_gen1,route_gen2,temp_chromsome)
		deallocate(temp_route_cost,temp_route_D_cost,temp_node_count)
		deallocate(temp_stop_number)
	!	write(*,*) "swap repair failed"

		end subroutine 

! swap original		
		recursive subroutine swap_start(chromsome,BN,route,stop_number,node_count,route_cost,node_type,link_cost,route_D_cost,tlt_e_cost)
		implicit none

		integer:: chromsome(initial_bees,chromsome_length),stop_number(initial_bees,max_routes),node_count(initial_bees,num_tsw_nodes),node_type(num_nodes)
		real*8 :: route_cost(initial_bees,max_routes),route_D_cost(initial_bees,max_routes),link_cost(num_nodes,num_nodes)
		integer :: route,BN,r,i,j,node1,node2
		integer :: start1,over1,start2,over2,candy_flag
		integer,allocatable ::route_gen1(:),route_gen2(:)     ! the two route_gen which will be swapped
		real*8,allocatable ::temp_route_cost(:,:),temp_route_D_cost(:,:)
		real*8 tlt_e_cost(num_destination_nodes)
		allocate(route_gen1(route_length),route_gen2(route_length))
		allocate(temp_route_cost(initial_bees,max_routes),temp_route_D_cost(initial_bees,max_routes))

		
		start1=(route-1)*route_length+1
		over1=route*route_length
		route_gen1(:)=chromsome(BN,start1:over1)

		do r=1,max_routes
			if (r/=route) then 
				start2=(r-1)*route_length+1
				over2=r*route_length
				route_gen2(:)=chromsome(BN,start2:over2)
		
			candy_flag=0				! 0 is ok
				do i=1,route_length
					if (route_gen2(i)==route_gen1(1)) then 
					candy_flag=1								! IF THERE IS ONE NODE IS SAME
					exit
					end if 
				end do 
			if (candy_flag==0) then				! NO SAME NODE
				call swap(route_gen1(1),route_gen2(1))
				call get_route_cost(route_gen1,link_cost,temp_route_cost(BN,route),stop_number(BN,route),tlt_e_cost,temp_route_D_cost(BN,route))
				call optimal_sequence(route_gen1,temp_route_cost(BN,route),stop_number(BN,route),link_cost,temp_route_D_cost,BN,route) 
						if (temp_route_cost(BN,route).le.max_tsw_cost) then 
							call get_route_cost(route_gen2,link_cost,temp_route_cost(BN,r),stop_number(BN,r),tlt_e_cost,temp_route_D_cost(BN,r))
							call optimal_sequence(route_gen2,temp_route_cost(BN,r),stop_number(BN,r),link_cost,temp_route_D_cost,BN,r)	
								if (temp_route_cost(BN,r).le.max_tsw_cost) then 
									chromsome(BN,start1:over1)=route_gen1(:)
									chromsome(BN,start2:over2)=route_gen2(:)
									route_cost(BN,route)=temp_route_cost(BN,route)
									route_cost(BN,r)=temp_route_cost(BN,r)
									route_D_cost(BN,route)=temp_route_D_cost(BN,route)
									route_D_cost(BN,r)=temp_route_D_cost(BN,r)

									write(9,*) "swap original terminal is used"
									write(9,*)  "after swap result"
									write(9,'("route_gen1",1x,10(i2,1x))') route_gen1(:)
									write(9,'("route_gen2",1x,10(i2,1x))') route_gen2(:)
									return 
							
								else if (temp_route_cost(BN,r).gt.max_tsw_cost) then 
							 		call swap(route_gen1(1),route_gen2(1))
									temp_route_cost(BN,route)=route_cost(BN,route)
									temp_route_cost(BN,r)=route_cost(BN,r)
									temp_route_D_cost(BN,route)=route_D_cost(BN,route)
									temp_route_D_cost(BN,r)=route_D_cost(BN,r)
								end if 
							else if (temp_route_cost(BN,route).gt.max_tsw_cost) then 
									call swap(route_gen1(1),route_gen2(1))
									temp_route_cost(BN,route)=route_cost(BN,route)
									temp_route_D_cost(BN,route)=route_D_cost(BN,route)
						end if 
			end if 

			
				end if 
		end do 


	end subroutine 



	!	subroutine get_route_cost(route_gen,link_cost,sum,N,tlt_e_cost,route_D_cost)  
		
		
	!	subroutine optimal_sequence(route_gen,route_cost,N,link_cost,route_D_cost,BN,route)
		
		
		
	subroutine check_node_count(chromsome,node_count,BN,node_type)
	implicit none
	
	integer chromsome(initial_bees,chromsome_length)
	integer node_count(initial_bees,num_tsw_nodes)
	integer node_type(num_nodes)
	integer BN,i
	integer check(num_tsw_nodes)
	
	check=0
	do i=1,chromsome_length
		if (chromsome(BN,i)/=0) then
			if (node_type(chromsome(BN,i))/=2) then
				check(chromsome(BN,i))=check(chromsome(BN,i))+1
			end if 
		end if 
	end do 
	do i=1,num_tsw_nodes
		if (check(i)/=node_count(BN,i)) then 
		write(*,*) "node_count wrong"
		pause
		end if 
	end do 
	end subroutine 
		
		
	subroutine final_get_object(objective,BN,demand,NR_ie,T_ie,frequency,bus_string,RT_ikn,RT_ien,RT_ke,alpha,beta,assignment_penalty)
	implicit none

	real*8 demand(num_tsw_nodes,num_destination_nodes) 
	real*8  T_ie(num_tsw_nodes,num_destination_nodes)
	integer NR_ie(num_tsw_nodes,num_destination_nodes)
	real*8 objective(initial_bees)
	real*8 assignment_penalty(initial_bees)
	real*8 frequency(max_routes)
	integer bus_string(initial_bees,max_routes),RT_ke(num_destination_nodes,max_routes)
	integer RT_ikn(num_tsw_nodes,max_routes),RT_ien(num_tsw_nodes,num_destination_nodes,max_routes)
	integer i,e,BN
	real*8 sum1,sum2
	real*8 alpha,beta

	sum1=0.0
	sum2=0.0

	do i=1,num_tsw_nodes
		do e=1,num_destination_nodes
		sum1=sum1+demand(i,e)*NR_ie(i,e)
!		sum2=sum2+demand(i,e)*T_ie(i,e)				!sum of travel time 
		end do 
	end do 

	objective(BN)=B1*sum1+B2*sum2
	if (objective(BN).le.zero) then 
	write(8,*) " get negetive objecitive"
	end if 
	
	call final_flow_assignment(assignment_penalty,frequency,demand,NR_ie,bus_string,BN,RT_ikn,RT_ien,RT_ke,alpha,beta)

	objective(BN)=objective(BN)+alpha*assignment_penalty(BN)
	
	
!	if (assignment_penalty(BN).le.1) Then 
	!	write(6,*) assignment_penalty(BN),objective(BN)
	!	write(6,*) objective(BN)
!	stop
!	end if 


!	objective(BN)=objective(BN)+100*assignment_penalty(BN)


	end subroutine
		
					
		subroutine final_flow_assignment(assignment_penalty,frequency,demand,NR_ie,bus_string,BN,RT_ikn,RT_ien,RT_ke,alpha,beta)
		implicit none
		
		real*8 frequency(max_routes),demand(num_tsw_nodes,num_destination_nodes)
		integer NR_ie(num_tsw_nodes,num_destination_nodes),bus_string(initial_bees,max_routes),RT_ikn(num_tsw_nodes,max_routes)
		integer RT_ien(num_tsw_nodes,num_destination_nodes,max_routes),RT_ke(num_destination_nodes,max_routes)
		real*8,allocatable::v_ik_e(:,:,:),v_i_ke(:,:,:)
		integer i,e,p,r,BN
		real*8 sum_ik,sum_ie,sum_ke,sum1,sum2
		real*8 penalty_sum,tsw,tlt_e
		real*8 assignment_penalty(initial_bees)
		real*8 alpha,beta,capacity

		allocate(v_ik_e(num_tsw_nodes,num_destination_nodes,max_routes),v_i_ke(num_tsw_nodes,num_destination_nodes,max_routes))
		v_ik_e=0.0
		v_i_ke=0.0

		do i=1,num_tsw_nodes
			do e=1,num_destination_nodes
					sum_ie=0.0
					sum_ik=0.0
					sum_ke=0.0
					do r=1,max_routes
						sum_ie=sum_ie+frequency(r)*RT_ien(i,e,r)
						sum_ik=sum_ik+frequency(r)*RT_ikn(i,r)
						sum_ke=sum_ke+frequency(r)*RT_ke(e,r)
					end do 
			

				do p=1,max_routes
					if (NR_ie(i,e)==1) then 
					!	v_ik_e(i,e,p)=demand(i,e)*NR_ie(i,e)*frequency(p)*RT_ikn(i,p)/dmax1(sum_ik,zero)
					!	v_i_ke(i,e,p)=demand(i,e)*NR_ie(i,e)*frequency(p)*RT_ke(e,p)/dmax1(sum_ke,zero)
						v_ik_e(i,e,p)=demand(i,e)*NR_ie(i,e)*frequency(p)*RT_ikn(i,p)/sum_ik
						v_i_ke(i,e,p)=demand(i,e)*NR_ie(i,e)*frequency(p)*RT_ke(e,p)/sum_ke
					else if (NR_ie(i,e)==0) then 
					!	v_ik_e(i,e,p)=demand(i,e)*frequency(p)*RT_ien(i,e,p)/dmax1(sum_ie,zero)
						v_ik_e(i,e,p)=demand(i,e)*frequency(p)*RT_ien(i,e,p)/sum_ie
						v_i_ke(i,e,p)=v_ik_e(i,e,p)
					!	if (v_i_ke(i,e,p)/=v_ik_e(i,e,p)) then 
					!	stop
					!	end if 
					end if 
				end do 
			end do 
		end do 
		assignment_penalty(BN)=0.0					
		do r=1,max_routes
			sum1=0.0
			sum2=0.0
			do i=1,num_tsw_nodes
				do e=1,num_destination_nodes		   
				   sum1=sum1+v_ik_e(i,e,r)                     ! sum flow within Tsw area
				   sum2=sum2+v_i_ke(i,e,r)						! sum flow from TLT to destination
				end do 
			end do 				 
			capacity=bus_string(BN,r)*num_seats
			tsw=dmax1((sum1-capacity),zero)
			tlt_e=dmax1((sum2-capacity),zero)

			assignment_penalty(BN)=assignment_penalty(BN)+tsw+tlt_e
		
			write(14,'(2(f6.2,1x))') tsw,tlt_e 
	
		end do 
		do i=1,num_tsw_nodes
			write(6,*) "node",i
		!	write(6,'(10(i2,6x))') NR_ie(i,:), NR_ie(i,:)
			do e=1,num_destination_nodes
				write(6,'(20(f6.2,1x))') v_ik_e(i,e,:),v_i_ke(i,e,:) 
			end do 
		end do 
	!	if (assignment_penalty(BN).le.1) then 
	!	write(6,*) "assignment penalty"	,assignment_penalty(BN)
!		end if 	
		
		deallocate(v_ik_e,v_i_ke)
		end subroutine		
	
	
	
	! calculate node probabilty to be selected
		subroutine node_probability(node_prob,demand)
		implicit none
		
		real*8 node_prob(num_tsw_nodes,num_destination_nodes)
		real*8 node_tranfer_cost(num_tsw_nodes)
		real*8 demand(num_tsw_nodes,num_destination_nodes)		
		integer i,e
		real*8 sum
	
		node_tranfer_cost(:)=(/17.3,18.3,17.3,14.4,14.8,15.2,14.9,13.5,13.2,12.4,&
								12.6,11.1,9.7,9.8,11.3,10.0,7.9,8.5,6.0,8.7,4.0,5.5,7.2/)



		do e=1,num_destination_nodes
			sum=0
			do i=1,num_tsw_nodes
				sum=sum+W1*demand(i,e)+W2/node_tranfer_cost(i)
			end do 
			do i=1,num_tsw_nodes
				node_prob(i,e)=(W1*demand(i,e)+W2/node_tranfer_cost(i))/sum
			end do 
		end do 
		
		end subroutine

	

		subroutine ran_select_node(node,ran,node_prob,D)
		implicit none

		real*8 ran,node_prob(num_tsw_nodes,num_destination_nodes)
		integer node,D,i
		real*8 sum
		
		sum=0.0
		do i=1,num_tsw_nodes
			sum=sum+node_prob(i,D)
			if (sum.ge.ran) then 
				node=i
				exit
			end if 
		end do 
		
		end subroutine 









	end module subs
 