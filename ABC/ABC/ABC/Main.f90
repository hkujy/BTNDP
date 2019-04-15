   ! intialization  
! 
! purpose
!	ob:				generate number of intial solutions    (intial bees) 
!				just chrosome indicating the stop sequence
!	constrains:  
!				1, node covering( tsw nodes & destination nodes )  					
!				2, max number of stops
!				3, max travel time of a certain route   !still doubt 
!**************************************************************************************
!Procedure 
!		1. generate O
!		2. generate	number of stops
!		3. generate destinaion
!		4. gerenate stop nodes
!		5. check stop covering for the reserve nodes
!*****************************************************************************
!Remark:	
!				1.each node should be selected at least once   : node_select
!				2.each destiniation should be selected at least once : D_select
!				3.each original should be selected at least once : O_select
!******************************************************************
!Questions :   1. how to ensure that each terminal is selected once
!			   2. how to ensure that each destination is selected once
!				
!			answer: 1 for the first 7 terminal, cover all the terminal .
!					2 for the first 5 destination,cover all the destinations.
!		
!		 3. how to ensure that all the nodes is covered once.
!				answer: 1. generate all the routes first
!						2. find the nodes which is select more than once ( random_generation) (ensure the type is not a terminal node)
!						3. find the position of that particalure node and replace that particular node with the node not select

!******************************************************************************
! two way to check the nodes covering 
!			1 insert a node
!			2 replace a current node

!********************************************************************
! for the node insert we add a very high penalty


	include 'module.f90'

	Program intial 
	use subs
	implicit none
	

! input & out put			 
!	integer d_select(num_destination_nodes)
!	integer o_select(num_terminal_nodes)
!	integer route_node(num_tsw_nodes)       ! this variable is to check whether a route has been gerenate with in one route	
							 ! record the number of starting lines and destination lines
!	integer el_num(num_destination_nodes)
!	integer temp_chro(chromsome_length)
	integer temp_bus_string(max_routes)
	integer cycle_feasible_count
	real*8  best_cost(max_routes),infea_best_cost(max_routes)
	integer best_gen(chromsome_length),infea_best_gen(chromsome_length)
	integer best_loc(1),best_route_D(max_routes),best_stop_number(max_routes)
	integer infea_best_loc(1),infea_best_route_D(max_routes),infea_best_stop_number(max_routes)
	real*8  best_obj,temp_best
	real*8  infea_best_obj,infea_temp_best
	real*8	solution_list(1000)
	real*8  travel_cost_penalty
	integer best_bus_string(max_routes),infea_best_bus_string(max_routes)
	integer cycle_count,BN
!	integer check_node_count(num_tsw_nodes)
	integer i,j,r,flag,chr,get_feasible_solution
	integer node,s,position,start,over
	integer solution_count
	integer allseed(20),seed_cycle,seed1(1)
	real*8 sum_demand(num_destination_nodes),c_time,best_alpha,best_beta
	real*8 node_prob(num_tsw_nodes,num_destination_nodes)
	INTEGER(2) time_START(4)
	INTEGER(2) time_OVER(4)
	real*8 alpha,beta,infea_best_alpha,infea_best_beta               ! two paramter for penalty funciong
!	Integer penalty_update_flag     ! an index for updata penalty funcion
	integer no_violate_count			! count solutions viloate constrain.

	integer feasible_solution(initial_bees)

! open file

	open (1,file='..\..\OutPut\data.txt') 
	open (2,file='..\..\OutPut\chrosome.txt') ! out put
	open (3,file='..\..\OutPut\link cost.txt')
	open (4,file='..\..\OutPut\demand.txt')
	open (5,file='..\..\OutPut\tlt_e_cost.txt')   ! this is to record the cost from TLT terminal to destination
	open (6,file='..\..\OutPut\flow.txt')
	open (7,file='..\..\OutPut\best.txt')
	open (8,file='..\..\OutPut\bug_log.txt')
	open (9,file='..\..\OutPut\bus frequency bug log.txt')
	open (10,file='..\..\OutPut\seed.txt')
	open (11,file='..\..\OutPut\time.txt')
	open (12,file='..\..\OutPut\parameter.txt')
	open (13,file='..\..\OutPut\feasible_solutions.txt')
	open (14,file='..\..\OutPut\infeasible_solutions.txt')
! read data
	write(12,*) "colony", initial_bees+onlookers
	write(12,*) "employed bees",initial_bees
	write(12,*) "on lookers ", onlookers
	write(12,*) "rate,d", initial_bees/local_min_control 
	write(12,*) "w1=",w1,"w2=",w2
	read (10,*) allseed(:)


	min_D_buses(:)=(/44,27,23,14,40/)


	


	do i=1,num_destination_nodes
		read(5,*) tlt_e_cost(i)
	end do 

	do i=1,num_tsw_nodes
		read(4,*) demand(i,:)
	end do 

	sum_demand=0.0

	do i=1,num_tsw_nodes	
		do j=1,num_destination_nodes
		sum_demand(j)=demand(i,j)+sum_demand(j)
		end do 
	end do 
	

	do i=1,num_nodes
		read(3,*) link_cost(i,:)
	end do 

	do i=1,num_terminal_nodes
		read(1,*) j,o_node(j)
		if (j/=i) then 
		write(*,*) " read o_node file problem"  
		end if 
	end do 

	do i=1,num_destination_nodes
		read(1,*) j,d_node(j)
		if (j/=i) then 
		write(*,*) " read d_node file problem"
		end if 
	end do 
	
	do i=1,num_nodes
		read(1,*) j,node_type(j)
		if (j/=i) then 
		write(*,*) " read node_type file problem"
		end if 
	end do 
	
	call node_probability(node_prob,demand)




!*****************************************************************
	seed_cycle=0
	
5   seed_cycle=seed_cycle+1	
	get_feasible_solution=0
	 cycle_feasible_count=0
	
	write(13,*) "seed_cycle",seed_cycle

	seed1(1)=allseed(seed_cycle)
	
	CALL GETTIM (time_START(1),time_START(2),time_START(3),time_START(4)) 

	best_obj=9.9E+20
	infea_best_obj=9.9E+20
	alpha=alpha_initial		! alpha==beta
	beta=alpha_initial
 
	write(12,*) "alpha",alpha
	write(12,*) "delta",delta
	call random_seed(put=seed1(:))


	do i=1,initial_bees
		call get_chromsome(i,chromsome,stop_number,route_D,node_type,o_node,d_node,node_count,node_prob)
	end do 	
!	next step  check route time
!   for each chorosme  
!		step 1, decent direction search-----determin the total cost
!		step 2.  if the cost greater than  TMAX
!					than removel a stop which give a max route cost.
!					add a route which give max flow.
!						
	
!********************************************************
!	step 1,get route cost of each chrosome
!	node_count=0
	do i=1,initial_bees
		 route_gen(:)=0
		 do r=1,max_routes
			start=(r-1)*route_length+1
			over=r*route_length
			route_gen(:)=chromsome(i,start:over)
			call get_route_cost(route_gen,link_cost,route_cost(i,r),stop_number(i,r),tlt_e_cost,route_D_cost(i,r))
			call optimal_sequence(route_gen,route_cost(i,r),stop_number(i,r),link_cost,route_D_cost,i,r)
			chromsome(i,start:over)=route_gen(:)
		end do 
		do r=1,max_routes
			if (route_cost(i,r)>max_tsw_cost) then 
				call repair_operator(chromsome,route_cost,link_cost,node_count,stop_number,i,r,node_type,tlt_e_cost,route_D_cost)
			end if
		end do 
		
		temp_bus_string=0			
        call min_frequency_bus(route_D_cost,i,min_fre_buses)
		call bus_allocation(temp_bus_string,i,min_fre_buses,route_D,min_D_buses)					
				bus_string(i,:)=temp_bus_string(:)
		call get_index(chromsome,i,NR_ie,RT_ikn,RT_ien,route_D,node_type,RT_ke)
		call get_frequency(bus_string(i,:),frequency,i,route_D_cost)
		call get_T_ien(chromsome,i,link_cost,route_D,stop_number,T_ien,T_ikn,tlt_e_cost)
		call get_T_ie(T_ie,T_ien,T_ikn,frequency,tlt_e_cost,NR_ie,RT_ien,RT_ikn,RT_ke)	
		call get_object(objective,i,demand,NR_ie,T_ie,frequency,bus_string,RT_ikn,RT_ien,RT_ke,alpha,beta,assignment_penalty)	
	    call optimal_frequency(bus_string,i,frequency,objective,demand,route_cost,T_ien,T_ikn,T_ie, tlt_e_cost,&
									 NR_ie,RT_ikn,RT_ien,RT_ke,min_fre_buses,alpha,beta,assignment_penalty,route_D_cost,route_D,min_D_buses)

	end do 	
		local_count=0
		cycle_count=0			
		solution_count=0
		solution_list=0

do while (cycle_count.le.max_cycle) 
		
	    cycle_feasible_count=0
		
		cycle_count=cycle_count+1

		write(*,*) cycle_count
			no_violate_count=0

		feasible_solution(:)=0

	do i=1,initial_bees

		call get_neighbour(objective,i,chromsome,node_type,stop_number,NR_ie,RT_ikn,RT_ien,RT_ke,route_cost,link_cost,frequency,&
									tlt_e_cost,route_D,bus_string,update,node_count,alpha,beta,assignment_penalty,route_D_cost,min_D_buses,node_prob)
	
	
		if (assignment_penalty(i).le.0.001) then	
			travel_cost_penalty=0
			do r=1,max_routes
				if (route_cost(i,r).gt.max_tsw_cost) then 
				write(9,*) chromsome(i,(r-1)*route_length+1:r*route_length)
				travel_cost_penalty=travel_cost_penalty+(route_cost(i,r)-max_tsw_cost)
				end if 
			end do 

			if (travel_cost_penalty==0) then 
				get_feasible_solution=1		 
				feasible_solution(i)=1
				flag=0
				do j=1,solution_count
					if (objective(i)==solution_list(j)) then 
					flag=1				
					exit
					end if 
				end do 
				if (flag==0) then 		
					solution_list(solution_count+1)=objective(i)
					solution_count=solution_count+1
					cycle_feasible_count=cycle_feasible_count+1
				write(13,'(i3,2x,f14.3)') solution_count,objective(i)
				end if 	
				no_violate_count=no_violate_count+1
			end if 
		end if 	
		
		objective(i)=objective(i)+alpha*travel_cost_penalty


		if (feasible_solution(i)==1) then 
			if (objective(i).le.best_obj) then 
				best_cost(:)=route_cost(i,:)
				best_obj=objective(i)
				best_bus_string=bus_string(i,:)
				best_gen=chromsome(i,:)
				best_route_D=route_D(i,:)
				best_stop_number=stop_number(i,:)
				best_alpha=alpha
				best_beta=beta		
			end if  
		end if 	
	
		if (update==0) then			! 0 : not update
			local_count(i)=local_count(i)+1
		else if (update==1) then 
			local_count(i)=0               
		end if  

		if (local_count(i).ge.local_min_control) then 
			call get_chromsome(i,chromsome,stop_number,route_D,node_type,o_node,d_node,node_count,node_prob)
				local_count(i)=0
				do r=1,max_routes
					start=(r-1)*route_length+1
					over=r*route_length
					route_gen(:)=chromsome(i,start:over)
					call get_route_cost(route_gen,link_cost,route_cost(i,r),stop_number(i,r),tlt_e_cost,route_D_cost(i,r))
					call optimal_sequence(route_gen,route_cost(i,r),stop_number(i,r),link_cost,route_D_cost,i,r)
						chromsome(i,start:over)=route_gen(:)
				end do 
				do r=1,max_routes
					if (route_cost(i,r)>max_tsw_cost) then 
						call repair_operator(chromsome,route_cost,link_cost,node_count,stop_number,i,r,node_type,tlt_e_cost,route_D_cost)
					end if
				end do 	
					temp_bus_string=0			
					 call min_frequency_bus(route_D_cost,i,min_fre_buses)
					 call bus_allocation(temp_bus_string,i,min_fre_buses,route_D,min_D_buses)					
					bus_string(i,:)=temp_bus_string(:)
					call get_index(chromsome,i,NR_ie,RT_ikn,RT_ien,route_D,node_type,RT_ke)
					call get_frequency(bus_string(i,:),frequency,i,route_D_cost)
					call get_T_ien(chromsome,i,link_cost,route_D,stop_number,T_ien,T_ikn,tlt_e_cost)
					call get_T_ie(T_ie,T_ien,T_ikn,frequency,tlt_e_cost,NR_ie,RT_ien,RT_ikn,RT_ke)	
					call get_object(objective,i,demand,NR_ie,T_ie,frequency,bus_string,RT_ikn,RT_ien,RT_ke,alpha,beta,assignment_penalty)	
					call optimal_frequency(bus_string,i,frequency,objective,demand,route_cost,T_ien,T_ikn,T_ie, tlt_e_cost,&
									 NR_ie,RT_ikn,RT_ien,RT_ke,min_fre_buses,alpha,beta,assignment_penalty,route_D_cost,route_D,min_D_buses)
			end if 
	end do
					
	call get_fitness(objective,fitness,prob)

	
	do i =1,onlookers		 
		call roulette(prob,chr)
		call get_neighbour(objective,chr,chromsome,node_type,stop_number,NR_ie,RT_ikn,RT_ien,RT_ke,route_cost,link_cost,frequency,&
									tlt_e_cost,route_D,bus_string,update,node_count,alpha,beta,assignment_penalty,route_D_cost,min_D_buses,node_prob)		

		if (update==0) then		
			local_count(chr)=local_count(chr)+1
		else if (update==1) then 
			local_count(chr)=0
		end if 
	
		feasible_solution(chr)=0
	
		if (assignment_penalty(chr).le.0.001) then
			travel_cost_penalty=0
			do r=1,max_routes
				if (route_cost(chr,r).gt.max_tsw_cost) then 
					write(9,*) chromsome(chr,(r-1)*route_length+1:r*route_length)
					travel_cost_penalty=travel_cost_penalty+(route_cost(chr,r)-max_tsw_cost)
				end if 
			end do 
			if (travel_cost_penalty==0) then 
				feasible_solution(chr)=1
				get_feasible_solution=1
				flag=0
				do j=1,solution_count
					if (objective(chr)==solution_list(j)) then 
						flag=1					! the solution is covered
					exit
					end if 
				end do 
				if (flag==0) then 		
					solution_list(solution_count+1)=objective(chr)
					solution_count=solution_count+1
				    cycle_feasible_count=cycle_feasible_count+1
				write(13,'(i3,2x,f14.3)') solution_count,objective(chr)
				end if 
				no_violate_count=no_violate_count+1
			end if 
		end if 
		
		objective(chr)=objective(chr)+alpha*travel_cost_penalty
	
			
		if (feasible_solution(chr)==1) then 
			if (objective(chr).lt.best_obj) then 	
				best_cost=route_cost(chr,:)
				best_obj=objective(chr)
				best_bus_string=bus_string(chr,:)
				best_gen=chromsome(chr,:)
				best_route_D=route_D(chr,:)
				best_stop_number=stop_number(chr,:)
				best_alpha=alpha
				best_beta=beta
			end if 
		END IF 
	end do 

		infea_best_loc(:)=minloc(objective(:))
		infea_temp_best=objective(infea_best_loc(1))
		if (infea_temp_best.lt.infea_best_obj) then
			infea_best_cost(:)=route_cost(infea_best_loc(1),:)
			infea_best_obj=objective(infea_best_loc(1))
			infea_best_bus_string=bus_string(infea_best_loc(1),:)
			infea_best_gen=chromsome(infea_best_loc(1),:)
			infea_best_route_D=route_D(infea_best_loc(1),:)
			infea_best_stop_number=stop_number(infea_best_loc(1),:)
			infea_best_alpha=alpha
			infea_best_beta=beta
		end if 


	if (no_violate_count.ge.(initial_bees/2)) then 
			alpha=alpha/(1+delta)
			beta=beta/(1+delta)
	else
			alpha=alpha*(1+delta)
			beta=beta*(1+delta)
	end if 
		
	if (seed_cycle.le.5) then 
	write(8,'(5i,2x,3i)') cycle_count,cycle_feasible_count
	end if 



end do 

	write(*,*) "ok after iterations"
	
	write(2,*) "seed cycle is",seed_cycle

	if (get_feasible_solution==0) then 
		write(2,*) "can not find feasible solution"
			chromsome(1,:)=infea_best_gen(:)
			route_D(1,:)=infea_best_route_D(:)
			stop_number(1,:)=infea_best_stop_number(:)
			bus_string(1,:)=infea_best_bus_string(:)
		write(2,'(I2,4x,f12.2)') seed_cycle,infea_best_obj
		write(2,*) "infeasible solution"
		write(7,'(I2,1x,f16.2,2x,"infeasible")') seed_cycle,infea_best_obj
		best_alpha=infea_best_alpha
		best_beta=infea_best_beta
		goto 999
	else
		write(7,'(I2,4x,f12.2)') seed_cycle,best_obj
		chromsome(1,:)=best_gen(:)
		route_D(1,:)=best_route_D(:)
		stop_number(1,:)=best_stop_number(:)
		bus_string(1,:)=best_bus_string(:)
	end if 




999	do r=1,max_routes
		start=(r-1)*route_length+1
		over=r*route_length
	!	write(2,'(12(I2,1x))')	chromsome(1,start:over),stop_number(1,r),route_D(1,r)
		write(2,'(30(I2,1x))')	chromsome(1,start:over),stop_number(1,r),route_D(1,r)
	end do 
!	write(2,'(10(I2,1x))') 	bus_string(1,:)
	write(2,'(30(I2,1x))') 	bus_string(1,:)

	call get_index(chromsome,1,NR_ie,RT_ikn,RT_ien,route_D,node_type,RT_ke)	!	
	call get_T_ien(chromsome,1,link_cost,route_D,stop_number,T_ien,T_ikn,tlt_e_cost)
	do r=1,max_routes
		start=(r-1)*route_length+1
		over=r*route_length
		route_gen(:)=chromsome(1,start:over)
		call  get_route_cost(route_gen,link_cost,route_cost(1,r),stop_number(1,r),tlt_e_cost,route_D_cost(1,r)) 	
	end do 
	call get_frequency(bus_string(1,:),frequency,1,route_D_cost)
	
!	write(2,'(10(f5.2,1x))') route_cost(1,:)	
!	write(2,'(10(f5.2,1x))') frequency 
!	write(2,'(10(f5.2,1x))') route_D_cost(1,:)	
!	write(2,*) best_alpha,best_beta

	write(2,'(30(f5.2,1x))') route_cost(1,:)	
	write(2,'(30(f5.2,1x))') frequency 
	write(2,'(30(f5.2,1x))') route_D_cost(1,:)	
	write(2,*) best_alpha,best_beta

	call get_T_ie(T_ie,T_ien,T_ikn,frequency,tlt_e_cost,NR_ie,RT_ien,RT_ikn,RT_ke)	

	write(6,*) "seed cycle is",seed_cycle

	call final_get_object(objective,1,demand,NR_ie,T_ie,frequency,bus_string,RT_ikn,RT_ien,RT_ke,best_alpha,best_beta,assignment_penalty)
	
	write(2,*) "obj=",objective(1) 
	write(2,*) assignment_penalty(1)

	
	CALL GETTIM (time_OVER(1),time_OVER(2),time_OVER(3),time_OVER(4)) 

	C_TIME=3600*(time_OVER(1)-time_START(1))+60.0*(time_OVER(2)-time_START(2))+(time_OVER(3)-time_START(3))+(time_OVER(4)-time_START(4))/100.0

	WRITE(11,"('seed=',I3,2X,'C_TIME=',F8.2)") seed_cycle,C_TIME


	
	if (seed_cycle.lt.1 ) then 
	go to 5
	end if 

write(*,*) "finaishe"

	END	
