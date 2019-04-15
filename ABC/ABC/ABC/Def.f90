! define variables 
!ABC main step
! generate initial solutions
! get fitness
! generate neibourhood solutions for intial solutions (generate,if better than replace)
! routeless select sources( number of on lookers)

include 'para.f90'


integer node_type(num_nodes)  ! 0: not defined node type.  1 terminal .2 destination. 3 other stops.
real*8 demand(num_tsw_nodes,num_destination_nodes)         ! demand from TSw TO DESTSTIONATION
real*8 link_cost(num_nodes,num_nodes)    
real*8 route_cost(initial_bees,max_routes)
real*8 objective(initial_bees)

real*8 fitness(initial_bees),prob(initial_bees)  ! prob is the probability of choose a certain bee to update
real*8 ran                !randome number
integer update
integer chromsome(initial_bees,chromsome_length)
integer bus_string(initial_bees,max_routes)
integer o_node(num_terminal_nodes)
integer d_node(num_destination_nodes)
integer stop_number(initial_bees,max_routes)           ! remark : stop number is the intermedia stop
integer route_gen(route_length)
integer route_D(initial_bees,max_routes)                ! this is to record the destination of each route
														! the number is from 1 to 5
integer min_buses(num_destination_nodes)
integer NR_ie(num_tsw_nodes,num_destination_nodes),RT_ikn(num_tsw_nodes,max_routes),RT_ien(num_tsw_nodes,num_destination_nodes,max_routes)
integer RT_ke(num_destination_nodes,max_routes)
integer local_count(initial_bees)
real*8  frequency(max_routes)
real*8  T_ie(num_tsw_nodes,num_destination_nodes)
real*8  T_ien(num_tsw_nodes,num_destination_nodes,max_routes),T_ikn(num_tsw_nodes,max_routes)
real*8  tlt_e_cost(num_destination_nodes)
integer min_fre_buses(initial_bees,max_routes)
integer node_count(initial_bees,num_tsw_nodes)
real*8  assignment_penalty(initial_bees)
real*8  route_D_cost(initial_bees,max_routes)			! record to the cost to the destination
integer	min_D_buses(num_destination_nodes)           ! minimal number of buses for each destination
integer route_group(num_destination_nodes,el_max)			! one group if their destinaion is same
