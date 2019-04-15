! define the parameters

 
integer,parameter::initial_bees=50		!number of intial solution generated10
integer,parameter::onlookers=50  ! on lookers search for each neighborhood
integer,parameter::max_cycle=1000
integer,parameter::local_min_control=50
real*8,parameter ::delta=0.001
real*8,parameter ::colony_size=100
real*8,parameter ::alpha_initial=0.1
real*8,parameter :: W1=100.0			!demand weight
real*8,parameter :: W2=0			!weight for travel cost


integer,parameter::min_initial_stops=4         ! use  4 when the number of route is 10
integer,parameter::num_nodes=28
integer,parameter::num_tsw_nodes=23
integer,parameter::num_terminal_nodes=7
integer,parameter::num_destination_nodes=5
integer,parameter::max_stops=8
integer,parameter::max_routes=10
integer,parameter::max_buses=176
integer,parameter::num_seats=150          
integer,parameter::chromsome_length=100        ! max_stop*max_routes
integer,parameter::route_length=10
integer,parameter::sl_max=3
integer,parameter::sl_min=1
integer,parameter::el_max=3
integer,parameter::el_min=1
real*8,parameter:: stop_time=1.5
real*8,parameter:: B1=1.0				!Weighting Parameters?
real*8,parameter:: B2=1.0
real*8,parameter:: max_tsw_cost=35.0							! 35
real*8,parameter:: zero=1E-45
real*8,parameter:: min_frequency=3              !0.208
