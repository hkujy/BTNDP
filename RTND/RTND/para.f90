
    module constpara
    implicit none
    logical, parameter::islogit = .false. ! logit model assignment 
    logical, parameter::isusebcm = .false.
    real*8, parameter::theta = 0.2 ! parameter for the logit model
    logical,parameter::isdebug = .true.
    logical,parameter::isConstBcm =.false.
    real*8,parameter::maxfre = 17
    real*8,parameter::minfre = 4
    integer,parameter::totalfleet = 15
    integer,parameter::logfileno = 99
    integer,parameter::gapfileno = 98
    ! number of method to generate_neighbour solutions
    integer,parameter::inputseed = 1
    integer,parameter::ndest=1  !number of distinations
    integer,parameter::nline=4	! number of line
    integer,parameter::nod=1	! number of od pairs
    integer,parameter::nn=4   ! number of nodes
    integer,parameter::nl=6	!number of links
    real*8,parameter::walkcost = 0.01
    integer,parameter::maxcom = 10
    integer,parameter::submax=0
    integer,parameter::maxsecline=4
    integer,parameter::maxlinestops = 4
    real*8,parameter::gama=60.0d0
    integer,parameter::maxiteration=1000
    real*8,parameter::maxct = 10
    integer,parameter::macsolc = 100
    real*8,parameter::zero = 1d-4
    real*8,parameter::maxcpu = 3600 ! 10mins
    real*8,parameter::delfloweps = 1d-5  ! delete flow eps
    !constant parameters
    real*8, parameter:: large = 1.0d10
    real*8, parameter:: inf1=10d15
    real*8, parameter::ncp_eps=0.0001
    real*8, parameter::flow_eps=0.001d0
    real*8, parameter::ncp_flow_eps = 0.1
    real*8, PARAMETER::rio=0.0  ! variance is not considered
    real*8, PARAMETER::bcmratio = 0.5
    integer::line_links(4,10)
    integer::caseindex=1
    integer::piter
    real*8::dist(nn) ! node laber for shortest cost
    real*8::lndist(nn,ndest)	! the longest distance from node all
    integer::congestion_n
    real*8::bs(nl)
    real*8::capk
    contains 
    
    subroutine read_para
        implicit none
        INTEGER i 
        write(logfileno,*) "TODO: to write, const para" 
    end subroutine 

   subroutine set_line_links
   implicit none
   ! to do, input the links associated with the lines
   !  lines	links
   !1, (1)
   !2, (2	3)
   !3, (4	5)
   !4, (6)

   line_links = -1
   line_links(1,1) = 1

   line_links(2,1) = 2
   line_links(2,2) = 3

   line_links(3,1) = 4
   line_links(3,2) = 5
   line_links(4,1) = 6

   end subroutine

    
    end module
 