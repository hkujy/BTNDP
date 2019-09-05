    !assignmode 
    !0 :  transit link cost function 
    !1 :  bpr function
    module constpara
    implicit none
    ! un affected constant para
    integer,parameter::maxlinestops = 4
    integer,parameter::maxsecline=4
    integer::num_line_seg_file_rows
    real*8,parameter::bpr_alpha = 0.15
    real*8,parameter::bpr_beta = 4
    logical,parameter::isConstBcm =.true.

    ! variable real para
    real*8,PARAMETER::theta = 0.5
    real*8,parameter::maxfre = 17
    real*8,parameter::minfre = 4
    
    ! variable integer para
    integer::ndest,nod,nn,nl,nline
    logical::isdebug,iswriteconverge 
    integer::networktype,assignmode 
    logical::islogit, isHardCodePathBcm  ! the two logical value determines the model type 

    integer::solver_index   ! 0: projection. 1: msa, 2:cost average 
    integer::load_index ! 0: node_value. 1: STOCH

    integer::tune_solver
    integer::congestion_n
    integer,parameter::macsolc = 100000
    real*8::capk
    real*8::rio  ! variance is not considered
    integer::fleetsize
    ! number of method to generate_neighbour solutions
    integer,parameter::inputseed = 1
    real*8,parameter::walkcost = 0.01
    integer,parameter::maxcom = 20
    integer,parameter::max_total_compete_sec = 100
    integer,parameter::submax = 0
    real*8,parameter::gama = 60.0d0
    real*8,parameter::maxct = 10
    real*8,parameter::zero = 1d-6
    real*8,parameter::maxcpu = 3600 ! 10mins
    real*8,parameter::delfloweps = 1d-5  ! delete flow eps
    !constant parameters
    real*8, parameter::large = 1.0d6
    real*8, parameter::inf1 = 10d15
    real*8, parameter::ncp_eps = 0.01
    real*8, parameter::flow_eps = 0.001d0
    real*8, parameter::ncp_flow_eps = 0.01
    real*8, PARAMETER::bcmratio = 0.5
    integer, PARAMETER::npath = 4
    real*8:: path_link(4,4)   ! path incidencen matrix
    integer::line_links(4,10)  ! noram linke
    integer::caseindex
    integer::piter
    real*8::const_bcm_value
    real*8,allocatable::dist(:) ! node laber for shortest cost
    real*8,allocatable::lndist(:,:)	! the longest distance from node all
    real*8,allocatable::bs(:)
   ! parameter for the bilevel 
    integer,allocatable::fleet_lb(:), fleet_ub(:) ! lower and upper bound of fleet
    real*8,allocatable::fre_lb(:), fre_ub(:) ! lower and upper bound of frequency 

    !const file number
    integer,parameter::dp_tune_para_file_part1 = 21
    integer,PARAMETER::dp_tune_para_file_part2 = 22
    integer,parameter::dp_converge_file = 77
    integer,parameter::logfileno = 99
    integer,parameter::gapfileno = 98


    contains 
    
    subroutine read_test_para
    implicit none
    ! read the test para
    INTEGER::i 
    integer::i_val
    OPEN(1,file='C:\GitCodes\BTNDP\Input\testsetting.txt') 
    do i = 1,8
        read(1,*) i_val
        SELECT CASE (i)
        case(1)
            networktype =  i_val
            select case(networktype)
            case(-2)
                write(*,*) "Network:           STOCH network"
            case(-1)
                write(*,*) "Network:           cycle network"
            case(0)
                write(*,*) "Network:           four nodes route section"
            case(1)
                write(*,*) "Network:           SiouxFall Transit toy demand"
            case(2)
                write(*,*) "Network:           SiouxFall Transit all od demand"
            case(3)
                write(*,*) "Network:           SiouxFall Transport toy od demand"
            case(4)
                write(*,*) "Network:           SiouxFall Transport all od demand"
            end select
        case(2)
            assignmode = i_val
            if (assignmode.eq.1) then 
                write(*,*) "AssignMode:        Transport BPR"
            end if
            IF (assignmode.eq.2) then 
                write(*,*) "AssignMode:        Transit Route Section" 
            end if
        case(3)
            if (i_val.eq.1) then 
                isdebug = .true.
                write(*,*) "RunExe:            Debug Mode"
            else 
                isdebug = .false.
                write(*,*) "RunExe:            Release Mode"
            end if
        case(4)
            if (i_val.eq.1) then 
                iswriteconverge =  .true.
                write(*,*) "WriteConverge:     True"
            else 
                iswriteconverge = .false.
                write(*,*) "WriteConverge:     False"
            end if 
        case (5) ! model index
            if (i_val.eq.0) then 
                write(*,*) "SovlveModel:       UE model"
                pause
            end if 
            if (i_val.eq.1) then 
                write(*,*)  "SolveModel:        Logit Sue"
                islogit = .true.
                isHardCodePathBcm = .false.
            end if 
            if (i_val.eq.2) then 
                write(*,*) "SolveModel:         Hard code path bcm"
                islogit =.true.
                isHardCodePathBcm = .true.      
            end if
        case (6)
            solver_index = i_val
            select case (solver_index)
            case(1)
                write(*,*) "Solver:            Double Projection"
            case(2)
                write(*,*) "Solver:            MSA"
            case(3)
                write(*,*) "Solver:            Cost averaging"
            ! case(3)
                ! write(*,*) "Solve hard code path bcm"
            end select
        case (7)
            tune_solver =  i_val
            if (i_val.eq.1) then 
                write(*,*) "TuneSolverPara:    True"
            else 
                write(*,*) "TuneSolverPara:    False"
            end if
        case(8)
            load_index = i_val
            if(i_val.eq.0) then 
                write(*,*) "Loading method:    Node value"
            else
                write(*,*) "Loading method:    Dial Stoch"
            endif 
        end select
    enddo
 
    end subroutine 
    


    subroutine writepara
    implicit none
    open(1,file='c:\gitcodes\BTNDP\results\test_setting_para.txt')
    write(1,*) "NetworkType,",networktype
    write(1,*) "AssignMode,",assignmode
    write(1,*) "RunExe,",isdebug
    write(1,*) "WriteConvegeFile,",iswriteconverge
    if (isHardCodePathBcm) then 
        WRITE(1,*) "SovlveMode,",2
    else
        WRITE(1,*) "SovlveMode,",1
    endif 
    write(1,*) "SolverIndex,    ",solver_index
    write(1,*) "TuneSolver,     ",tune_solver
    write(1,*) "LoadIndex,      ",load_index
    write(1,*) "NumNodes,       ",nn
    write(1,*) "NumDest,        ",ndest
    WRITE(1,*) "NumOd,          ",nod
    WRITE(1,*) "NumBusLine,     ",nline
    write(1,*) "NumLink,        ",nl
    write(1,*) "congestion_n,   ",congestion_n
    write(1,*) "cap_k,          ",capk
    write(1,*) "rio,            ",rio
    write(1,*) "theta,          ",theta
    write(1,*) "islogit,        ",islogit
    write(1,*) "isdebug,        ",isdebug
    write(1,*) "iswriteconverge,",iswriteconverge
    close(1)
    
    end subroutine
    

   end module







 