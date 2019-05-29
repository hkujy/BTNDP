! Main code the ABC algorithm 
! Solution representation 
! each gen represents the number of fleet allocated to for a ABC
    
    module ABC
    use mysolclass
    implicit none 

    type, public::abcclass
    integer::npop   ! population size
    type(solclass),ALLOCATABLE::chrom(:)
    integer, allocatable::limitcount(:)   ! count the number of limints
    type(graphclass)::basenwk

    contains 
    
    procedure,pass::abcmain=>abcmain
    procedure,pass::inipara=>inipara
    ! procedure,pass::evaluate=>evaluate
    procedure,pass::gen_sol=>gen_sol
    !procedure,pass::get_neigh=>get_neigh
    
    end type abcclass
     
    contains

    subroutine inipara(this)
    implicit none 
    class(abcclass)::this
    ! todo : read abc parametrs
    this%npop =  20
    ALLOCATE(this%chrom(this%npop))
    Allocate(this%limitcount(this%npop))
    this%limitcount = 0

    end subroutine


    subroutine abcmain(this)
    implicit none
    CLASS(abcclass)::this

    ! step 0: read basci parameters for the abc
    call this%inipara
    ! step 1: generate initial soluitons
    call this%gen_sol

    ! call gen_sol
    end subroutine

    ! generate initla solution between upper and lower bound
    subroutine gen_sol(this)
    implicit none
    class(abcclass)::this 
    integer::i,ts, l 
    integer::residule


    do i=1,this%npop
        call this%chrom(i)%generate
        ts = 0
        do l = 1, nline
            ts =  ts + this%chrom(i)%mylines(l)%fleet
        enddo 
        residule= fleetsize - ts
        call this%chrom(i)%assign_fleet(residule)
        call this%chrom(i)%evaluate(this%basenwk)
    end do 

    end subroutine

    ! generate a neighbour by randomly increase the fleet size 
  

    !subroutine get_neigh(this,replaced,basenwk)
    !use constpara
    !use GraphLib
    !implicit none 
    !logical, INTENT(OUT)::replaced
    !class(solclass)::this
    !! integer,intent(in)::now
    !type(lineclass),DIMENSION(nline)::templines
    !real*8::temp_fit
    !real*8::neigh_fleet(nline),now_fleet(nline)
    !
    !do l = 1, nline
    !    call templines(l)%copy(this%dp%nwk%mylines(l))
    !    now_fleet(l) = this%fleet(l)
    !end do 
    !
    !temp_fit = this%fitness
    !! genertate new fleet    
    !call mute_increa(now_fleet, neigh_fleet)
    !call this%set_fleet_and_fre(neigh_fleet)
    !call this%evaluate(this%basenwk)
    !
    !! switch back the new fitness values is wrose
    !if (temp_fit.gt.this%fitness) then 
    !    replaced = .false.
    !    call this%mylines%copy(templines)
    !    this%fitness = temp_fit
    !else
    !    replaced = .true.
    !end if 
    !
    !do l = 1, nline
    !    call basenwk%mylines(l)%copy(templines(l))
    !enddo 
    !
    !

    !end subroutine 

! subroutine employ_bee()
! end subroutine 


!subroutine on_looker()

!end subroutine 


end module


