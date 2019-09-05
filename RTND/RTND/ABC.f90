! Main code the ABC algorithm 
! Solution representation 
! each gen represents the number of fleet allocated to for a ABC
    
    module ABC
    use mysolclass
    implicit none 

    type, public::abcclass
    integer::npop   ! population size
    integer::onlooker
    integer::maxlimit
    integer::maxiter
    type(solclass),allocatable::chrom(:)
    integer, allocatable::limitcount(:)   ! count the number of limints
    type(graphclass)::basenwk
    integer,allocatable::best_fleet(:)
    real*8::best_fit
    integer::best_id
    real*8,allocatable::BaseODcost(:)
    real*8,allocatable::baselinkflow(:,:)

    contains 
    
    procedure,pass::abcmain=>abcmain
    procedure,pass::inipara=>inipara
    procedure,pass::employ_bee=>employ_bee
    procedure,pass::onlooker_bee=>onlooker_bee
    procedure,pass::scouts=>scouts
    procedure,pass::gen_sol=>gen_sol
    procedure,pass::update_global_best=>update_global_best

    end type abcclass
     
    contains

    subroutine inipara(this)
    implicit none 
    class(abcclass)::this
    integer::val, i
    ! todo : read abc parametrs
    this%npop =  20
    this%onlooker = 5

    open(1,file='c:\gitcodes\BTNDP\input\testnetwork\abc.txt')
    
    do i = 1, 4
        read(1,*) val
        if (i==1) then 
            this%npop = val
        end if 
        if (i==2) then 
            this%onlooker = val
        end if 
        if (i==3) then 
            this%limitcount = val
        end if 
        if (i==4) then 
            this%maxiter =  val
        end if
    enddo 
    close(1)


    allocate(this%chrom(this%npop))
    allocate(this%limitcount(this%npop))
    this%limitcount = 0

    write(*,*) "Num of Pop = ", this%npop
    write(*,*) "Num of Onlooker = ", this%onlooker
    write(*,*) "Num of Limit = ", this%limitcount
    write(*,*) "Max iter = ", this%maxiter

    allocate(this%baseodcost(nod))
    allocate(this%baselinkflow(nl,ndest))

    end subroutine


    subroutine abcmain(this,basenwk)
    implicit none
    CLASS(abcclass)::this
    type(graphclass)::basenwk
    integer:: iter
   ! step 0: read basci parameters for the abc
    call this%inipara
    call this%basenwk%inigraph
    call this%basenwk%copynwk(basenwk)
    ! step 1: generate initial soluitons
    this%best_fleet = 999999
    this%best_fleet = -1 
    call this%gen_sol
    
    iter = 0

    do while(iter.le.this%maxiter) 
        call this%employ_bee
        call this%onlooker_bee
        call this%scouts(this%basenwk)
        iter = iter + 1
    enddo 

    !todo: need to find and update global best soluitons
    deallocate(this%chrom)
    deallocate(this%limitcount)
 

    ! call gen_sol
    end subroutine

    subroutine update_global_best(this,pid)
    implicit none 
    class(abcclass)::this
    integer, intent(in)::pid
    integer::l
    if (this%chrom(pid)%fitness.lt.this%best_fit) then 
        this%best_fit = this%chrom(pid)%fitness
        this%best_id = pid
        do l = 1, nline
            this%best_fleet(l)=this%chrom(pid)%mylines(l)%fleet
        enddo 
    end if
    end subroutine



    ! generate initla solution between upper and lower bound
    subroutine gen_sol(this)
    implicit none
    class(abcclass)::this 
    integer::i,ts, l 
    integer::residule

    do i=1,this%npop
        call this%chrom(i)%generate(this%basenwk)
        ts = 0
        do l = 1, nline
            ts =  ts + this%chrom(i)%mylines(l)%fleet
        enddo 
        residule= fleetsize - ts
        call this%chrom(i)%assign_fleet(residule)
        call this%chrom(i)%evaluate(this%basenwk)
        call this%update_global_best(i)
    end do 

    end subroutine

    subroutine employ_bee(this)
    implicit none 
    class(abcclass)::this
    integer::p
    logical::replaced
    do p = 1, this%npop
       call this%chrom(p)%get_neigh(replaced, this%basenwk)
       if (replaced) then 
        this%limitcount(p) = 0
       else
        this%limitcount(p) = this%limitcount(p) + 1
       end if
       call this%update_global_best(p)
    enddo 
    end subroutine

    subroutine onlooker_bee(this)
    use mutelib
    implicit none 
    class(abcclass):: this
    real*8,allocatable::fits(:)
    integer::p,id
    integer,allocatable::select_list(:)
    logical::replaced


    allocate(fits(this%npop))
    allocate(select_list(this%onlooker))

    do p=1, this%npop
        fits(p) = this%chrom(p)%fitness
    enddo

    call roulette(fits,this%npop,select_list,this%onlooker) 

    do p = 1, this%onlooker
        id = select_list(p)
       call this%chrom(id)%get_neigh(replaced, this%basenwk)
       if (replaced) then 
            this%limitcount(id) = 0
       else
            this%limitcount(id) = this%limitcount(id) + 1
       end if
       call this%update_global_best(p)
    enddo 


    deallocate(fits)
    deallocate(select_list)
    end subroutine


    subroutine scouts(this,basenwk)
    implicit none
    CLASS(abcclass)::this
    type(graphclass)::basenwk
    integer:: p, ts, l, residule

    do p = 1, this%npop 
        if (this%limitcount(p).gt.this%maxlimit) then 
            call this%chrom(p)%generate(basenwk)
            ts = 0
            do l = 1, nline
                ts =  ts + this%chrom(p)%mylines(l)%fleet
            enddo 
            residule = fleetsize - ts
            call this%chrom(p)%assign_fleet(residule)
            call this%chrom(p)%evaluate(this%basenwk)
            call this%update_global_best(p)   
        end if
    enddo

    end subroutine

end module


