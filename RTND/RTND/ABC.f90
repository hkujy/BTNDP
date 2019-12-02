! Main code the ABC algorithm 
! Solution representation 
! each gen represents the number of fleet allocated to for a ABC
    module ABC
    use mysolclass
    use GraphLib
    implicit none 

    type,public::abcclass
    integer::npop   ! population size
    integer::onlooker
    integer::maxlimit
    integer::maxiter
    type(solclass)::BaseCaseSol
    type(graphclass)::basenwk
    real*8::best_fit
    integer::best_id
    type(solclass),allocatable::chrom(:)
    integer,allocatable::limitcount(:)   ! count the number of limints
    integer,allocatable::best_fleet(:)
    real*8,allocatable::BaseODcost(:)
    real*8,allocatable::baselinkflow(:,:)

    contains 
    procedure,pass::abcmain=>abcmain
    procedure,pass::iniabc=>iniabc
    procedure,pass::delabc=>delabc
    procedure,pass::getBaseCaseOd=>getBaseCaseOd
    procedure,pass::employ_bee=>employ_bee
    procedure,pass::onlooker_bee=>onlooker_bee
    procedure,pass::scouts=>scouts
    procedure,pass::gen_sol=>gen_sol
    procedure,pass::update_global_best=>update_global_best
    procedure,pass::getfit=>gitfit

    end type abcclass
    contains
       subroutine gitfit(this)
        implicit none
        class(abcclass)::this
        integer::i,j, statval
        do i = 1, this%npop
            this%chrom(i)%NumBeat = 0
            this%chrom(i)%NumLoss = 0
            this%chrom(i)%fitness = 0
        end do
        do i = 1, this%npop-1
            do j = i+1, this%npop
                statval = this%chrom(i)%compare(this%chrom(j))
                if (statval.eq.1) then 
                    this%chrom(i)%NumBeat = this%chrom(i)%NumBeat + 1
                    this%chrom(j)%NumLoss = this%chrom(i)%NumLoss + 1
                end if
                if (statval.eq.2) then
                    this%chrom(i)%NumLoss = this%chrom(i)%NumLoss + 1
                    this%chrom(i)%NumBeat = this%chrom(i)%NumBeat + 1
                endif
            end do 
        end do
        do i = 1, this%npop
            this%chrom(i)%fitness = real(this%chrom(i)%NumBeat)/real(this%npop)
            write(*,*) "Sol,",i,"fit=",this%chrom(i)%fitness
        enddo
    end subroutine

    subroutine iniabc(this,input_basenwk)
    implicit none 
    class(abcclass)::this
    type(graphclass)::input_basenwk
    integer::val, i

    open(1,file='c:\gitcodes\BTNDP\input\testnetwork\abcpara.txt')
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
    allocate(this%baselinkflow(nl,ndest))
    this%baselinkflow = -1
    allocate(this%BaseODcost(nod))
    this%BaseODcost = -1
    allocate(this%best_fleet(nline))
    this%best_fleet = -1
    write(*,*) "Num of Pop = ", this%npop
    write(*,*) "Num of Onlooker = ", this%onlooker
    write(*,*) "Num of Limit = ", this%limitcount
    write(*,*) "Max iter = ", this%maxiter
    call this%basenwk%inigraph
    call this%basenwk%copynwk(input_basenwk)
   
    end subroutine
    
    subroutine delabc(this)
    implicit none
    class(abcclass)::this
    deallocate(this%chrom)
    deallocate(this%limitcount)
    deallocate(this%baselinkflow)
    deallocate(this%BaseODcost)
    deallocate(this%best_fleet)
    end subroutine

    ! get base case OD cost
    ! this is for the computing fairness values
    subroutine getBaseCaseOd(this)
    implicit none 
    class(abcclass)::this
    integer l

    call this%BaseCaseSol%inisol(this%basenwk)
    call this%BaseCaseSol%dp%ini
    call this%BaseCaseSol%dp%solver(this%basenwk)
    call get_od_cost(this%BaseCaseSol%dp,this%BaseCaseSol%odcost)
    
    write(*,*) "Write initial fleet szie"
    do l = 1, nline 
      write(*,*) l, this%BaseCaseSol%mylines(l)%fleet 
    enddo

    write(*,*) "Write initial OD cost"
    do l = 1, nod
      write(*,*) l,this%BaseCaseSol%odcost(l)
    enddo 
    call this%BaseCaseSol%dp%del

    end subroutine

    subroutine abcmain(this,input_basenwk)
        use mysolclass
        implicit none
        class(abcclass)::this
        type(graphclass),intent(in)::input_basenwk
        integer:: iter
   ! step 0: read bass parameters for the ABC
        call this%iniabc(input_basenwk)
        call this%getBaseCaseOd
    ! step 1: generate initial solutions
        call this%gen_sol
        iter = 0
        do while(iter.le.this%maxiter) 
            call this%employ_bee
            call this%getfit
            !TODO: write the fitness function
            call this%onlooker_bee
            call this%scouts
            iter = iter + 1
        enddo 

        write(*,*) "need to write onlook and new fitness function"
        call this%delabc 

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

    ! generate initial solution between upper and lower bound
    subroutine gen_sol(this)
    implicit none
    class(abcclass)::this 
    integer::i,ts, l 
    integer::residule

    do i=1,this%npop
        call this%chrom(i)%generate(this%basenwk)
        ! ts = 0
        ! do l = 1, nline
            ! ts =  ts + this%chrom(i)%mylines(l)%fleet
        ! enddo 
        ! residule= fleetsize - ts
        ! call this%chrom(i)%assign_fleet(residule)
        call this%chrom(i)%evaluate(this%basenwk,this%BaseCaseSol)
        ! call this%update_global_best(i)
    end do 

    end subroutine

    subroutine employ_bee(this)
    implicit none 
    class(abcclass)::this
    integer::p
    logical::replaced
    do p = 1, this%npop
       call this%chrom(p)%get_neigh(replaced, this%basenwk,this%BaseCaseSol)
       if (replaced) then 
           this%limitcount(p) = 0
       else
           this%limitcount(p) = this%limitcount(p) + 1
       end if
       ! wtf print
    !    call this%update_global_best(p)
    enddo 
    end subroutine

    subroutine onlooker_bee(this)
        use mutelib
        implicit none 
        class(abcclass):: this
        real*8,allocatable::fits(:)
        integer::p,id
        integer,allocatable::ssll(:) ! select list
        logical::replaced

        allocate(fits(this%npop))
        allocate(ssll(this%onlooker))
    
        do p=1, this%npop
            fits(p) = this%chrom(p)%fitness
        enddo

        call roulette(fits,this%npop,ssll,this%onlooker) 
        do p = 1, this%onlooker
            id = ssll(p)
            call this%chrom(id)%get_neigh(replaced, this%basenwk, this%BaseCaseSol)
            if (replaced) then 
                this%limitcount(id) = 0
            else
                this%limitcount(id) = this%limitcount(id) + 1
            end if
            call this%update_global_best(p)
        enddo 

        deallocate(fits)
        deallocate(ssll)
    end subroutine


    subroutine scouts(this)
    implicit none
    CLASS(abcclass)::this
    type(solclass)::basesol
    integer:: p, ts, l, residule

    do p = 1, this%npop 
        if (this%limitcount(p).gt.this%maxlimit) then 
            call this%chrom(p)%generate(this%basenwk)
            ! ts = 0
            ! do l = 1, nline
            !     ts =  ts + this%chrom(p)%mylines(l)%fleet
            ! enddo 
            ! residule = fleetsize - ts
            ! call this%chrom(p)%assign_fleet(residule)
            call this%chrom(p)%evaluate(this%basenwk,this%BaseCaseSol)
            this%limitcount(p) = 0
        end if
    enddo

    end subroutine

    ! update the fitness values of all the solutions 
    ! reference: a multi-objective artifical bee colony algorithm
 
end module


