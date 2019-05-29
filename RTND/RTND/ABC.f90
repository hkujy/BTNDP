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

    contains 
    
    procedure,pass::abcmain=>abcmain
    procedure,pass::inipara=>inipara
    procedure,pass::employ_bee=>employ_bee
    procedure,pass::onlooker_bee=>onlooker_bee
    procedure,pass::scouts=>scouts
    procedure,pass::gen_sol=>gen_sol
    
    end type abcclass
     
    contains

    subroutine inipara(this)
    implicit none 
    class(abcclass)::this
    ! todo : read abc parametrs
    this%npop =  20
    this%onlooker = 5
    ALLOCATE(this%chrom(this%npop))
    Allocate(this%limitcount(this%npop))
    this%limitcount = 0

    end subroutine


    subroutine abcmain(this)
    implicit none
    CLASS(abcclass)::this
    integer:: iter
    ! step 0: read basci parameters for the abc
    call this%inipara
    ! step 1: generate initial soluitons
    call this%gen_sol
    
    iter = 0

    do while(iter.le.this%maxiter) 
        call this%employ_bee
        call this%onlooker_bee
        call this%scouts
        iter = iter + 1
    enddo 


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
    enddo 


    deallocate(fits)
    deallocate(select_list)
    end subroutine


    subroutine scouts(this)
    implicit none
    CLASS(abcclass)::this
    integer:: p

    do p = 1, this%npop 
        if (this%limitcount(p).gt.this%maxlimit) then 
          call this%chrom(p)%generate

        end if
    enddo

    end subroutine

end module


