! cost averaging: self regulated step size
! reference: Sun and Szeto 2018: logit based transit assignment approachc based formulation and paradox 
    module csramsolverlib
    use solverlib
    implicit none

    type,extends(methods)::csramsolver
    real*8::stepsize
    real*8::eta  ! greater than 1
    real*8::gama ! less than 1
    real*8,allocatable::c0(:)
    real*8,allocatable::c1(:)
    real*8,allocatable::h0(:)  !
    real*8,allocatable::h1(:)
    contains 
        procedure,pass::solver=>solver
        procedure,pass::update_stepsize=>update_stepsize
        procedure,pass::ini=>ini
        procedure,pass::del=>del
        procedure,pass::get_load_prob=>get_load_prob
    end type csramsolver
    
    contains
    
    subroutine ini(this)
    use constpara
    implicit none 
    class(csramsolver)::this
    this%name="CostAve"
    call this%inimethod
    allocate(this%c0(nl))
    this%c0=0
    allocate(this%c1(nl))
    this%c1=0
    allocate(this%h0(nl))
    this%h0=0
    allocate(this%h1(nl))
    this%h1=0
    end subroutine 

    subroutine del(this)
    implicit none 
    class(csramsolver)::this
    call this%delmethod
    deallocate(this%c0)
    deallocate(this%c1)
    deallocate(this%h0)
    deallocate(this%h1)
    end subroutine

    subroutine solver(this,large_step, small_step)
    use constpara
    implicit none
    real*8::norm_value
    real*8::iter
    real*8::large_step, small_step
    integer::subcounter
    reaL*8::et,cputime, time_end, time_begin
    real*8::fx(nl,ndest)
    real*8::x(nl,ndest)
    class(csramsolver):: this  

    this%gapfileno = 98
    open (unit=this%gapfileno,file='c:\gitcodes\logitassign\results\csramgap.txt',&
         status='replace',action="write")
    write(this%gapfileno,*) 'solc,ncperr,disterr'

    call this%geninisol
    call cpu_time(time_begin)
    subcounter = 0
    
    ! this%ncperr = this%getncperr(this%x,this%xfa,this%fx,this%logitprob)
    
    iter = 1.0
    this%stepsize = 1.0
    this%eta = large_step
    this%gama = small_step

    this%c0 = this%nwk%scost
    this%h0 = this%lt - this%nwk%scost
    
    ! in the first iteratirion 
5   if (iter.eq.1.0) then 
        this%stepsize = this%stepsize + this%eta
    else 
      call this%update_stepsize
    end if
    this%c1 = this%c0 + (1/this%stepsize)*this%h0
    this%lt = this%c1
    call this%backward_update_fx(this%fx,this%logitprob,d1=nl,d2=ndest)
    this%x = this%logitprob
    this%c0 =  this%c1
    call this%forward_update_flow(this%x,d1=nl,d2=ndest)
    call this%nwk%link_time(this%lf,this%c1)
    if (iter.gt.1.0) then 
        this%h0 = this%h1
    end if 
    this%h1 = this%c1- this%c0
    this%ncperr = this%getncperr(this%x,this%xfa,this%fx)
    ! write(*,*) norm_value(this%h1,nl),this%ncperr
    call cpu_time(time_end)
		cputime=time_end-time_begin
   
    if ((cputime.gt.maxcpu).or.(iter.gt.macsolc)) then 
       close(1) 
       close(this%gapfileno)
      return
    else 
        iter = iter + 1
        go to 5
    end if
    close(1)
    close(this%gapfileno)
    return
    end subroutine 

    subroutine update_stepsize(this)
    implicit none 

    class(csramsolver)::this
    real*8::h0norm, h1norm
    reaL*8::norm_value
    h0norm = norm_value(this%h0,nl)
    h1norm= norm_value(this%h1,nl)
    if (h1norm.ge.h0norm) then 
      this%stepsize = this%stepsize + this%eta 
    else 
      this%stepsize = this%stepsize + this%gama
    end if

    end subroutine


    subroutine get_load_prob(this)
    implicit none 
    
    class(csramsolver)::this
      

    end subroutine

    



    end module


