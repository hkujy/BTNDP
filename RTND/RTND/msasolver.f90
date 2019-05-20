 
    module msasolverlib
    use solverlib
    implicit none

    type,extends(methods)::msasolver
    real*8::stepsize
    real*8::disterr
    contains 
        procedure,pass::solver=>solver
        !procedure,pass::cal_fx=>cal_fx
    end type msasolver
    
    contains 
   
    subroutine solver(this)
    use constpara
    implicit none
    real*8::x0(nl,ndest)
    real*8::iter
    integer::subcounter
    logical::del
    reaL*8::et,cputime, time_end, time_begin
    class(msasolver):: this  

    this%gapfileno = 98
    open (unit=this%gapfileno,file='c:\gitcodes\logitassign\results\msagap.txt',&
         status='replace',action="write")
    write(this%gapfileno,*) 'solc,ncperr,diserr'

    call this%geninisol
  
    subcounter = 0
    call cpu_time(time_begin)

    iter = 1.0
    this%stepsize = 1.0
5   x0 = this%x
    call msa(x0,this%logitprob,this%x,this%stepsize,nl,ndest) ! two dimension msa
    subcounter = subcounter + 1
    call this%cal_fx(this%x, this%fx)

1000	call cpu_time(time_end)
		cputime=time_end-time_begin
		if (this%ncperr<=ncp_eps.or.(et-time_begin).gt.maxcpu.or.(this%solc.gt.macsolc)) return
    del = .true.
    call this%nwk%updatesub(this%lt,del,this%xfa)

    !if (.not.ueconverge) then
    if (this%ncperr<=ncp_eps.or.(et-time_begin).gt.maxcpu) then
      close(1) 
      close(this%gapfileno)
      return 
    else
        iter = iter + 1.0
        this%stepsize = 1.0/iter
        go to 5
    end if
		!end if 

    close(1)
    close(this%gapfileno)
    return
    end subroutine 

 
    !
    !subroutine cal_fx(this,x,fx)
    !use constpara
    !implicit none 
    !
    !real*8,DIMENSION(nl,ndest):: x
    !real*8:: fx(nl, ndest)
    !class(msasolver):: this
    !
    !call this%forward_update_flow(x)
    !call this%nwk%link_time(this%lf,this%lt)
    !call this%backward_update_fx(fx,this%logitprob)
    !
    !end subroutine
 
  end module