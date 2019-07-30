 
    module msasolverlib
    use solverlib
    implicit none

    type,extends(methods)::msasolver
    real*8::stepsize
    contains 
    
        procedure,pass::solver=>solver
        procedure,pass::ini=>ini
        procedure,pass::del=>del
    end type msasolver
    
    contains
    
    subroutine ini(this)
    use constpara
    implicit none 
    class(msasolver)::this
    this%name = "MSA"
    call this%inimethod
    end subroutine 

    subroutine del(this)
    implicit none 
    class(msasolver)::this
    call this%delmethod
    end subroutine


   
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
    write(this%gapfileno,*) 'solc,ncperr,disterr'

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
  end module