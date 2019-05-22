
    
    module dpsolverlib
    use constpara
    use SolverLib
    use GraphLib
    implicit none 
    type,extends(methods)::dpsolver
        real*8::beta,lama,v,miu,tau,betastep
        real*8::x_bar(nl,ndest) ! percentage
        real*8::fx_bar(nl,ndest) ! 
    contains 
        procedure,pass::solver=>solver
        procedure,pass::dpmain=>dpmain
        procedure,pass::readpare=>readpare
    end type dpsolver 
    contains 
      
    subroutine readpare(this)
    implicit none 
    integer i
    class(dpsolver)::this
    write(*,*) " write dp solver read paramter"
    
    end subroutine
    
    subroutine solver(this,set_nwk)
    use constpara
    implicit none
    class(dpsolver)::this
    class(graphclass),optional::set_nwk
    integer::i,j
    integer::mb, nb
    !	step 1 fixed beta ! test other three
    open(1,file='c:\gitcodes\BTNDP\results\fortran_dp_para1.txt')
    write(1,*) "case,lama,miu,v"

    open(2,file='c:\gitcodes\BTNDP\results\fortran_dp_para2.txt')
    write(2,*) "case,beta,solc,cputime,error,distanceerr"

    !open(17,file='..\..\results\fortran_finalerr.txt',status='old',position='append')
    open(3,file='c:\gitcodes\BTNDP\results\fortran_finalerr.txt')
    write(3,*) "case,err" 
    ! just use the double project method
    this%name ='dp'
    do mb = 1, 3
        if (mb==1) then
            this%betastep = 0.33d0
        end if
        if (mb==2) then
            this%betastep = 0.67d0
        end if
        if (mb==3) then
            this%betastep = 0.99d0
        end if
        this%lama = 1.9d0
        this%beta = 1.0d0
        do i = 1, 10
            this%v = 0.1d0*i
            do j = 1, 10
                this%miu = 0.1d0*j
                if (this%miu.lt.this%v) then
                    call this%dpmain(set_nwk)
                    if (this%isNCPconverge) then
                        write(3,'(i3,a,f8.4)') caseindex,',',this%ncperr
                        goto 999
                    endif
                    !write(1,'(i3,a,f4.2,a,f4.2,a,f4.2)') caseindex,',',&
                    !this%lama,',', this%miu,',',this%v
                    !!write(2,'(i3,a,f4.2,a,i6,a,f7.4,a,f8.4,a,f10.6)') &
                    !!    caseindex,',',this%betastep,',',this%solc,',',this%cputime,',',this%ncperr,',',this%distanceer
                end if
            end do
        enddo
    enddo
    
    goto 999
    close(1)
    close(2)
    close(3)
999 write(*,*) "done"
     end subroutine

    subroutine dpmain(this,set_nwk)
    use constpara
    implicit none
    class(dpsolver):: this
    class(graphclass),OPTIONAL::set_nwk
    integer::i,subcounter,j
    logical::del
    real*8::alph,numerator,denominator
    real*8::eu(nl,ndest),du(nl,ndest),norm_value2
    !real*8::update_alph
    integer:: temppathset,nowpathsize
    real*8::st,et,r,time_begin,time_end
    
    this%isNCPconverge = .false.
    this%gapfileno = 98

    open(1,file='c:\gitcodes\BTNDP\results\fortran_dp_converge.txt')
    write(1,*) "case,solc,err"
    open (unit=this%gapfileno,file='c:\gitcodes\BTNDP\results\dpgap.txt',&
         status='replace',action="write")
    write(this%gapfileno,*) 'solc,ncperr'    

    call cpu_time(time_begin)
    this%solc = 0
    call this%geninisol(set_nwk)
    subcounter = 0
    this%beta = 0.01
! 10 call cpu_time(et)
10  call projection(this%x_bar,this%x,this%fx,this%beta,this%nwk)
    call this%cal_fx(this%x_bar,this%fx_bar)
    subcounter = subcounter + 1
    !write(7,'(i4,2x,f14.6,2x,f10.6)') solc,err,distanceerr

20  numerator=norm_value2(this%x,this%x_bar,nl,ndest)
    denominator=norm_value2(this%fx,this%fx_bar,nl,ndest)
    if (numerator.eq.0) then
        write(*,*) "numerator = 0, ncperr = ",this%ncperr 
        if (this%ncperr.le.ncp_eps) then
            goto 1000
        else
            goto 998
        endif
    end if
    r=this%beta*denominator/numerator

    if (r.le.this%v) then
        eu =this%x-this%x_bar
        du =eu-this%beta*(this%fx-this%fx_bar)
        alph = update_alph(eu,du,this%beta,this%lama)
        call projection(this%x_bar,this%x,this%fx_bar,alph,this%nwk)
        this%x = this%x_bar
        call this%cal_fx(this%x,this%fx)
        subcounter = subcounter + 1
        ! call cpu_time(et)
        if (r.le.this%miu) this%beta=this%beta/this%betastep
        if ((this%solc.ge.macsolc).or.(this%ncperr<=ncp_eps).or.(subcounter.gt.submax)) goto 1000
        if (this%ncperr>ncp_eps) goto 10
    end if
    ! step 3
998 this%x=this%x_bar
    this%fx=this%fx_bar

    if (this%ncperr<=ncp_eps.or.this%solc.ge.macsolc) goto 1000
        this%beta=this%betastep*this%beta*min(1.0,(1.0/r))
    goto 10

1000 call cpu_time(time_end)
    this%cputime=time_end-time_begin
    del = .true.
    !call updatesub(stt,del,xfa)
    call this%nwk%updatesub(this%lt,del,this%xfa)

    if (this%solc.gt.macsolc) then
        close(1)
        close(this%gapfileno)
        return
    end if 
    if (.not.this%isNCPconverge) then
        subcounter = subcounter + 1
        ! TODO: May need to update subnetwork
        ! call this%backward_update_fx(this%fx,this%logitprob)
        !call this%backward_update_fx(this%fx,this%logitprob)
        go to 10
    else
        close(1)
        close(this%gapfileno)
        return
    end if

    end subroutine

    
    real*8 function update_alph(eu,du,beta,lama)
    use constpara
    implicit none
    real*8::eu(nl,ndest),du(nl,ndest),beta,lama
    integer i,j
    real*8::up,down
    real*8::norm_value0
    up=0.0
    down=0.0
    do i=1,ndest
        do j=1,nl
            up=up+eu(j,i)*du(j,i)
        end do
    end do
    down=norm_value0(du,nl,ndest)
    down=down**2
    update_alph=lama*beta*up/down

    return
    end function


end module 
