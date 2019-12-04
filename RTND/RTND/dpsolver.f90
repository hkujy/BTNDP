    module dpsolverlib
    use constpara
    use SolverLib
    use GraphLib
    implicit none 
    type,extends(methods)::dpsolver
        real*8::beta,lama,v,miu,tau,betastep
        real*8,allocatable::x_bar(:,:) ! percentage
        real*8,allocatable::fx_bar(:,:) ! 
    contains 
        procedure,pass::solver=>solver
        procedure,pass::tunesolver=>tunesolver
        procedure,pass::dpmain=>dpmain
        procedure,pass::readpare=>readpare
        procedure,pass::ini=>ini
        procedure,pass::del=>del
        !procedure,pass::outputod=>outputod
    end type dpsolver 
    contains 

    subroutine ini(this)
    use constpara
    implicit none 
    class(dpsolver)::this
    call this%inimethod
    if (.not.allocated(this%x_bar)) then 
        allocate(this%x_bar(nl,ndest))
        allocate(this%fx_bar(nl,ndest))
    end if
    this%x_bar=0
    this%fx_bar = 0
    end subroutine 

    subroutine del(this)
    implicit none 
    class(dpsolver)::this
    call this%delmethod
    deallocate(this%x_bar)
    deallocate(this%fx_bar)
    end subroutine

    subroutine readpare(this)
    implicit none 
    class(dpsolver)::this
    real*8::dta(5)
    write(*,*) " write dp solver read paramter"
    open(1, file='C:\GitCodes\BTNDP\Input\dppara.txt')
    read(1,*) dta(:)
    close(1)
    this%lama = dta(1)
    this%miu = dta(2)
    this%v = dta(3)
    this%beta = dta(4)
    this%betastep = dta(5)

    end subroutine
   
    subroutine tunesolver(this,lam,beta0)    
    use constpara
    implicit none
    class(dpsolver)::this
    integer::i,j
    real*8::lam, beta0
    integer::mb
    !	step 1 fixed beta ! test other three

    open(dp_tune_para_file_part1,file='c:\gitcodes\LogitAssign\results\fortran_dp_para1.txt',status='old',position='append')
    open(dp_tune_para_file_part2,file='c:\gitcodes\BTNDP\results\fortran_dp_para2.txt',status='old',position='append')
    open(dp_converge_file,file='c:\gitcodes\BTNDP\results\converge_dp_para.txt',status='old',position='append')
    !write(77,*) "lama,miu,v,inibeta,betastep,solc,cputime,ncperr"
    ! write(*,*) "lama,miu,v,inibeta,betastep,solc,cputime,ncperr"
    ! just use the double project method
    this%name ='dp'
    do mb = 1, 9
        do i = 1, 10
            this%v = 0.1d0*real(i)
            do j = 1, 10
                this%miu = 0.1d0*real(j)
                if (this%miu.lt.this%v) then
                    this%beta = beta0
                    this%betastep = 0.1*mb 
                    this%lama  =lam
                    call this%dpmain
                    
                    write(dp_converge_file,'(f4.2,a,f4.2,a,f4.2,a,f4.2,a,f4.2,a,i6,a,f8.4,a,f10.6)') this%lama,",",this%miu,",",this%v,",",&
                        beta0,",",this%betastep,",",this%solc,",",this%cputime,",",this%ncperr
                    if (this%isNCPconverge) then
                        write(*,*) "Ncp Converge = True"
                        ! write(dp_converge_file,'(f4.2,a,f4.2,a,f4.2,a,f4.2,a,f4.2,a,i6,a,f8.4,a,f10.6)') this%lama,",",this%miu,",",this%v,",",&
                        ! beta0,",",this%betastep,",",this%solc,",",this%cputime,",",this%ncperr
                        write(*,'(f4.2,a,f4.2,a,f4.2,a,f4.2,a,f4.2,a,i6,a,f8.4,a,f10.6)') this%lama,",",this%miu,",",this%v,",",&
                        beta0,",", this%betastep,",",this%solc,",",this%cputime,",",this%ncperr
                         ! call this%outputx
                        ! goto 999
                    endif
                    write(dp_tune_para_file_part1,'(i3,a,f4.2,a,f4.2,a,f4.2)') caseindex,',',&
                                this%lama,',', this%miu,',',this%v
                    write(dp_tune_para_file_part2,'(i3,a,f4.2,a,i6,a,f7.4,a,f8.4,a,f10.6)') &
                            caseindex,',',this%betastep,',',this%solc,',',this%cputime,',',this%ncperr
                end if
            end do
        enddo
    enddo
    goto 999
    close(dp_tune_para_file_part1)
    close(dp_tune_para_file_part2)
    close(dp_converge_file)
999 write(*,*) "done"
    end subroutine

    subroutine solver(this,set_nwk)
    use constpara
    implicit none
    class(dpsolver)::this
    class(graphclass),optional::set_nwk
    integer::i,j
    integer::mb
    !	step 1 fixed beta ! test other three
    open(1,file='c:\gitcodes\BTNDP\results\fortran_dp_para1.txt',status='old',position='append',action='write')
    open(2,file='c:\gitcodes\BTNDP\results\fortran_dp_para2.txt',status='old',position='append',action='write')
    !open(17,file='..\..\results\fortran_finalerr.txt',status='old',position='append')
    open(3,file='c:\gitcodes\BTNDP\results\fortran_finalerr.txt',status='old',position='append')
    ! just use the double project method
    this%name ='dp'
    do mb = 1, 3
       this%lama = 1.9d0
        do i = 1, 10
            this%v = 0.1d0*i
            do j = 1, 10
                this%miu = 0.1d0*j
                if (this%miu.lt.this%v) then
                    this%beta = 1.0d0
                    if (mb==1) then
                        this%betastep = 0.33d0
                    end if
                    if (mb==2) then
                        this%betastep = 0.67d0
                    end if
                    if (mb==3) then
                        this%betastep = 0.99d0
                    end if
                    call this%dpmain(set_nwk)
                    if (this%isNCPconverge) then
                        write(3,'(i3,a,f8.4)') caseindex,',',this%ncperr
                        goto 999
                    endif
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
    class(dpsolver)::this
    class(graphclass),OPTIONAL::set_nwk
    integer::i,subcounter,j,nr,l,cc
    logical::del_subgraph_link
    real*8::alph,numerator,denominator
    real*8,allocatable::eu(:,:),du(:,:)
    real*8::norm_value2
    real*8::r,time_begin,time_end
    logical::oldsublink(nl,ndest), issubequal
    allocate(eu(nl,ndest))
    allocate(du(nl,ndest))
    this%isNCPconverge = .false.
    this%gapfileno = 98

    open(1,file='c:\gitcodes\BTNDP\results\fortran_dp_converge.txt',&
         status='old',position='append')
    open (unit=this%gapfileno,file='c:\gitcodes\BTNDP\results\dpgap.txt',&
         status='replace',action="write")
    write(this%gapfileno,"(a5,a6)") "solc,","ncperr"    

    call cpu_time(time_begin)
    this%solc = 0
    call this%geninisol(set_nwk)
    subcounter = 0
    !this%beta = 0.01
! 10 call cpu_time(et)
10  call projection(this%x_bar,this%x,this%fx,this%beta,this%nwk,nl,ndest)
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
        eu = this%x-this%x_bar
        du = eu-this%beta*(this%fx-this%fx_bar)
        alph = update_alph(eu,du,this%beta,this%lama,d1=nl,d2=ndest)
        call projection(this%x_bar,this%x,this%fx_bar,alph,this%nwk,nl,ndest)
        this%x = this%x_bar
        call this%cal_fx(this%x,this%fx)
        subcounter = subcounter + 1
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
    del_subgraph_link = .true.
    !call updatesub(stt,del,xfa)
    if (this%ncperr<=ncp_eps) then 
        oldsublink =  this%nwk%sublink
        call this%nwk%updatesub(this%lt,del_subgraph_link,this%xfa)
        issubequal= .true.
        cc = 0
        do nr = 1,ndest
            do l = 1, nl
                if (this%nwk%sublink(l,nr).ne.oldsublink(l,nr)) then 
                    issubequal = .false.
                    cc = cc + 1
                    WRITE(*,*) cc,l,nr
                endif
            enddo 
        enddo 
        if (.not.issubequal) then 
            call this%backward_update_fx(this%fx,this%logitprob,d1=nl,d2=ndest)
            goto 10
        endif
    end if

    if (this%solc.gt.macsolc) then
        close(1)
        close(this%gapfileno)
        deallocate(eu,du)
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
        deallocate(eu,du)
        return
    end if

    end subroutine

    
    real*8 function update_alph(eu,du,beta,lama,d1,d2)
    use constpara
    implicit none
    integer,INTENT(IN)::d1,d2
    real*8,INTENT(IN)::eu(d1,d2),du(d1,d2)
    real*8::beta,lama
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
