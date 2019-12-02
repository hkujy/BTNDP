    module pathbcmlib
    use constpara
    use SolverLib
    use GraphLib
    implicit none 
    type,extends(methods)::pbcmsolver
        real*8::beta,lama,v,miu,tau,betastep
        ! for the hard code bcm, ndest = 
        real*8,ALLOCATABLE::path_flow(:,:)
        real*8,ALLOCATABLE::path_x(:,:)
        real*8,ALLOCATABLE::path_fx(:,:)
        real*8,ALLOCATABLE::path_cost(:)
        real*8,ALLOCATABLE::path_cost_wav(:)     !u _wave cost: revised the cost for the mapping function
        real*8,ALLOCATABLE::path_bcm_logit(:)
        real*8,ALLOCATABLE::path_x_bar(:,:)  ! percentage
        real*8,ALLOCATABLE::path_fx_bar(:,:) ! 

    contains 

        procedure,pass::solver=>solver
        procedure,pass::dpmain=>dpmain
        procedure,pass::readpare=>readpare
        procedure,pass::gen_path_sol=>gen_path_sol
        procedure,pass::init_path_flow=>init_path_flow  !initiate, path based flow
        procedure,pass::cal_path_bcm_fx=>cal_path_bcm_fx
        procedure,pass::get_path_bcm_ncperr=>get_path_bcm_ncperr
        procedure,pass::path_bcm_outputx=>path_bcm_outputx
        procedure,pass::ini=>ini
        procedure,pass::del=>del

    end type pbcmsolver
    contains 
    

    subroutine ini(this)
    use constpara
    implicit none 
    class(pbcmsolver)::this
    call this%inimethod
    allocate(this%path_flow(npath,ndest))
    this%path_flow=0
    allocate(this%path_x(npath,ndest))
    this%path_x=0
    allocate(this%path_fx(npath,ndest))
    this%path_fx=0
    allocate(this%path_cost(npath))
    this%path_cost=0
    allocate(this%path_cost_wav(npath))    !u _wave cost: revised the cost for the mapping function
    this%path_cost_wav =0
    allocate(this%path_bcm_logit(npath))
    this%path_bcm_logit = 0
    allocate(this%path_x_bar(npath,ndest))  ! percentage
    this%path_x_bar=0
    allocate(this%path_fx_bar(npath,ndest)) ! 
    this%path_fx_bar = 0

    end subroutine
    subroutine del(this)
    implicit none 
    class(pbcmsolver):: this
    call this%delmethod
    deallocate(this%path_flow)
    deallocate(this%path_x)
    deallocate(this%path_fx)
    deallocate(this%path_cost)
    deallocate(this%path_cost_wav)    !u _wave cost: revised the cost for the mapping function
    deallocate(this%path_bcm_logit)
    deallocate(this%path_x_bar)  ! percentage
    deallocate(this%path_fx_bar) ! 

    end subroutine


    subroutine readpare(this)
    implicit none 
    integer i
    class(pbcmsolver)::this
    write(*,*) " write dp solver read paramter"
    end subroutine
    
    subroutine solver(this,set_nwk)
    use constpara
    implicit none
    class(pbcmsolver)::this
    class(graphclass),optional::set_nwk
    integer::i,j
    integer::mb, nb



   open(1,file='C:\GitCodes\LogitAssign\Input\TestNetwork\bcmval.txt')
   read(1,*) const_bcm_value
   close(1) 

    
    !	step 1 fixed beta ! test other three
    open(1,file='c:\gitcodes\LogitAssign\results\fortran_dp_para1.txt',status='old',position='append')
    open(2,file='c:\gitcodes\LogitAssign\results\fortran_dp_para2.txt',status='old',position='append')
    !open(17,file='..\..\results\fortran_finalerr.txt',status='old',position='append')
    open(3,file='c:\gitcodes\LogitAssign\results\fortran_finalerr.txt',status='old',position='append')
    open(4,file='c:\gitcodes\LogitAssign\results\pathbcm_converge_para.txt')
    ! just use the double project method
    this%name ='dp_bcm'
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
                        write(*,'(f4.2,a,f4.2,a,f4.2,a,f4.2,a,f4.2,a,i6,a,f8.4,a,f10.6)') this%lama,",",this%miu,",",this%v,",",&
                        this%beta,",", this%betastep,",",this%solc,",",this%cputime,",",this%ncperr
                        write(4,*) "bcm =", const_bcm_value
                        write(*,*) "bcm =", const_bcm_value
                        write(4,'(f4.2,a,f4.2,a,f4.2,a,f4.2,a,f4.2,a,i6,a,f8.4,a,f10.6)') this%lama,",",this%miu,",",this%v,",",&
                        this%beta,",", this%betastep,",",this%solc,",",this%cputime,",",this%ncperr
                        pause
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
    ! write path bcm output



999 write(*,*) "done"
     end subroutine

    subroutine dpmain(this,set_nwk)
    use constpara
    implicit none
    class(pbcmsolver)::this
    class(graphclass),OPTIONAL::set_nwk
    integer::i,subcounter,j
    logical::del
    real*8::alph,numerator,denominator
    !real*8::eu(nl,ndest),du(nl,ndest),path_norm_value
    real*8::eu(npath,ndest),du(npath,ndest),path_norm_value
    real*8::path_update_alph
    integer:: temppathset,nowpathsize
    real*8::st,et,r,time_begin,time_end
    
    this%isNCPconverge = .false.
    this%gapfileno = 98

    open(1,file='c:\gitcodes\LogitAssign\results\fortran_dp_converge.txt',&
         status='old',position='append')
    open (unit=this%gapfileno,file='c:\gitcodes\LogitAssign\results\dpgap.txt',&
         status='replace',action="write")
    write(this%gapfileno,"(a5,a6)") "solc,","ncperr"    

    call cpu_time(time_begin)
    this%solc = 0
    ! call this%geninisol(set_nwk)
    call this%gen_path_sol 
    subcounter = 0
    this%beta = 0.1
! 10 call cpu_time(et)
!10  call projection(this%x_bar,this%x,this%fx,this%beta,this%nwk)
10  call path_projection(this%path_x_bar,this%path_x,this%path_fx,this%beta,this%nwk)
    !call this%cal_fx(this%x_bar,this%fx_bar)
    call this%cal_path_bcm_fx(this%path_x_bar,this%path_fx_bar,this%path_flow)
    subcounter = subcounter + 1

!20  numerator=norm_value2(this%x,this%x_bar,nl,ndest)
!20  numerator=norm_value2(this%x,this%x_bar,npath,ndest)
20  numerator = path_norm_value(this%path_x,this%path_x_bar,npath,ndest)
    !denominator=norm_value2(this%fx,this%fx_bar,npath,ndest)
    denominator = path_norm_value(this%path_fx,this%path_fx_bar,npath,ndest)
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
        !eu = this%x-this%x_bar
        eu = this%path_x-this%path_x_bar
        !du = eu-this%beta*(this%fx-this%fx_bar)
        du = eu-this%beta*(this%path_fx-this%path_fx_bar)
        alph = path_update_alph(eu,du,this%beta,this%lama)
        !call projection(this%x_bar,this%x,this%fx_bar,alph,this%nwk)
        call path_projection(this%path_x_bar,this%path_x,this%path_fx_bar,alph,this%nwk)
        !this%x = this%x_bar
        this%path_x = this%path_x_bar
        !call this%cal_fx(this%x,this%fx)
        call this%cal_path_bcm_fx(this%path_x,this%path_fx,this%path_flow)
        subcounter = subcounter + 1
        if (r.le.this%miu) this%beta = this%beta/this%betastep
        if ((this%solc.ge.macsolc).or.(this%ncperr<=ncp_eps).or.(subcounter.gt.submax)) goto 1000
        if (this%ncperr>ncp_eps) goto 10
    end if
    ! step 3
!998 this%x = this%x_bar
998 this%path_x = this%path_x_bar
    !this%fx =this%fx_bar
    this%path_fx =this%path_fx_bar

    if (this%ncperr<=ncp_eps.or.this%solc.ge.macsolc) goto 1000
        this%beta=this%betastep*this%beta*min(1.0,(1.0/r))
    goto 10

1000 call cpu_time(time_end)
    this%cputime=time_end-time_begin
    del = .true.

    if (this%solc.gt.macsolc) then
        close(1)
        close(this%gapfileno)
        return
    end if 
    if (.not.this%isNCPconverge) then
        subcounter = subcounter + 1
        go to 10
    else
        close(1)
        close(this%gapfileno)
        return
    end if

    end subroutine

   


    subroutine gen_path_sol(this)
    implicit none 

    class(pbcmsolver)::this
    !TODO: write the path generation for the path bcm choice generation
    call this%nwk%readnwt
    !call this%nwk%readnwt(numlink=nl,numnode=nn,numline=nline,maxcomsec=maxcom,&
                    !maxseclineval=maxsecline,maxlinestopval=maxlinestops)
    call this%nwk%minspantree(this%nwk%scost)
    call this%init_path_flow
    call this%nwk%getorder
    call this%nwk%countconect
    call update_linkflow_from_path(this%path_x*this%nwk%demand(1), this%lf,this%path_x,this%nwk%demand(1))

    !call this%initial_x
    !call this%forward_update_flow(this%x)
    call this%nwk%link_time(this%lf,this%lt)
    !call this%nwk%updatesub(this%lt,.false.,this%xfa)
    call this%cal_path_bcm_fx(this%path_x,this%path_fx,this%path_flow)
    !this%ncperr = this%getncperr(this%x,this%xfa,this%fx,this%logitprob)
    this%ncperr = this%get_path_bcm_ncperr()
    end subroutine

    !pf: path flow
    !lkf: link flow
    subroutine update_linkflow_from_path(pf,lkf,pathprob,demand)
    use constpara
    implicit none 
    integer::p,l, linkid 
    real*8::lkf(nl),pf(npath)
    real*8::pathprob(npath) 
    real*8::demand
    do p = 1, npath
        pf(p) = demand*pathprob(p)
    end do 
    
    
    lkf = 0
    do p = 1, npath
      do l = 1, 4
        linkid = path_link(p,l)  
        if (linkid.gt.0) then 
            lkf(linkid) = lkf(linkid) + pf(p)
        end if 
      end do 
    enddo


    end subroutine


    subroutine init_path_flow(this)
    implicit none 
    class(pbcmsolver)::this
    integer::i,o,root,nr,node,arc
    integer::p, l, linkid
    this%nwk%subnode = .true.
    this%nwk%sublink = .true.
    this%path_x = 0.0
    this%lf = 0

    ! Assign flow to the average demand
    this%path_flow(1,1) = this%nwk%demand(1)/4
    this%path_flow(2,1) = this%nwk%demand(1)/4
    this%path_flow(3,1) = this%nwk%demand(1)/4
    this%path_flow(4,1) = this%nwk%demand(1)/4
    do p = 1, npath
        !this%path_flow(p,1) = this%nwk%demand(1)/npath
        this%path_x(p,1) = this%path_flow(p,1)/this%nwk%demand(1)
    end do
    

    
    end subroutine

    ! compute mapping function given link and path data

    subroutine cal_path_bcm_fx(this,x,fx,pf)
    use constpara
    implicit none 
    class(pbcmsolver)::this
    integer::i,j,node,nr,link,ll
    real*8::lamda
    real*8::x(npath,ndest), fx(npath,ndest),pf(npath)
    real*8::expsum
    real*8::bcm
    real*8::val
    real*8::temp_min
    real*8::bcmvalue
    real*8::u_sl
    integer::num_bcm_link,bl
    integer::numupnode, unode,unn  ! number of updated nodes 
    integer::p,linkid,l


    call update_linkflow_from_path(pf, this%lf,x,this%nwk%demand(1))

    call this%nwk%link_time(this%lf,this%lt)

    ! step 0: update path cost 
    this%path_cost = 0
    do p = 1,npath
      do l = 1, 4
        linkid = path_link(p,l)  
        if (linkid.gt.0) then 
            this%path_cost(p) = this%path_cost(p) + this%lt(linkid) 
        end if 
      end do 
    enddo


    ! step 1 compute ndist under bcm

    do nr = 1, ndest
        this%nwk%ndist=large
        this%nwk%ndist(this%nwk%roots(nr),nr) = 0.0
        temp_min = large
        do p = 1, npath
            temp_min = min(temp_min, this%path_cost(p))
        enddo 

        bcmvalue = get_bcmval(temp_min)

        do p =1, npath
            this%path_cost_wav(p)=this%path_cost(p)-temp_min-bcmvalue
        enddo

        expsum = 0
        do p = 1, npath
          if (this%path_cost_wav(p).le.0) then 
            val = -theta*(this%path_cost_wav(p))
            expsum =expsum +exp(val) - 1
          endif 
        enddo 

        lamda = (1/theta)*log(this%nwk%demand(1)/expsum)
        if (lamda.le.0.0) then 
          write(*,*) " lamada is .le. 0, val = ", lamda
        end if

        do p = 1, npath
            if (this%path_cost_wav(p).le.0) then 
                ! it is ok to add 
                fx(p,nr) =  this%path_cost_wav(p)+&
                    + (1/theta)*log(max(pf(p),0.00000001)+exp(theta*lamda))
            else 
                fx(p,nr) = large + 1
            endif 
        end do

        do p = 1, npath
            if (this%path_cost_wav(p).le.0) then 
                this%path_bcm_logit(p) = (exp(-theta*(this%path_cost_wav(p)))-1)/expsum
            else
                this%path_bcm_logit(p) = 0
            end if
        end do
    enddo

    
    this%ncperr = this%get_path_bcm_ncperr()
    this%solc =  this%solc + 1
    write(this%gapfileno,'(i4,a,f14.6)') this%solc,",",this%ncperr
    write(*,'(i4,a,f14.6)') this%solc,",",this%ncperr

    end subroutine



    function get_path_bcm_ncperr(this) result (madf)
    implicit none 
    class(pbcmsolver)::this
    real*8::madf,minfx,dmax1,dabs
    integer::nr,p

    madf = 0.0d0 
    do nr = 1,ndest
        minfx = large
        do p = 1, npath
            if (this%path_fx(p,nr).le.minfx) then 
              minfx = this%path_fx(p,nr)
            end if 
        end do
        do p =1, npath
          if (this%path_x(p,nr).gt.ncp_flow_eps) then 
            madf = dmax1(dabs(minfx - this%path_fx(p,nr)),madf)
           end if
        end do 
    end do
    if (madf.lt.ncp_eps) then 
        this%isNCPconverge =  .true.
    else 
        this%isNCPconverge =  .false.
    end if 

    end function

         
    subroutine path_projection(x1,x0,fx0,alph,nwk)
    use constpara
    use GraphLib
    implicit none
    real*8,intent(out)::x1(npath,ndest)
    real*8,intent(in)::x0(npath,ndest)
    real*8,intent(in)::fx0(npath,ndest)
    type(graphclass),intent(in)::nwk
    integer,parameter::max_out_links=10
    integer::i,n,j,nr,node,p
    real*8::alph
    real*8::proj_vector(max_out_links),proj_result(max_out_links)
    real*8::const=1.0
    ! find max out links
    piter=piter+1
    x1=0.0
    do nr=1,ndest
        proj_vector=0.0
        proj_result=0.0
        n = 0
        do p =1, npath
          n = n + 1 
          proj_vector(n) = x0(p,nr)-alph*fx0(p,nr)
        end do
        call simplex_projection(proj_result,proj_vector,const,n)
        n = 0
        do p = 1, npath
           n=n+1 
           x1(p,nr)=proj_result(n)
        end do
    end do
    return
    end subroutine

    subroutine path_bcm_outputx(this)
    use constpara
    implicit none
    class(pbcmsolver)::this
    integer::i,j,nr,n,p
    logical::isused
    real*8:: largecost
    real*8:: printfx
    largecost = 1000.0

    open(1,file="c:\gitcodes\LogitAssign\results\path_bcm.txt")
   
    write(1,"(a3,a5,a5,a2,a6,a5)") "id,","cost,","flow,","x,","mapfx,","logit"
    do p=1, npath
       write(1,"(i3,a,f6.2,a,f6.2,a,f8.6,a,f8.2,a,f8.6)") p,",",this%path_cost(p),",",this%path_flow(p,1),",", &
        this%path_x(p,1),",", this%path_fx(p,1),",",this%path_bcm_logit(p)
    end do

    close(1)


    
    open(1,file='c:\gitcodes\LogitAssign\results\fortran_output_link.txt',status='old',position='append')
    ! write(1,*) "method,case,dest,link,flow,fx,lt,xprob,logitprob,tail,head"
    do i=1,ndest
        do j=1,nl
            ! if (this%xfa(j,i)/=0.0) then
                write(1,"(a5,a,i3,a,i2,a,i3,a,f6.2,a,f7.2,a,f6.2,a,f6.4,a,f6.4,a,i3,a,i3)") &
                    this%name,',',caseindex,',', i,',',j,',',this%xfa(j,i),',',dmin1(largecost,this%fx(j,i)), ',', &
                    this%lt(j),',',this%x(j,i),',', this%logitprob(j,i),',',this%nwk%anode(j),',',this%nwk%bnode(j)
            ! endif
        enddo
    end do
    close(1)

    open(1,file='c:\GitCodes\LogitAssign\results\fortran_output_node.txt',position='append')
    ! write(1,*) "method,case,dest,node,fout,lout,label"
    do nr = 1, ndest
        do n = 1, nn
          isused = .false.
          do j = this%nwk%firstout(n), this%nwk%lastout(n)
            if (this%xfa(j,nr).gt.ncp_flow_eps) then 
              isused = .true.
            end if 
          end do 
          if (isused) then 
            write(1,"(a5,a,i3,a,i2,a,i3,a,i3,a,i3,a,f6.2)") &
             this%name,',',caseindex,',',nr,',',n,',',this%nwk%firstout(n),',',this%nwk%lastout(n),',',this%nwk%ndist(n,nr)
          end if
        enddo 
    end do 

    close(1)

    end subroutine






    end module 

    real*8 function path_norm_value(v1,v2,n1,n2)
    implicit none
    integer,intent(in)::n1,n2
    real*8,dimension(n1,n2)::v1,v2
    real*8:: results
    integer::i,j
    results=0.0
    do i=1,n1
        do j=1,n2
        results=results+(v1(i,j)-v2(i,j))**2
        end do 
    end do 
    path_norm_value=sqrt(results)
    return

    end function
     
    real*8 function path_update_alph(eu,du,beta,lama)
    use constpara
    implicit none
    real*8::eu(npath,ndest),du(npath,ndest),beta,lama
    integer i,j
    real*8::up,down
    real*8::norm_value0
    up=0.0
    down=0.0
    do i=1,ndest
        do j=1,npath
            up=up+eu(j,i)*du(j,i)
        end do
    end do
    down=norm_value0(du,nl,ndest)
    down=down**2
    path_update_alph=lama*beta*up/down

    return
    end function
