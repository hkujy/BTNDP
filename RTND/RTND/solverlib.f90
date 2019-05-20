
    module SolverLib
    use constpara
    use graphlib
    implicit none
    type,public::methods
        character(len = 10)::name
        type(graphclass)::nwk
        real*8::x(nl,ndest) ! percentage
        real*8::fx(nl,ndest)
        real*8::xfa(nl,ndest)
        real*8::logitprob(nl,ndest)  ! logit probability
        real*8::ncperr,cputime
        real*8::lf(nl)
        real*8::lt(nl)
        real*8::nf(nn,ndest)
        ! real*8::stt(nl)  ! section travel time
        real*8::node_exp_sum(nn,ndest)
        logical::isNCPconverge
        integer::solc
        integer::tests
        integer::gapfileno
    contains
      !procedure,pass::minsp=>method_init_arc_flow
      procedure,pass::initial_x=>initial_x
      procedure,pass::forward_update_flow=>forward_update_flow
      procedure,pass::backward_update_fx=>backward_update_fx
      procedure,pass::getncperr=>getncperr
      procedure,pass::outputx=>outputx
      procedure,pass::outputod=>outputod
      procedure,pass::init_arc_flow=>init_arc_flow
      procedure,pass::node_flow=>node_flow
      procedure,pass::cal_fx=>cal_fx
      procedure,pass::cal_bcm_fx=>cal_bcm_fx
      procedure,pass::geninisol=>geninisol
      procedure,nopass::get_bcmval=>get_bcmval
    end type methods
    contains
    
   function get_bcmval(baseval) result(res)

    reaL*8::res,baseval
    if (isConstBcm) then 
        res = 10
    else 
       res =  baseval*bcmratio
    endif 
    
   return
   end function
    
    
    
    subroutine geninisol(this)
    implicit none
    CLASS(methods)::this
    ! read network -> create topological order -> check connectivity
    ! compute inital x and intial y
    call this%nwk%readnwt
    call this%nwk%minspantree
    call this%init_arc_flow
    call this%nwk%getorder
    call this%nwk%countconect
    call this%initial_x
    call this%forward_update_flow(this%x)
    call this%nwk%link_time(this%lf,this%lt)
    call this%nwk%updatesub(this%lt,.false.,this%xfa)
    call this%backward_update_fx(this%fx,this%logitprob)
    this%ncperr = this%getncperr(this%x,this%xfa,this%fx,this%logitprob)
    
    return 
    end subroutine



  
    
    subroutine iniassignflow(this)
    implicit none 
    class(methods)::this 
	
    call this%nwk%minspantree
    call this%init_arc_flow

    return
    end subroutine
    
    subroutine node_flow(this)
    ! given arf flow get node flow
    use constpara
    implicit none
    class(methods)::this
    integer::l,nr

    this%nf=0.0
    do nr=1,ndest
        do l =1,nl
            this%nf(this%nwk%anode(l),nr) = this%nf(this%nwk%anode(l),nr)+this%xfa(l,nr)
        end do
    end do

    return
    end subroutine
    
      
    !Solution class
    subroutine initial_x(this)
    use constpara
    implicit none
    class(methods)::this
    integer::i,l,node
    integer::lcount

    this%x=0.0
    call this%node_flow  	! intial node flow
    do i = 1, ndest
        do l = 1, nl
            node = this%nwk%anode(l)
            if (this%nf(node,i).ne.0.0) then 
                this%x(l,i) = this%xfa(l,i)/this%nf(node,i)
            end if 
        end do 
    end do 

    do i = 1, ndest
        do node =1, nn
            if (this%nwk%subnode(node,i)) then
                if (this%nf(node,i).eq.0.0) then 
                    lcount = 0
                    do l = this%nwk%firstout(node),this%nwk%lastout(node)
                        if(this%nwk%sublink(l,i)) then
                            this%x(l,i) = 1.0
                            exit
                        end if 
                    end do 
                endif
            end if 
        end do 
    end do 

    if (islogit) then 
        do i = 1, ndest
            do node =1, nn
                if (this%nwk%subnode(node,i)) then
                    lcount = 0
                    do l = this%nwk%firstout(node),this%nwk%lastout(node)	
                        if (this%nwk%sublink(l,i)) then 
                            lcount=lcount + 1 
                        end if 
                    end do 
                    do l = this%nwk%firstout(node),this%nwk%lastout(node)	
                        if(this%nwk%sublink(l,i)) then 
                            this%x(l,i)=1.0/lcount 
                        end if 
                    end do 
                end if 
            end do 
        end do 
    end if 
    return 
    end subroutine

    subroutine backward_update_fx(this,fx1,logitprob)
    use constpara
    implicit none 
	! tail(a)--------->head(b)
    class(methods)::this
    real*8,intent(out)::fx1(nl,ndest)
    integer::i,j,node,nr,link,ll
    real*8,optional::logitprob(nl,ndest)
    !update the label on the subnetwork	
    this%node_exp_sum = 0

    if (isusebcm) then 
        goto 66
    end if 
    if (.not.islogit) then
        fx1=large
        this%nwk%ndist = large
        do nr = 1,ndest
            this%nwk%ndist(this%nwk%roots(nr),nr) = 0.0
            do i = nn,1,-1
                node = this%nwk%torder(i,nr)
                if ((node.ne.0).and.this%nwk%subnode(node,nr)) then 
                    do j=this%nwk%firstin(node),this%nwk%lastin(node)
                        link = this%nwk%backtoforward(j)
                        if (this%nwk%sublink(link,nr)) then 
                            if (this%nwk%ndist(this%nwk%bnode(link),nr) + this%lt(link).lt. &
                                this%nwk%ndist(this%nwk%anode(link),nr)) then
                                this%nwk%ndist(this%nwk%anode(link),nr) &
                                =  this%nwk%ndist(this%nwk%bnode(link),nr)+this%lt(link) 
                            end if 
                            fx1(link,nr) = this%nwk%ndist(this%nwk%bnode(link),nr)+this%lt(link)
                        end if 
                    end do 
                end if
            end do 
        end do 
     else ! else if logit model
        fx1 = large
        ! TODO
        this%nwk%ndist = 0		
        !TODO "solver bwd check ndist label large or zero"
        do nr = 1, ndest
            this%nwk%ndist(this%nwk%roots(nr),nr) = 0.0
            do i = nn,1,-1
                node = this%nwk%torder(i,nr)
                if (node.eq.0) then 
                    continue
                end if 
                if (this%nwk%subnode(node,nr)) then 
                    do j = this%nwk%firstin(node),this%nwk%lastin(node)
                        link = this%nwk%backtoforward(j) 
                        !write(*,*) "link=,",link,"anode=",anode(link) 
                        if((this%x(link,nr).gt.0).and.(this%lf(link).gt.0)) then
                            this%node_exp_sum(this%nwk%anode(link),nr) = this%node_exp_sum(this%nwk%anode(link),nr) &
                                + exp((-theta)*(this%lt(link) &
                                + this%nwk%ndist(this%nwk%bnode(link),nr)))
                        end if 
                        if (this%node_exp_sum(this%nwk%anode(link),nr).lt.0) then 
                            write(*,*) "node = ",this%nwk%anode(link)," link = ", link
                            write(*,*) "expvalue = ",exp((-theta)*(this%lt(link)+this%nwk%ndist(this%nwk%bnode(link),nr)))
                            write(*,*) "backward: expum is less than 0" 
                        endif 
                        this%nwk%ndist(this%nwk%anode(link),nr) = &
                            (-1/theta)*log(this%node_exp_sum(this%nwk%anode(link),nr)) 
                            !(-1/theta)*log(max(this%node_exp_sum(this%nwk%anode(link),nr),zero))
                        if (this%lf(link).eq.0) then 
                            fx1(link,nr) = this%nwk%ndist(this%nwk%bnode(link),nr) &
                            + this%lt(link) + 1/theta
                        else 
                            fx1(link,nr) = this%nwk%ndist(this%nwk%bnode(link),nr) &
                            + this%lt(link)+(1+log(this%lf(link)))/theta 
                        end if
                        do ll = this%nwk%firstout(this%nwk%anode(link)),this%nwk%lastout(this%nwk%anode(link))
                            if((this%x(ll,nr).gt.0).and.(this%lf(ll).gt.0)) then 
                                logitprob(ll,nr) = exp((-theta)*(this%lt(ll) &
                                + this%nwk%ndist(this%nwk%bnode(ll),nr)))/this%node_exp_sum(this%nwk%anode(link),nr) 
                            end if 
                        end do 
                    end do 
                end if 
            end do
        end do
     endif 
    
    
66  if (isusebcm) then 
        call this%cal_bcm_fx(fx1,logitprob)
    end if
    
    return
    end subroutine 

    ! this subroutine is used to update the flow for each node
    ! input is the flow proportion 
    ! out put is the link flow and node flow
    ! Algorithm
    subroutine forward_update_flow(this,x0)
    use constpara
    implicit none 
    class(methods)::this 
    real*8,intent(in)::x0(nl,ndest)
    integer i,j,nr,node,link,o,d

    this%xfa = 0.0
    this%nf = 0.0 
    
    do i=1,nod
        o = this%nwk%origin(i)
        d = this%nwk%dest(i)
        do j=1,ndest
            if (this%nwk%roots(j)==d) then 
                nr =j
                exit
            end if 
        enddo  
        this%nf(o,nr) = this%nf(o,nr) + this%nwk%demand(i)
    end do 

    do nr = 1,ndest
        do i = 1,nn
            node = this%nwk%torder(i,nr)
            if (node.ne.0) then 
                do j = this%nwk%firstout(node),this%nwk%lastout(node)
                    link = j
                    if(this%nwk%sublink(link,nr).and.this%nwk%subnode(this%nwk%bnode(link),nr)) then 
                        this%xfa(link,nr) = this%nf(this%nwk%anode(link),nr)*x0(link,nr)
                        this%nf(this%nwk%bnode(link),nr)  &
                        = this%nf(this%nwk%bnode(link),nr) + this%xfa(link,nr)
                    end if 
                end do 
            end if 
        enddo 
    enddo

    forall (i=1:nl)
        this%lf(i)=sum(this%xfa(i,:))
    end forall 

    end subroutine 
	
!this contains two subroutine to measure the error
    !use constpara
    function getncperr(this,x,xfa,fx,logitprob) result (madf)
    use constpara
    implicit none
    class(methods)::this
    real*8::x(nl,ndest),xfa(nl,ndest),fx(nl,ndest)
    real*8,optional::logitprob(nl,ndest)
    real*8::madf
    integer::i,nr,node,j, link, tail, head
    real*8::lf(nl)
    real*8::nodefx(nn,ndest)
    real*8::lamda, mincost,minfx,bcm
    integer::bcmcount
    logical::isbcm(5)
    logical::isupdated(nn,ndest)
    open(1,file='c:\gitcodes\BTNDP\results\fortran_checkmadf.txt',position="append") 
    madf = 0.0d0 
    ! Todo: Check Whether i need to update xfa 
    if (islogit) then 
        nodefx = large
        isupdated = .false.
        do nr =1, ndest
          do i = 1,nl 
            if (this%nwk%sublink(i,nr).and.this%nwk%subnode(this%nwk%anode(i),nr).and.this%nwk%subnode(this%nwk%bnode(i),nr)) then 
                if (this%nwk%anode(i)/=this%nwk%roots(nr)) then 
                  node =  this%nwk%anode(i)
                  if (isupdated(node,nr)) then 
                    continue
                  endif 
                  do j = this%nwk%firstout(node), this%nwk%lastout(node)
                    if(xfa(j,nr).gt.ncp_flow_eps) then 
                      nodefx(node,nr) = min(fx(j,nr),nodefx(node,nr))
                    end if 
                  enddo
                   isupdated(node,nr) = .true.
                end if 
            end if 
          end do
        enddo
    end if
    do nr =1, ndest
        do i = 1,nl 
            if (this%nwk%sublink(i,nr).and.this%nwk%subnode(this%nwk%anode(i),nr).and.this%nwk%subnode(this%nwk%bnode(i),nr)) then 
                if (this%nwk%anode(i)/=this%nwk%roots(nr)) then 
                    if (islogit) then 
                        if (xfa(i,nr).gt.ncp_flow_eps) then 
                          madf = max(madf,fx(i,nr)-nodefx(this%nwk%anode(i),nr))
                        endif
                        !end if 
                    else
                        if (xfa(i,nr).gt.0.1.and.(fx(i,nr)-this%nwk%ndist(this%nwk%anode(i),nr))>madf) then 
                            madf = fx(i,nr) - this%nwk%ndist(this%nwk%anode(i),nr) 
                            write(1,'(i3,a,i5,a,i5,a,i5,a,f6.2,a,f6.2,a,f6.2,a,f6.2)') & 
                                caseindex,',',i,',',this%nwk%anode(i),',',nr,',',xfa(i,nr),',',fx(i,nr),',', &
                                this%nwk%ndist(this%nwk%anode(i),nr),',',madf 
                        end if 
                    endif   ! if to check whether it is logit 
                endif
            endif
        end do 
    end do 
 
    if(isusebcm) then 
        madf = 0.0d0 
        do nr = 1,ndest
            do i = 1,nn
                node = this%nwk%torder(i,nr)
                if (node.ne.0) then 
                ! step 1: find the minimum cost
                    mincost = large
                    minfx = large
                    do j = this%nwk%firstout(node),this%nwk%lastout(node)
                        link = j
                        tail = this%nwk%anode(link)
                        head = this%nwk%bnode(link)
                        if(this%nwk%sublink(link,nr).and.this%nwk%subnode(this%nwk%bnode(link),nr)) then 
                            if (this%lt(link)+this%nwk%ndist(head,nr).lt.mincost) then 
                                mincost = this%lt(link) + this%nwk%ndist(head,nr)
                            end if 
                        end if
                    end do 
                    !bcm = mincost*bcmratio
                    bcm = get_bcmval(mincost)
                    !bcm = 5
                ! step 2: add bcm links
                    isbcm = .false.
                    bcmcount = 1
                    do j = this%nwk%firstout(node),this%nwk%lastout(node)
                        link = j
                        tail = this%nwk%anode(link)
                        head = this%nwk%bnode(link)
                        if(this%nwk%sublink(link,nr).and.this%nwk%subnode(this%nwk%bnode(link),nr)) then 
                            if ((this%lt(link) + this%nwk%ndist(head,nr)).lt.(mincost + bcm)) then 
                                isbcm(bcmcount) = .true.
                                if (minfx.gt.fx(link,nr)) then 
                                    minfx =  fx(link,nr)
                                endif 
                            end if 
                        end if
                        bcmcount = bcmcount + 1 
                    end do 
                !step 3: compute gap w.r.t. the minfx
                    bcmcount = 1
                    do j = this%nwk%firstout(node),this%nwk%lastout(node)
                        link = j
                        tail = this%nwk%anode(link)
                        head = this%nwk%bnode(link)
                        if(this%nwk%sublink(link,nr).and.this%nwk%subnode(this%nwk%bnode(link),nr)) then 
                            if (isbcm(bcmcount)) then 
                                if (xfa(link,nr).gt.ncp_flow_eps) then 
                                    madf = max(abs(minfx - fx(link,nr)),madf)
                                end if
                                !write(*,*) "madf = ",madf, "fx = ",fx(link,nr)," minfx = ", minfx
                            end if
                        end if
                        bcmcount = bcmcount + 1 
                    end do 
                end if
            end do
        end do 
    end if

    if (madf.lt.ncp_eps) then 
        this%isNCPconverge =  .true.
    else 
        this%isNCPconverge =  .false.
    end if 
               
    close(1)
    end function 

    ! TODO: add to change to logit model    
 !   subroutine error_euclidean_distance(f1,f0,n1,n2,error)
 !   implicit none
 !
 !   integer,intent(in)::n1,n2
 !   real*8,dimension(n1,n2),intent(in)::f0,f1
 !   real*8::error
 !   integer i,j
 !   real*8::sum
 !   sum = 0.0 
 !   error=0.0
	!	do i=1,n1
	!		do j=1,n2
	!			error=error+abs(f1(i,j)-f0(i,j))
 !       sum=sum+f0(i,j)
	!		end do 
	!	enddo 
	!	error=error/sum
	!!	write(*,*) "err = ", error
	!	return 
	!	end subroutine 

    
    ! out link flow
    subroutine outputx(this)
    use constpara
    implicit none
    class(methods)::this
    integer::i,j,nr,n
    logical::isused
    real*8:: largecost
    real*8:: printfx
    largecost = 1000.0

    open(1,file='c:\gitcodes\BTNDP\results\fortran_output_link.txt')
    write(1,*) "method,case,dest,link,flow,fx,lt,xprob,logitprob,tail,head"
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

    open(1,file='c:\GitCodes\LogitAssign\Results\fortran_output_node.txt')
    
    write(1,*) "method,case,dest,node,fout,lout,label"
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

    subroutine outputod(this,flow,y)
    use constpara
    implicit none
    class(methods)::this
    integer i,j
    real*8::flow(nl,ndest),y(nl,ndest)
    integer q,w

    !open(39,file='..\..\results\fortran_outputod.txt',status='old', position='append' )
    open(1,file='c:\gitcodes\BTNDP\results\fortran_outputod.txt')
    write(1,*) "case","origin","dest","demand","y","flow"
    !integer::dest(nod),origin(nod)
    do q = 1, nod
        do w=1,ndest
            if (this%nwk%roots(w).eq.this%nwk%dest(q)) then
                do j = 1, nl
                    if (this%nwk%anode(j).eq.this%nwk%origin(q)) then
                        !write(39,"(i2,1x,i3,1x,1x,f7.2)") origin(q),dest(q),y(j,w)
                        if (flow(j,w)>0.0001) then
                            !write(39,"(i2,1x,i3,1x,f6.2,1x,f7.2)") origin(q),dest(q),demand(q),y(j,w)
                            !write(39,"(i2,a,i3,a,f6.2,a,f7.2)") origin(q),',',dest(q),',', demand(q),',',y(j,w)
                            write(1,"(i3,a,i2,a,i3,a,f8.4,a,f8.4,a,f8.4)") &
                                caseindex,',',this%nwk%origin(q),',',this%nwk%dest(q),',',this%nwk%demand(q),',',y(j,w),',',flow(j,w)
                            exit
                        endif
                    endif
                enddo
            endif
        enddo
    enddo

    close(1)

    end subroutine


    subroutine method_outputpath(this,linktime, xfa)
    use constpara
    implicit none
    class(methods)::this
    integer i,j
    real*8::linktime(nl)
    real*8,intent(in)::xfa(nl,ndest)

    open(1,file='c:\gitcodes\BTNDP\results\fortran_linkcost.txt' )
    open(2,file='c:\gitcodes\BTNDP\results\fortran_pathcost.txt' ) 
    
    write(1,*) "case,linkid,linktime"
    do i = 1, nl
        write(1,'(i2,a,i3,a,f8.4)') caseindex,',',i,',',linktime(i)
    enddo

    close(1)


    write(2,*) "case,x,approchtime" 
    ! the second demmension of the xfa is the destination index
    ! the first three paths blong to od pair 1-4
    write(2,'(i3,a,f8.4,a,f8.4)') caseindex,',',xfa(3,1),',',linktime(3)
    write(2,'(i3,a,f8.4,a,f8.4)') caseindex,',',xfa(2,1),',',linktime(2)+linktime(6)
    write(2,'(i3,a,f8.4,a,f8.4)') caseindex,',',xfa(1,1),',',linktime(1)+linktime(4)+linktime(6)
    ! the following two belongs to od pair 2-4
    write(2,'(i3,a,f8.4,a,f8.4)') caseindex,',',xfa(4,1),',',linktime(4)+linktime(6)
    write(2,'(i3,a,f8.4,a,f8.4)') caseindex,',',xfa(5,1),',',linktime(5)
    ! the last belong to od pair  3-4
    write(2,'(i3,a,f8.4,a,f8.4)') caseindex,',',xfa(6,1),',',linktime(6)

    close(2)

    end subroutine


    !
    !subroutine outputpara
    !use graph
    !implicit none
    !integer i
    !
    !open(1,file='c:\gitcodes\logitassign\results\fortran_6linkdata.txt')
    !write(1,*) 'scost,svar,fare'
    !do i = 1, 6
    !    write (1,'(f6.2,a,f6.2,a,f6.2)') scost(i),',', svar(i),',',fare(i)
    !enddo
    !close(1)
    !
    !
    !!open(1,file='..\..\results\fortran_linecostandvar.txt')
    !!write(1,'(f6.2,a,f6.2)') tsl(1,1),',',tsl(1,2)  ! line 1
    !!write(1,'(f6.2,a,f6.2)') tsl(7,1),',',tsl(7,2)  ! line 2
    !!write(1,'(f6.2,a,f6.2)') tsl(8,1),',',tsl(8,2)  ! line 3
    !!write(1,'(f6.2,a,f6.2)') tsl(6,1),',',tsl(6,2)  ! line 4
    !!close(1)
    !
    !end subroutine

    subroutine cal_fx(this,x,fx)
    ! x => fx
    use constpara
    implicit none
    class(methods)::this
    real*8,intent(out)::fx(nl,ndest)
    real*8,intent(in)::x(nl,ndest)
    real*8::st,et
    call cpu_time(st)
    this%solc =  this%solc + 1 
    call this%forward_update_flow(x)
    ! call this%nwk%link_time(this%lf,this%stt)
    call this%nwk%link_time(this%lf,this%lt)
    call this%backward_update_fx(fx,this%logitprob)

    this%ncperr = this%getncperr(x,this%xfa,fx,this%logitprob)

    call cpu_time(et)

    write(*,'(i4,a,f14.6,a,f12.4)')  this%solc,',',this%ncperr,',',et-st
    write(this%gapfileno,'(i4,a,f14.6)') this%solc,",",this%ncperr

    return 
    end subroutine
    
    
    subroutine init_arc_flow(this)
    implicit none 
    class(methods)::this
    integer::i,o,root,nr,node,arc

    if (islogit) then 
        this%nwk%subnode = .true.
        this%nwk%sublink = .true.
        this%xfa=0.0
        do nr =1, ndest
            root = this%nwk%roots(nr)
            do i = 1,nod
                o = this%nwk%origin(i)
                node = o
                do while (node.ne.root)
                    arc = this%nwk%pa(node)	
                    this%nwk%sublink(arc,nr)=.true.
                    node = this%nwk%backanode(arc)	
                    arc = this%nwk%backtoforward(arc)
                    this%xfa(arc,nr) = this%xfa(arc,nr) + this%nwk%demand(i)
                end do 
            enddo 
        end do 
    else
        this%nwk%subnode = .false.
        this%nwk%sublink = .false.
        this%xfa=0.0
    ! Minimum spanning tree
        do nr =1,ndest
            root = this%nwk%roots(nr)
            do i = 1, nn
                if (this%nwk%pa(i).ne.0) then 
                    arc = this%nwk%pa(i)
                    arc = this%nwk%backtoforward(arc)
                    this%nwk%sublink(arc,nr)=.true.
                    this%nwk%subnode(this%nwk%anode(arc),nr)=.true.
                    this%nwk%subnode(this%nwk%bnode(arc),nr)=.true.
                end if 
            end do
            do i=1,nod
                o=this%nwk%origin(i)
                node=o
                do while (node.ne.root)
                    arc = this%nwk%pa(node)	
                    this%nwk%sublink(arc,nr)=.true.
                    node = this%nwk%backanode(arc)	
                    arc = this%nwk%backtoforward(arc)
                    this%xfa(arc,nr) = this%xfa(arc,nr) + this%nwk%demand(i)
                end do 
            enddo 
        end do 
    end if 

    end subroutine

    subroutine cal_bcm_fx(this,fx1,logitprob)
    use constpara
    implicit none 
    class(methods)::this
    integer::i,j,node,nr,link,ll
    real*8::logitprob(nl,ndest)
    real*8::lamda(nn,ndest)
    real*8::expsum
    real*8::mincost
    integer:: tail, head
    integer:: bcmcount
    real*8::bcm
    real*8::val
    logical:: isbcm(5) 
    real*8::fx1(nl,ndest)
    real*8::temp_min(nl),bcmvalue
    integer::num_bcm_link,bl
    INTEGER::upnodes(10) ! update nodes, maxmum number of incomling links
    integer::numupnode, unode,unn  ! number of updated nodes 
    ! step 1 compute ndist under bcm
    do nr = 1, ndest
        this%nwk%ndist=large
        this%nwk%ndist(this%nwk%roots(nr),nr) = 0.0
        temp_min = large
        temp_min(this%nwk%roots(nr)) = 0
        do i = nn,1,-1
            node = this%nwk%torder(i,nr)
            if (node.eq.0) then 
                continue
            end if 
            if (this%nwk%subnode(node,nr)) then 
                numupnode = 0
                upnodes = 0 
                do j = this%nwk%firstin(node),this%nwk%lastin(node)
                    link = this%nwk%backtoforward(j) 
                    tail = this%nwk%anode(link)                    
                    head = node
                    if (this%lt(link) + this%nwk%ndist(head,nr) < temp_min(tail)) then 
                        temp_min(tail) = this%lt(link) + this%nwk%ndist(head,nr)  
                    end if
                    numupnode =  numupnode +  1
                    upnodes(numupnode)  = tail
                end do 
                do unn = 1, numupnode
                    tail = upnodes(unn)
                    !bcmvalue = temp_min(tail)*bcmratio
                    bcmvalue = get_bcmval(temp_min(tail))
                    this%node_exp_sum(tail, nr) = 0 
                    do bl = this%nwk%firstout(tail), this%nwk%lastout(tail)
                        if ((this%lt(bl) + this%nwk%ndist(this%nwk%bnode(bl),nr)).lt.temp_min(tail) + bcmvalue) then 
                            this%node_exp_sum(tail,nr) = this%node_exp_sum(tail,nr)-1&
                                    + exp((-theta)*(this%lt(bl) &
                                    + this%nwk%ndist(this%nwk%bnode(bl),nr)-temp_min(tail)-bcmvalue))
                        endif
                    end do 
                    this%nwk%ndist(tail,nr) = &
                            !(-1/theta)*log(max(this%node_exp_sum(tail,nr),zero))
                            (-1/theta)*log(this%node_exp_sum(tail,nr))
                    
                    if (this%node_exp_sum(this%nwk%anode(link),nr).lt.0) then 
                            write(*,*) "node = ",this%nwk%anode(link)," link = ", link
                            write(*,*) "expvalue = ",exp((-theta)*(this%lt(link)+this%nwk%ndist(this%nwk%bnode(link),nr)))
                            write(*,*) "backward: expum is less than 0" 
                    endif 
                end do
            endif
        end do 
    end do
    
               

    ! step compute lameda s
    do nr = 1,ndest
        do i = 1,nn
            node = this%nwk%torder(i,nr)
            if (node.ne.0) then 
                ! step 1: find the minimum cost
                mincost = large
                do j = this%nwk%firstout(node),this%nwk%lastout(node)
                    link = j
                    tail = this%nwk%anode(link)
                    head = this%nwk%bnode(link)
                    if(this%nwk%sublink(link,nr).and.this%nwk%subnode(this%nwk%bnode(link),nr)) then 
                        if (this%lt(link)+this%nwk%ndist(head,nr).lt.mincost) then 
                            mincost = this%lt(link) + this%nwk%ndist(head,nr)
                        end if 
                    end if
                end do 
                !bcm = mincost*bcmratio
                
                bcm = get_bcmval(mincost)
                ! step 2: add bcm links
                isbcm = .false.
                bcmcount = 1
                expsum = 0 
                do j = this%nwk%firstout(node),this%nwk%lastout(node)
                    link = j
                    tail = this%nwk%anode(link)
                    head = this%nwk%bnode(link)
                    if(this%nwk%sublink(link,nr).and.this%nwk%subnode(this%nwk%bnode(link),nr)) then 
                      if ((this%lt(link) + this%nwk%ndist(head,nr)).lt.mincost + bcm) then 
                        isbcm(bcmcount) = .true.
                        val = -theta*(this%lt(link)+this%nwk%ndist(head,nr)-mincost-bcm)
                        !expsum =expsum +exp(max(val,0.0)) - 1
                        expsum =expsum +exp(val) - 1
                        !expsum =  expsum + exp(-theta*(this%lt(link)+this%nwk%ndist(head,nr)-mincost-bcm))-1
                      end if
                    endif 
                    bcmcount = bcmcount + 1 
                end do 
                !write(*,*) "nf = ",this%nf(node,nr),"expsu = ", expsum
                lamda(node,nr) = (1/theta)*(log(this%nf(node,nr)/expsum))
                lamda(node,nr) =  max(lamda(node,nr),0.0)
                !write(*,*) "lamda = ",lamda(node,nr)
                !step 3: compute update mapping function fx
                
                bcmcount = 1
                do j = this%nwk%firstout(node),this%nwk%lastout(node)
                    link = j
                    tail = this%nwk%anode(link)
                    head = this%nwk%bnode(link)
                    !bcm = temp_min(tail)*bcmratio
                    bcm = get_bcmval(temp_min(tail))
                    if(this%nwk%sublink(link,nr).and.this%nwk%subnode(this%nwk%bnode(link),nr)) then 
                        if (isbcm(bcmcount)) then 
                            fx1(link,nr) =  this%lt(link) + this%nwk%ndist(head,nr) - mincost - bcm &
                            + (1/theta)*log(max(this%xfa(link,nr),0.00000001)+exp(theta*lamda(node,nr)))
                            ! compute the lgit prob
                            logitprob(link,nr) = (exp((-theta)*(this%lt(link)+this%nwk%ndist(this%nwk%bnode(link),nr)  &
                            -temp_min(tail) -bcm))-1)/this%node_exp_sum(this%nwk%anode(link),nr) 
                        else

                            fx1(link,nr) = large + 1
                            logitprob(link,nr) = 0
                        end if
                    endif
                    bcmcount =  bcmcount + 1
                end do 
            end if 
        enddo 
    enddo



    end subroutine




    end module 

