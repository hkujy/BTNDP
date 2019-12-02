
    module SolverLib
    use constpara
    use graphlib
    implicit none
    type,public::methods
        character(len = 10)::name
        type(graphclass)::nwk
        real*8,allocatable::x(:,:) ! percentage, approach proportion
        real*8,allocatable::fx(:,:) !mapping function
        real*8,allocatable::xfa(:,:) ! link flow for each dest
        real*8,allocatable::logitprob(:,:)  ! logit probability
        real*8,allocatable::lf(:),lt(:)
        real*8,allocatable::nf(:,:)  ! node flow for each dest
        real*8,ALLOCATABLE::node_exp_sum(:,:)
        real*8,ALLOCATABLE::dial_link_like(:,:)
        real*8,ALLOCATABLE::dial_rlabel(:,:),dial_slabel(:,:)
        integer,ALLOCATABLE::rorder(:,:),sorder(:,:) ! ascend order
        real*8,ALLOCATABLE::dial_Wsd(:,:)
        real*8::ncperr,cputime,max_dist_gap
        logical::isNCPconverge
        integer::solc
        integer::gapfileno
    contains
      !procedure,pass::minsp=>method_init_arc_flow
      procedure,pass::initial_x=>initial_x
      procedure,pass::forward_update_flow=>forward_update_flow
      procedure,pass::backward_update_fx=>backward_update_fx
      procedure,pass::update_bush=>update_bush
      procedure,pass::getncperr=>getncperr
      procedure,pass::outputx=>outputx
      procedure,pass::outputod=>outputod
      procedure,pass::init_arc_flow=>init_arc_flow
      procedure,pass::node_flow=>node_flow
      procedure,pass::cal_fx=>cal_fx
      procedure,pass::geninisol=>geninisol
      procedure,nopass::get_bcmval=>get_bcmval
      procedure,PASS::inimethod=>inimethod
      procedure,pass::delmethod=>delmethod
      procedure,pass::dial_load_main =>dial_load_main
      procedure,pass::dial_sub_graph=>dial_sub_graph
      procedure,pass::dial_get_link_like=>dial_get_link_like
      procedure,pass::dial_foward=>dial_forward
      procedure,pass::dial_backward=>dial_backward

    end type methods
    contains

    subroutine inimethod(this)
    implicit none 
    CLASS(methods)::this
    call this%nwk%inigraph
    if(.not.allocated(this%x)) then
        allocate(this%x(nl,ndest))
        allocate(this%fx(nl,ndest))
        allocate(this%xfa(nl,ndest))
        allocate(this%logitprob(nl,ndest))
        allocate(this%lf(nl))
        allocate(this%lt(nl))
        allocate(this%nf(nn,ndest))
        allocate(this%node_exp_sum(nn,ndest))
        allocate(this%dial_link_like(nl,ndest))
        allocate(this%dial_rlabel(nn,ndest),this%dial_slabel(nn,ndest))
        allocate(this%rorder(nn,ndest),this%sorder(nn,ndest))
        allocate(this%dial_wsd(nl,ndest))
    end if
    this%x=0
    this%fx=0
    this%xfa=0
    this%logitprob = 0
    this%lf=0
    this%lt=0
    this%nf=0
    this%node_exp_sum=0
    this%dial_link_like = 0
    this%dial_Wsd = 0
    this%rorder = 0
    this%sorder = 0
 
    end subroutine

    subroutine delmethod(this)
    implicit none 
    class(methods)::this
    deallocate(this%x,this%fx,this%xfa,this%logitprob,this%lf,this%lt)
    deallocate(this%nf,this%node_exp_sum)
    deallocate(this%dial_wsd,this%dial_link_like,this%dial_rlabel,this%dial_slabel)
    DEALLOCATE(this%sorder,this%rorder)
    call this%nwk%delgraph

    end subroutine

    !TODO: check this
    function get_bcmval(baseval) result(res)
    reaL*8::res,baseval
    if (isConstBcm) then 
        res = const_bcm_value
    else 
       res =  baseval*bcmratio
    endif 
    return
    end function
   
    subroutine update_bush(this)
    implicit none 
    class(methods)::this
    call this%nwk%minspantree(this%lt)
    call this%nwk%bfs_torder
    call this%nwk%getorder
    call this%nwk%countconect
    if (islogit) then 
        call this%nwk%getsuebush
    end if 
    end subroutine

    subroutine geninisol(this,set_nwk)
    implicit none
    CLASS(methods)::this
    integer::l
    CLASS(graphclass),optional::set_nwk
    real*8::max_dist_err_2
    ! read network -> create topological order -> check connectivity
    ! compute inital x and intial y
    call this%nwk%readnwt(set_nwk)
    this%lt = this%nwk%scost
    !call this%nwk%readnwt(set_nwk,numlink=nl,numnode=nn,numline=nline,&
    !maxcomsec=maxcom,maxseclineval=maxsecline,maxlinestopval=maxlinestops)
    if (load_index.eq.1) then 
        this%lt = this%nwk%scost
        call this%dial_sub_graph
        this%nwk%torder = this%sorder
    else
        call this%update_bush
    endif 
    call this%init_arc_flow
    call this%initial_x
    call this%forward_update_flow(this%x,d1=nl,d2=ndest)
    call this%nwk%link_time(this%lf,this%lt)

    ! *******the following codes are created to compare with the stoch algorihtm 
    !this%lt = this%nwk%scost
    !this%nf = 10
    !this%x = 1
    !this%nwk%sublink(5,1)=.false.
    !this%nwk%sublink(7,1) =.false.
    !**************************************************************************
    ! call this%nwk%updatesub(this%lt,.false.,this%xfa)
    ! this%lt = this%nwk%scost
    call this%backward_update_fx(this%fx,this%logitprob,d1=nl,d2=ndest)
    
    !this%ncperr = this%getncperr(this%x,this%xfa,this%fx,this%logitprob,nl,ndest,nn)
    this%ncperr = this%getncperr(this%x,this%xfa,this%fx,this%logitprob)
    this%max_dist_gap = max_dist_err_2(this%x,this%logitprob,this%xfa,ncp_flow_eps,nl,ndest)

    if (iswriteconverge) then 
        if (isdebug) then 
            write(*,'(i4,a,f16.8,a,f16.8)')  this%solc,',',this%ncperr,',',this%max_dist_gap
        end if
        write(this%gapfileno,'(i4,a,f16.8,a,f16.8)') this%solc,",",this%ncperr,",",this%max_dist_gap
    end if 

    
    return 
    end subroutine
    
    subroutine iniassignflow(this)
    implicit none 
    class(methods)::this 
	
    call this%nwk%minspantree(this%nwk%scost)
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
    integer::i,l,node,nr
    integer::lcount

    this%x=0.0
    if (islogit) then 
        goto 10
    end if
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

10  if (islogit) then 
        do nr = 1, ndest
            do node =1, nn
                if (this%nwk%subnode(node,nr)) then
                    lcount = 0
                    do l = this%nwk%firstout(node),this%nwk%lastout(node)	
                        if (this%nwk%sublink(l,nr)) then 
                            lcount=lcount + 1 
                            !if (lcount.ge.2) then 
                            !    write(*,*) " wft:2 links"
                            !end if
                        end if 
                    end do 
                    do l = this%nwk%firstout(node),this%nwk%lastout(node)	
                        if(this%nwk%sublink(l,nr)) then 
                            this%x(l,nr)=1.0/lcount 
                        end if 
                    end do 
                end if 
            end do 
        end do 
    end if 
    return 
    end subroutine

    subroutine backward_update_fx(this,fx1,logitprob,d1,d2)
    use constpara
    implicit none 
	! tail(a)--------->head(b)
    class(methods)::this
    ! real*8,intent(out)::fx1(nl,ndest)
    integer::d1,d2
    real*8,intent(out),DIMENSION(d1,d2)::fx1
    real*8,optional,DIMENSION(d1,d2)::logitprob
    integer::i,j,node,nr,link,ll,l
    real*8::link_dest_flow
    !update the label on the subnetwork	
    logitprob = 0
    ! if (load_index.eq.1) then 
        ! if load is based on Dial's method
    !call this%nwk%BFS_torder
    !if (islogit) then
    !    call this%nwk%minspantree(this%lt)   
    !    call this%nwk%bfs_torder
    !    call this%nwk%getorder
    !    call this%nwk%getsuebush
    !end if

    ! endif
    if (.not.islogit) then
        fx1=large
        this%nwk%ndist = large
        do nr = 1,ndest
            this%nwk%ndist(this%nwk%roots(nr),nr) = 0.0
            do i = nn,1,-1
                node = this%nwk%torder(i,nr)
                if ((i.eq.nn).and.(node.ne.this%nwk%roots(nr))) then 
                    write(*,*) "Solverlib: backward update fx: check torder"
                    pause
                end if 
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
        this%nwk%ndist = 0		
        this%node_exp_sum = 0
        !TODO "solver bwd check ndist label large or zero"
        do nr = 1, ndest
            this%nwk%ndist(this%nwk%roots(nr),nr) = 0.0
            do i = nn,1,-1
            !do i = 1,nn
                node = this%nwk%torder(i,nr)
                if ((node.eq.0)) then 
                    cycle
                end if 
                if (this%nwk%toder_level(node,nr).lt.0) then 
                    this%nwk%ndist(node,nr) = large
                    !cycle
                end if
                if (this%nwk%subnode(node,nr)) then 
                    if (node.eq.this%nwk%roots(nr)) then 
                        this%nwk%ndist(node,nr) = 0
                    else
                    this%nwk%ndist(node,nr) = &
                        !(-1/theta)*log(this%node_exp_sum(this%nwk%anode(link),nr)) 
                        (-1/theta)*log(max(this%node_exp_sum(node,nr),zero))
                    end if
                    do j = this%nwk%firstin(node),this%nwk%lastin(node)
                        link = this%nwk%backtoforward(j) 
                        !if (link.eq.66) then 
                        !   write(*,*) "wtf" 
                        !end if 
                        if (this%nwk%sublink(link,nr)) then 
                            ! if((this%x(link,nr).gt.0).and.(this%lf(link).gt.0)) then
                            !if((this%x(link,nr).gt.0).and.(this%nf(this%nwk%anode(link),nr).gt.0.0)) then
                                this%node_exp_sum(this%nwk%anode(link),nr) = this%node_exp_sum(this%nwk%anode(link),nr) &
                                    + exp((-theta)*(this%lt(link) + this%nwk%ndist(this%nwk%bnode(link),nr)))
                            !end if 
                            
                            if (this%node_exp_sum(this%nwk%anode(link),nr).lT.0.0) then
                                write(*,*) "node = ",this%nwk%anode(link)," link = ", link
                                write(*,*) "expvalue = ",exp((-theta)*(this%lt(link)+this%nwk%ndist(this%nwk%bnode(link),nr)))
                                write(*,*) "backward: expum is less than 0" 
                            endif 
                        
                                ! if ((this%nwk%anode(link).eq.18).and.(nr.eq.1)) then 
                                ! write (*,*) "debug"
                                ! write (*,*) this%node_exp_sum(this%nwk%anode(link),nr)
                                ! write (*,*) log(this%node_exp_sum(this%nwk%anode(link),nr)) 
                            ! end if 
                            ! this%nwk%ndist(this%nwk%anode(link),nr) = &
                            !(-1/theta)*log(this%node_exp_sum(this%nwk%anode(link),nr)) 
                                ! (-1/theta)*log(max(this%node_exp_sum(this%nwk%anode(link),nr),zero))
                            link_dest_flow =  this%nf(this%nwk%anode(link),nr) * this%x(link,nr)
                            ! if (this%lf(link).eq.0) then 
                            if (link_dest_flow.eq.0) then
                                fx1(link,nr) = this%nwk%ndist(this%nwk%bnode(link),nr) &
                                    + this%lt(link) + 1/theta
                                ! if ( fx1(link,nr) .gt.10000) then 
                                !    write (*,*) " wtf"
                                !end if
                            else 
                                fx1(link,nr) = this%nwk%ndist(this%nwk%bnode(link),nr) &
                                    ! + this%lt(link)+(1+log(this%lf(link)))/theta 
                                    + this%lt(link)+(1+log(link_dest_flow))/theta 
                                !if ( fx1(link,nr) .gt.10000) then 
                                !    write (*,*) " wtf"
                                !endif
                            end if
                      
                        endif
                    end do 
                end if 
            end do ! do torder nodes
        end do  ! do ndest
    
     endif 
   
    if (islogit) then 
       if(load_index.eq.0) then 
            do nr = 1, ndest
              do l = 1, nl
                if (this%nwk%sublink(l,nr)) then
                    if((this%x(l,nr).gt.0).and.(this%nf(this%nwk%anode(l),nr).gt.0)) then 
                        logitprob(l,nr) = &
                         exp((-theta)*(this%lt(l)+this%nwk%ndist(this%nwk%bnode(l),nr))) &
                         /this%node_exp_sum(this%nwk%anode(l),nr) 
                    end if
                    if (logitprob(l,nr).gt.1+zero) then 
                        write(*,*) "get prob larger than 1"
                    endif
                    if (isnan(logitprob(l,nr))) then 
                        write(*,*) "wtf"
                    end if 
                end if 
               end do
            end do
        else    
            call this%dial_load_main(logitprob)
        endif
        
    end if 


    return
    end subroutine 

    ! this subroutine is used to update the flow for each node
    ! input is the flow proportion 
    ! out put is the link flow and node flow
    ! Algorithm
    subroutine forward_update_flow(this,x0,d1,d2)
    use constpara
    implicit none 
    class(methods)::this 
    ! real*8,intent(in)::x0(nl,ndest)
    integer::d1,d2
    real*8,intent(in),DIMENSION(d1,d2)::x0
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
    ! real*8::x(nl,ndest),xfa(nl,ndest),fx(nl,ndest)
    real*8,dimension(nl,ndest)::x,xfa,fx
    real*8,optional,dimension(nl,ndest)::logitprob
    real*8::madf
    integer::i,nr,node,j, link, tail, head
    ! real*8::lf(nl)
    real*8,dimension(nl)::lf
    ! logical::isupdated(nn,ndest)
    logical,dimension(nn,ndest)::isupdated
    ! real*8::nodefx(nn,ndest)
    real*8,dimension(nn,ndest)::nodefx
    real*8::lamda, mincost,minfx,bcm,thismdf
    integer::bcmcount
    logical::isbcm(5)
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
                    cycle
                  endif 
                  do j = this%nwk%firstout(node), this%nwk%lastout(node)
                    !if(xfa(j,nr).gt.ncp_flow_eps) then 
                      nodefx(node,nr) = min(fx(j,nr),nodefx(node,nr))
                    !end if 
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
                        if (this%nf(this%nwk%anode(i),nr)*this%x(i,nr).gt.ncp_flow_eps) then 
                            !if (abs(this%x(i,nr)-this%logitprob(i,nr)).gt.0.001) then 
                                madf = max(madf,fx(i,nr)-nodefx(this%nwk%anode(i),nr))
                                if (this%solc.gt.100.and.(fx(i,nr)-nodefx(this%nwk%anode(i),nr)).gt.490)  then 
                                    write(*,*) "wtf"
                                end if
                            !endif
                        endif
                        !endif
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

    open(1,file='c:\gitcodes\LogitAssign\results\fortran_output_link.txt',status='old',position='append')
    ! write(1,*) "method,case,dest,link,flow,fx,lt,xprob,logitprob,tail,head"
    do i=1,ndest
        do j=1,nl
            ! if (this%xfa(j,i)/=0.0) then
                write(1,"(a5,a,i3,a,i2,a,i3,a,f10.2,a,f7.2,a,f6.2,a,f6.4,a,f6.4,a,i3,a,i3)") &
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

    subroutine outputod(this,flow,y,d1,d2)
    use constpara
    implicit none
    class(methods)::this
    integer i,j
    ! real*8::flow(nl,ndest),y(nl,ndest)
    integer::d1,d2
    real*8,DIMENSION(d1,d2)::flow,y
    integer q,w
    open(1,file='c:\gitcodes\logitassign\results\fortran_output_od.txt',position='append')
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


    ! subroutine method_outputpath(this,linktime, xfa)
    ! use constpara
    ! implicit none
    ! class(methods)::this
    ! integer i,j
    ! real*8::linktime(nl)
    ! real*8,intent(in)::xfa(nl,ndest)

    ! open(1,file='c:\gitcodes\logitassign\results\fortran_linkcost.txt' )
    ! open(2,file='c:\gitcodes\logitassign\results\fortran_pathcost.txt' ) 
    
    ! write(1,*) "case,linkid,linktime"
    ! do i = 1, nl
    !     write(1,'(i2,a,i3,a,f8.4)') caseindex,',',i,',',linktime(i)
    ! enddo

    ! close(1)


    ! write(2,*) "case,x,approchtime" 
    ! ! the second demmension of the xfa is the destination index
    ! ! the first three paths blong to od pair 1-4
    ! write(2,'(i3,a,f8.4,a,f8.4)') caseindex,',',xfa(3,1),',',linktime(3)
    ! write(2,'(i3,a,f8.4,a,f8.4)') caseindex,',',xfa(2,1),',',linktime(2)+linktime(6)
    ! write(2,'(i3,a,f8.4,a,f8.4)') caseindex,',',xfa(1,1),',',linktime(1)+linktime(4)+linktime(6)
    ! ! the following two belongs to od pair 2-4
    ! write(2,'(i3,a,f8.4,a,f8.4)') caseindex,',',xfa(4,1),',',linktime(4)+linktime(6)
    ! write(2,'(i3,a,f8.4,a,f8.4)') caseindex,',',xfa(5,1),',',linktime(5)
    ! ! the last belong to od pair  3-4
    ! write(2,'(i3,a,f8.4,a,f8.4)') caseindex,',',xfa(6,1),',',linktime(6)

    ! close(2)

    ! end subroutine

    subroutine cal_fx(this,x,fx)
    ! x => fx
    use constpara
    implicit none
    class(methods)::this
    real*8,intent(out)::fx(nl,ndest)
    real*8,intent(in)::x(nl,ndest)
    real*8::st,et,max_dist_err_2
    call cpu_time(st)

    this%solc =  this%solc + 1 
    if (this%solc.eq.33) then 
        write(*,*) "wtf"
    endif 
    call this%forward_update_flow(x,d1=nl,d2=ndest)
    ! call this%nwk%link_time(this%lf,this%stt)
    call this%nwk%link_time(this%lf,this%lt)
    call this%backward_update_fx(fx,this%logitprob,d1=nl,d2=ndest)
    !this%ncperr = this%getncperr(x,this%xfa,fx,this%logitprob,nl,ndest,nn)
    this%ncperr = this%getncperr(x,this%xfa,fx,this%logitprob)

    this%max_dist_gap = max_dist_err_2(x,this%logitprob,this%xfa,ncp_flow_eps,nl,ndest)

    call cpu_time(et)

    if (iswriteconverge) then 
        if(isdebug) then 
            write(*,'(i4,a,f16.8,a,f16.8)')  this%solc,',',this%ncperr,',',this%max_dist_gap
        end if
        write(this%gapfileno,'(i4,a,f16.8,a,f16.8)') this%solc,",",this%ncperr,",",this%max_dist_gap
    end if 

    return 
    end subroutine
    
    
    subroutine init_arc_flow(this)
    implicit none 
    class(methods)::this
    integer::i,o,root,nr,node,arc

    if (islogit) then 
        return
        ! this%nwk%subnode = .false.
        ! this%nwk%sublink = .false.
        this%xfa=0.0
        do nr =1, ndest
            root = this%nwk%roots(nr)
            ! first set the subnode and sublink
            do i = 1, nn
                if (this%nwk%pa(i,nr).ne.0) then 
                    arc = this%nwk%pa(i,nr)
                    arc = this%nwk%backtoforward(arc)
                    ! this%nwk%sublink(arc,nr)=.true.
                    ! this%nwk%subnode(this%nwk%anode(arc),nr)=.true.
                    ! this%nwk%subnode(this%nwk%bnode(arc),nr)=.true.
                end if 
            end do
            do i = 1,nod
                o = this%nwk%origin(i)
                node = o
                do while (node.ne.root)
                    arc = this%nwk%pa(node,nr)	
                    node = this%nwk%backanode(arc)	
                    arc = this%nwk%backtoforward(arc)
                    ! this%nwk%sublink(arc,nr)=.true.
                    ! this%xfa(arc,nr) = this%xfa(arc,nr) + this%nwk%demand(i)
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
                if (this%nwk%pa(i,nr).ne.0) then 
                    arc = this%nwk%pa(i,nr)
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
                    arc = this%nwk%pa(node,nr)	
                    this%nwk%sublink(arc,nr)=.true.
                    node = this%nwk%backanode(arc)	
                    arc = this%nwk%backtoforward(arc)
                    this%xfa(arc,nr) = this%xfa(arc,nr) + this%nwk%demand(i)
                end do 
            enddo 
        end do 
    end if 

    end subroutine

!  dial algorithm solver







subroutine dial_load_main(this,prob)
implicit none 
CLASS(methods)::this

real*8,intent(inout)::prob(nl,ndest)
! this%lt= this%nwk%scost
call this%dial_sub_graph
this%nwk%torder = this%sorder
call this%dial_get_link_like
call this%dial_backward
call this%dial_foward(prob)

end subroutine


subroutine dial_sub_graph(this)
use constpara
implicit none 

integer::r,s,w,i,nr,l,j
class(methods)::this
this%nwk%sublink =.false.
this%nwk%subnode =.false.
do w = 1,nod
    r = this%nwk%origin(w)
    s = this%nwk%dest(w)
    do i = 1, ndest
        if (s.eq.this%nwk%roots(i)) then 
            nr = i
            exit  
        end if 
    enddo 
    ! from r to all nodes 
    call sp(r,this%lt,this%nwk%firstout,this%nwk%lastout,this%nwk%pa(:,nr),this%nwk%bnode)
    this%dial_rlabel(:,nr) = dist(:)
    call sort(-1*this%dial_rlabel(:,nr),this%rorder(:,nr),nn)
    ! from s to all nodes
    call rsp(s,this%nwk%scost,this%nwk%firstin,this%nwk%lastin,this%nwk%pa(:,nr),this%nwk%backbnode,this%nwk%backtoforward)
    this%dial_slabel(:,nr) = dist(:)
    call sort(-1*this%dial_slabel(:,nr),this%sorder(:,nr),nn)

    do l = 1,nl
        i = this%nwk%anode(l)
        j = this%nwk%bnode(l)
        if ((this%dial_rlabel(i,nr).lt.this%dial_rlabel(j,nr)).and.(this%dial_slabel(i,nr).gt.this%dial_slabel(j,nr))) then 
            this%nwk%sublink(l,nr) = .true.  
            this%nwk%subnode(i,nr) = .true.
            this%nwk%subnode(j,nr) = .true.
        end if
    end do 
enddo 

end subroutine 


! compute the link likelyhood
subroutine dial_get_link_like(this)
implicit none 

integer::l,nr,i,j
CLASS(methods)::this

do nr = 1, ndest
    do l = 1, nl
        i = this%nwk%anode(l)
        j = this%nwk%bnode(l)
        if (this%nwk%sublink(l,nr)) then 
            this%dial_link_like(l,nr) = exp(theta*(this%dial_rlabel(j,nr)-this%dial_rlabel(i,nr)-this%lt(l)))
            ! write(*,*) "i = ",i," j = ",j," val = ",this%dial_rlabel(j,nr)-this%dial_rlabel(i,nr)-this%lt(l),&
                        ! " L = ", this%dial_link_like(l,nr)
        else
            this%dial_link_like(l,nr) = 0
        endif
    end do 
end do 

end subroutine

subroutine dial_backward(this)
implicit none 
integer::nr,i,now,l,ol,link
class(methods)::this
real*8::sumwsd

do nr = 1, ndest
    this%dial_Wsd(:,nr) = 0
    ! call sort(-1*this%dial_slabel(:,nr),this%sorder(:,nr),nn)
    do i = 1, nn
        now = this%sorder(i,nr)
        do link = this%nwk%firstin(now),this%nwk%lastin(now)
            l=this%nwk%backtoforward(link)
           if (now.eq.this%nwk%roots(nr)) then 
                !this%dial_Wsd(l,nr) = 1 
               this%dial_Wsd(l,nr) = this%dial_link_like(l,nr)
           else
                sumwsd = 0
                do ol = this%nwk%firstout(now), this%nwk%lastout(now)
                    sumwsd = sumwsd + this%dial_Wsd(ol,nr)
                enddo 
                this%dial_Wsd(l,nr) = this%dial_link_like(l,nr)*sumwsd
           endif 
        !    write(*,*) "i = ",this%nwk%anode(l), " j =",this%nwk%bnode(l), " val=",&
                ! this%dial_Wsd(l,nr)
        enddo
    enddo 
end do

end subroutine


subroutine dial_forward(this,prob)
implicit none

class(methods)::this
integer::nr,i,now,l,a,b
real*8::sumwsd
real*8::prob(nl,ndest)

do nr = 1, ndest
    ! call sort(-1*this%dial_rlabel(:,nr),this%rorder(:,nr),nn)
    do i = 1, nn
        now = this%rorder(i,nr)
        sumwsd = 0
        do l = this%nwk%firstout(now), this%nwk%lastout(now)
            sumwsd = this%dial_Wsd(l,nr) + sumwsd
        enddo
        do l = this%nwk%firstout(now), this%nwk%lastout(now)
            if (sumwsd .eq.0) then
                prob(l,nr) = 0 
            else
                prob(l,nr) = this%dial_Wsd(l,nr)/sumwsd 
            end if
        enddo 
    enddo
    do l = 1, nl
       a = this%nwk%anode(l) 
       b = this%nwk%bnode(l)
    enddo
end do

end subroutine




















    end module 

