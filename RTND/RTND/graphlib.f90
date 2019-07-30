    module GraphLib
    use constpara 
    use mylineclass
    implicit none 

    type, public::graphclass
        logical,allocatable::connect(:,:,:)
        integer,allocatable::numconnect(:,:,:)
        integer,allocatable::roots(:)  !
        integer,allocatable::torder(:,:) ! topoloy oreder
        logical,allocatable::sublink(:,:),subnode(:,:)
        integer,allocatable::firstin(:),lastin(:)
        integer,allocatable::firstout(:),lastout(:)
        integer,allocatable::anode(:),bnode(:)
        integer,allocatable::backanode(:),backbnode(:)
        integer,allocatable::backtoforward(:)
        integer,allocatable::dest(:),origin(:)
        integer,allocatable::locatecompete(:)
        integer,allocatable::competesec(:)
        integer,allocatable::competesec_line(:)
        integer,allocatable::numcom(:)  ! num of competing sections for each link
        real*8,allocatable::scost(:),svar(:),fare(:) ! section cost and section variance
        real*8,allocatable::bpr_t0(:)   !bpr function free flwo travel time
        real*8,allocatable::bpr_cap(:)
        real*8,allocatable::demand(:)
        ! real*8,ALLOCATABLE::fre(:)
        real*8,allocatable::sf(:)       ! section frequency sum
        real*8,allocatable::slf(:,:)   ! section frequency sum: section line frequency
        integer,allocatable::sindex(:)! relate section and link index
        integer,allocatable::sl(:,:)
        integer,allocatable::slc(:)
        integer::caseindex
        !real*8:: tsl(8,4) ! cost, var, fare, fre
        real*8,allocatable::ndist(:,:) ! the shortest distance from node all
        integer,allocatable::pa(:,:)
        integer,ALLOCATABLE::toder_level(:,:)
        logical::isUEconverge
        type(lineclass),dimension(:),ALLOCATABLE::mylines
   contains

    procedure,pass::readnwt=>readnwt
    procedure,pass::createcompetenwk=>createcompetenwk
    procedure,pass::readtransportnwk=>readtransportnwk
    procedure,pass::readsmall=>readsmall
    procedure,pass::minspantree=>minspantree
    procedure,pass::getorder=>getorder
    procedure,pass::getsuebush=>getsuebush   ! get bush based on the order
    procedure,pass::order=>order
    procedure,pass::link_time=>link_time
    procedure,pass::countconect=>countconect
    procedure,pass::updatesub=>updatesub
    procedure,pass::update_section_cost=>update_section_cost
    procedure,pass::update_sec_fre=>update_sec_fre
    procedure,pass::copynwk=>copynwk
    procedure,pass::printnwk=>printnwk
    procedure,pass::construct=>construct
    procedure,pass::outputsub=>outputsub
    procedure,PASS,public::inigraph=>inigraph
    procedure,pass::delgraph=>delgraph
    procedure,pass::BFS_torder=>BFS_torder
    ! procedure,pass::DFS_visit=>DFS_visit
    end type graphclass
    contains

    subroutine inigraph(this)
    use constpara
    implicit none 
    CLASS(graphclass):: this

    allocate(this%connect(nn,nn,ndest))
    this%connect = .false.
    ALLOCATE(this%numconnect(nn,nn,ndest))
    this%numconnect = 0
    allocate(this%roots(ndest))
    this%roots=-1
    allocate(this%torder(nn,ndest)) ! topoloy oreder
    this%torder=-1
    allocate(this%sublink(nl,ndest),this%subnode(nn,ndest),this%firstin(nn),this%lastin(nn))
    this%sublink=.false.
    this%subnode=.false.
    this%firstin= -1
    this%lastin=-1
    allocate(this%firstout(nn),this%lastout(nn),this%anode(nl),this%bnode(nl))
    this%firstout = -1
    this%lastout = -1 
    this%anode = -1
    this%bnode = -1
    allocate(this%backanode(nl),this%backbnode(nl),this%backtoforward(nl))
    this%backanode = -1
    this%backbnode = -1
    this%backtoforward = -1
    allocate(this%dest(nod),this%origin(nod),this%locatecompete(nl+1))
    this%dest = -1
    this%origin = -1
    this%locatecompete = -1
    allocate(this%competesec(max_total_compete_sec),this%competesec_line(max_total_compete_sec))
    this%competesec = -1
    this%competesec_line = -1
    allocate(this%scost(nl),this%svar(nl))
    this%scost = 0
    this%svar = 0
    allocate(this%bpr_t0(nl))
    this%bpr_t0 =  0
    allocate(this%bpr_cap(nl))
    this%bpr_cap = 0
    allocate(this%numcom(nl))
    this%numcom = 0
    allocate(this%fare(nl),this%demand(nod))
    this%fare = 0
    this%demand = 0
    allocate(this%sf(nl))       ! section frequency sum
    this%sf = 0
    allocate(this%slf(nl,nline),this%sindex(nl))
    this%slf = 0
    this%sindex = 0
    allocate(this%sl(nl,maxsecline),this%slc(nl))
    this%sl = 0
    this%slc = 0
    allocate(this%ndist(nn,ndest),this%pa(nn,ndest))
    this%ndist = 0
    this%pa = 0
    allocate(this%toder_level(nn,ndest))
    allocate(this%mylines(nline))
    
    this%caseindex=1

    end subroutine
    
    subroutine delgraph(this)
    implicit none
    CLASS(graphclass)::this
    deallocate(this%connect)
    deALLOCATE(this%numconnect)
    deallocate(this%roots)
    deallocate(this%torder) 
    deallocate(this%sublink,this%subnode,this%firstin,this%lastin)
    deallocate(this%firstout,this%lastout,this%anode,this%bnode)
    deallocate(this%backanode,this%backbnode,this%backtoforward)
    deallocate(this%dest,this%origin,this%locatecompete)
    deallocate(this%competesec,this%competesec_line)
    deallocate(this%scost,this%svar)
    deallocate(this%bpr_t0,this%bpr_cap)
    deallocate(this%numcom)
    deallocate(this%fare,this%demand)
    deallocate(this%sf)       ! section frequency sum
    deallocate(this%slf,this%sindex)
    deallocate(this%sl,this%slc)
    deallocate(this%ndist,this%pa,this%toder_level)
    deallocate(this%mylines)

    end subroutine


    ! read the origin sioxu fall transport network
    subroutine readtransportnwk(this)
    use constpara
    implicit none 
    class(graphclass)::this
    ! integer,intent(in)::numlink,numnode
    integer::i,l,j,k
    integer::dta(3)    ! demand data
    integer::rl(2)
    integer,dimension(nl)::an,bn
    real*8::capval,t0
    real*8,dimension(nl)::temp_bpr_t0
    real*8,DIMENSION(nn,nn)::lcost

    select case(networktype)
    case(-2)
        open(1,file='c:/GitCodes/OpenTransportData/STOCH/demand.txt')
        OPEN(2,file='c:/GitCodes/OpenTransportData/STOCH/linkdata.txt')
        open(3,file='c:/GitCodes/OpenTransportData/STOCH/cap.txt')
        open(4,file='c:/GitCodes/OpenTransportData/STOCH/t0.txt')
        OPEN(5,file='c:/GitCodes/OpenTransportData/STOCH/dests.txt')
    case(-1)
        open(1,file='C:\GitCodes\OpenTransportData\cycle\demand.txt')
        open(2,file='C:\GitCodes\OpenTransportData\cycle\linkdata.txt')
        open(3,file='c:/GitCodes/OpenTransportData/cycle/cap.txt')
        open(4,file='c:/GitCodes/OpenTransportData/cycle/t0.txt')
        open(5,file='c:/GitCodes/OpenTransportData/cycle/dests.txt')
    case(0)
        write(*,*) "you should set asisgn mode to trasnprot "
    case(1)
        write(*,*) "you should set asisgn mode to trasnprot "
    case(2)
        write(*,*) "you should set asisgn mode to trasnprot "
    case(3)
        open(1,file='c:\gitcodes\opentransportdata\siouxfallnet\Transport_Toy\demand.txt')
        open(2,file='c:\gitcodes\opentransportdata\siouxfallnet\Transport_Toy\linkdata.txt')
        open(3,file='c:\GitCodes\OpenTransportData\SiouxFallNet\Transport_Toy\cap.txt')
        open(4,file='c:\GitCodes\OpenTransportData\SiouxFallNet\Transport_Toy\t0.txt')
        open(5,file='c:\GitCodes\OpenTransportData\SiouxFallNet\Transport_Toy\dests.txt')
   
    case(4)
        open(1,file='c:\gitcodes\opentransportdata\siouxfallnet\Transport_AllOD\demand.txt')
        open(2,file='c:\gitcodes\opentransportdata\siouxfallnet\Transport_AllOD\linkdata.txt')
        open(3,file='c:\GitCodes\OpenTransportData\SiouxFallNet\Transport_AllOD\cap.txt')
        open(4,file='c:\GitCodes\OpenTransportData\SiouxFallNet\Transport_AllOD\t0.txt')
        open(5,file='c:\GitCodes\OpenTransportData\SiouxFallNet\Transport_AllOD\dests.txt')
    case default 
        write(*,*) "Wrong Netwotk Case in readsioux_origin"
        pause
    end select
   
    ! the data input is start from index 0, I add one to origin and dest node
     do i=1,nod
        read(1,*) dta(:)
        this%origin(i) = dta(1) 
        this%dest(i) = dta(2) 
        this%demand(i) = dta(3) 
    enddo
    close(1)

    ! read link
    lcost = 0 
    do l = 1, nl
      read(2,*) rl(:)
      read(3,*) capval
      read(4,*) t0
      this%bpr_cap(l) = capval
      an(l) =  rl(1) 
      bn(l) =  rl(2)
      temp_bpr_t0(l) = t0
      lcost(an(l),bn(l)) = t0   ! there exists a link
    end do 

    ! index starts from 0
    do i = 1,ndest
        read(5,*) j,k
        this%roots(j ) = k
    enddo
    close(2)
    close(3)
    close(4)
    close(5)
    
    
    call this%construct(lcost,nn)
    this%bpr_t0 = temp_bpr_t0
    this%scost = this%bpr_t0

    end subroutine
    
      ! construct incoming and outgoing link
    subroutine construct(this,lcost,numnode,lvar)
    implicit none 
    class(graphclass):: this
    integer,intent(in)::numnode
    real*8,intent(in)::lcost(numnode,numnode)
    real*8,optional::lvar(numnode,numnode)
    integer::i,j,countline,l

    ! build first out and last out
    countline=1
    do i=1,nn
        this%firstout(i)=countline
        do j=1,nn
            if(lcost(i,j)>0.0) then
                this%anode(countline)=i
                this%bnode(countline)=j
                this%scost(countline) = lcost(i,j)
                if(present(lvar))then
                    this%svar(countline) = lvar(i,j)
                end if
                countline=countline+1
            end if
        end do
        this%lastout(i)=countline-1
    end do
    ! build first in and last in
    countline = 1
    do j = nn,1,-1
        this%firstin(j) = countline
        do i = 1,nn
            if(lcost(i,j)>0.0) then
                this%backanode(countline) = j
                this%backbnode(countline) = i
                countline = countline+1
            end if
        end do
        this%lastin(j) = countline-1
    end do

    do l = 1,nl
        do j = 1,nl
            if ((this%backanode(l).eq.this%bnode(j)).and.(this%backbnode(l).eq.this%anode(j))) then
                this%backtoforward(l)=j
            end if
        end do
    enddo

    do i=1,nl
        l = this%backtoforward(i)
        if ((this%anode(l).ne.this%backbnode(i)).or.(this%bnode(l).ne.this%backanode(i))) then
            write(*,*) "bug inconsit"
        end if
    end do

    end subroutine 
    
    ! subroutine readnwt(this,rhsnwk,numlink,numnode,numline,maxcomsec,maxseclineval,maxlinestopval)
    subroutine readnwt(this,rhsnwk)
    use constpara
    use mylineclass
    implicit none
    class(graphclass)::this 
    class(graphclass),OPTIONAL::rhsnwk
    ! integer,intent(in)::numlink,maxcomsec,numnode,numline,maxseclineval,maxlinestopval

    if (present(rhsnwk)) then 
        write(*,*) "copy nwk is called, which should not be presented"
        call this%copynwk(rhsnwk)
        call gc_update_secfre(this%mylines%fre,this%slc,this%sl,this%sf,this%slf)
        call this%update_section_cost
        return
    end if 

    if (assignmode.eq.1) then 
        call this%readtransportnwk
    else
        call this%createcompetenwk
    end if

    if (isHardCodePathBcm) then 
       path_link(:,:) = -1
       path_link(1,1) = 3
       path_link(2,1) = 1
       path_link(2,2) = 4
       path_link(2,3) = 6
       path_link(3,1) = 1
       path_link(3,2) = 5
       path_link(4,1) = 2
       path_link(4,2) = 6
    end if 
    ! select case(networktype)
    ! case (0) !small network 

    !     call this%createcompetenwk

    !     write(*,*) "create comete"

    ! case (1)
        
    !     if (assignmode.eq.1) then 
    !         write(*,*) "Read original sioux fall network"
    !         !call this%readsioux_origin(numlink=nl,numnode=nn)
    !         call this%readsioux_origin
    !     else
    !         write(*,*) "Create transit network for sioux fall"
    !         call this%createcompetenwk
    !         !call this%createcompetenwk(numlink=nl,numline=nline,numnode=nn,&
    !             !maxcomval=maxcomsec,maxseclineval=maxcomsec,maxlinestopsval=maxlinestops)
    !     end if
    ! case default 
    !     write(*,*) "network index has not been set"
    !     pause
    ! end select

    end subroutine
    
    subroutine gc_update_secfre(fre,slc,sl,sf,slf)
    use constpara
    implicit none
    integer i,j
    integer::numlink,numline,maxseclineval
    real*8,intent(in),dimension(nline)::fre
    integer,intent(in),dimension(nl)::slc
    integer,intent(in),dimension(nl,maxsecline)::sl
    real*8,intent(inout),dimension(nl)::sf
    real*8,intent(inout),dimension(nl,nl)::slf

    sf=0.0
    do i=1,nl
        do j=1,slc(i)
            sf(i)=sf(i)+fre(sl(i,j))
        enddo
        ! if sf = 0, then it is walking link
        if (sf(i).eq.0) then
            sf(i)=inf1
        end if
    enddo
    slf=0.0
    do i=1,nl
        do j=1,slc(i)
            slf(i,sl(i,j))=fre(sl(i,j))/sf(i)
        enddo
    end do
    end subroutine

    ! get topological order of the network 
    subroutine getorder(this)
    use constpara
    implicit none
    class(graphclass)::this
    integer::nr,i
    integer::node, link, j
    
!the order is found by via the minimum spanning tree  
    do nr = 1,ndest
        call this%order(nr,this%ndist(:,nr))
    end do 
!find the laregest cost lable
    lndist = 0.0
    do nr = 1,ndest
        do i = nn,1,-1
            node = this%torder(i,nr)
            if (node.ne.0) then
                do j = this%firstin(node),this%lastin(node)
                    link = this%backtoforward(j)
                    if (this%subnode(this%anode(link),nr)) then 
                        if (lndist(this%anode(link),nr).lt.lndist(this%bnode(link),nr)+this%scost(link)) then 
                            lndist(this%anode(link),nr) = lndist(this%bnode(link),nr)+this%scost(link)
                        end if 
                    endif
                enddo
            end if 
        enddo 
    enddo 
    end subroutine


    subroutine order(this,nr,lable)
    implicit none 
    class(graphclass)::this
    integer,intent(in):: nr
    real*8,intent(in)::lable(nn)
	integer i,j,ti
	real*8 vec(nn),temp
	forall(i=1:nn)
		this%torder(i,nr)=i
	endforall

	vec(:)=lable(:)
	do i=1, nn
        do j=nn, i+1, -1
            if (vec(j-1).le.vec(j)) then 
				temp = vec(j-1)
				vec(j-1) = vec(j)
				vec(j) = temp
				ti =  this%torder(j-1,nr)
				this%torder(j-1,nr) = this%torder(j,nr)
				this%torder(j,nr) = ti
            end if 
        end do 
    end do 
    do i = 1,nn
        if (vec(i).ge.large) then
            this%torder(i,nr) = 0
        end if
    end do  
    return

    end subroutine

    ! update subnetowrk for the bcm paper
    subroutine updatesub(this,lt,del,xfa)
    implicit none 
    
    class(graphclass)::this
	!step 
    real*8,intent(in)::lt(nl)
    logical,intent(in)::del
    real*8,intent(in)::xfa(nl,ndest)
    integer::nr,node,i,j,link,a,b,l
! first add new links 	
    if (islogit) then
        call this%minspantree(lt)   
        call this%bfs_torder
        call this%getorder
        call this%getsuebush
        return 
    end if
    if (.not.del) then 
        goto 5
    else   
        do nr = 1,ndest
            do l = 1, nl
                if (this%sublink(l,nr)) then
                    if (xfa(l,nr).le.flow_eps) then 
                        a = this%anode(l)
                        b = this%bnode(l)
                        if (this%connect(a,b,nr).and.(this%numconnect(a,b,nr)-1.ge.1)) then
                            this%sublink(l,nr) = .false.
                            this%numconnect(a,b,nr) = this%numconnect(a,b,nr) - 1
                            write(*,*) "del link"
                        end if 
                    end if
                end if 
            enddo 
        enddo 
    end if 

! add link


5   this%isUEconverge = .true.
! step 1 update lable
    this%ndist = large				
    do nr = 1,ndest
        this%ndist(this%roots(nr),nr) = 0.0
        do i = nn,1,-1
            node = this%torder(i,nr)
            if (node.ne.0) then 
                if (this%subnode(node,nr)) then 
                    do j = this%firstin(node),this%lastin(node)
                        link = this%backtoforward(j)
                        if (this%sublink(link,nr)) then
                            if (this%ndist(this%bnode(link),nr)+lt(link).lt.this%ndist(this%anode(link),nr)) then
                                this%ndist(this%anode(link),nr) = this%ndist(this%bnode(link),nr)+lt(link)
                            end if 
                        end if
                    end do 
                end if 
            end if 
        end do 
    enddo

    do nr = 1,ndest 
        do i = nn,1,-1 
            node = this%torder(i,nr)
            if (node.ne.0) then 
                do j = this%firstin(node),this%lastin(node)
                    link = this%backtoforward(j) 
                    if ((.not.this%sublink(link,nr))) then 
                        if (lt(link)+this%ndist(this%bnode(link),nr).lt.this%ndist(this%anode(link),nr)) then 
                            if (lndist(this%bnode(link),nr).lt.lndist(this%anode(link),nr)) then 
                                this%sublink(link,nr) = .true. 
                                this%numconnect(this%anode(link),this%bnode(link),nr) & 
                                = this%numconnect(this%anode(link),this%bnode(link),nr) + 1 
                                this%isUEconverge = .false.
					!			write(*,*) "addlnk"
                            end if 
                        end if 
                    end if 
                end do 
            end if 
        end do 
    end do
	 
    return
    end subroutine 
    
    subroutine minspantree(this,lt)
    implicit  none
    class(graphclass)::this
    real*8,intent(in)::lt(nl)
    integer::root,nr,i,arc

    do nr =1,ndest
        root = this%roots(nr)
        call rsp(root,lt,this%firstin,this%lastin,this%pa(:,nr),this%backbnode,this%backtoforward)
        !subroutine rsp(r,link_time,firstout,lastout,pa,backbnode,backtoforward)
        this%ndist(:,nr)=dist(:)  ! Shortest distance
        do i = 1, nn
            if (this%pa(i,nr).ne.0) then 
                arc = this%pa(i,nr)
                arc = this%backtoforward(arc)
                this%sublink(arc,nr)=.true.
                this%subnode(this%anode(arc),nr)=.true.
                this%subnode(this%bnode(arc),nr)=.true.
            end if 
        end do
    enddo
    end subroutine
    
    subroutine countconect(this)
    ! should check the connectivity between od nodes
    use constpara
    implicit none 
    class(graphclass)::this
    integer i,nr,l,j,node
    integer a, b
    this%numconnect = 0.0
    this%connect = .false.
    do nr = 1,ndest
        do i = nn,1,-1
            node = this%torder(i,nr)
            if (node.ne.0) then 
                do j = this%firstin(node),this%lastin(node)
                    l = this%backtoforward(j)
                    a = this%anode(l)
                    b = this%bnode(l)
                    if (this%sublink(l,nr).and.(this%subnode(a,nr))) then
                        this%connect(a,b,nr) = .true.
                        this%numconnect(a,b,nr) = this%numconnect(a,b,nr) + 1
                    end if 
                enddo 
            endif 
        enddo 
    enddo 
    return 
    end subroutine 


    
    subroutine link_time(this,linkflow,linktime)
    use constpara
    implicit none 
    class(graphclass)::this
    
    real*8,intent(in)::linkflow(nl)  ! this is link flow 
    real*8,intent(out)::linktime(nl)
    real*8::vehicle_mean(nl),vehicle_var(nl),wait_mean(nl),wait_var(nl),congest_mean(nl),congest_var(nl)
    real*8::cflow(nl)
    integer::i,j,l
    real*8::fact
    integer::n,kj
    integer::cl


    ! bpr function
    if (assignmode.eq.1) then 
        ! do l =1, nl
            ! linktime(l) = this%bpr_t0(l)*(1+bpr_alpha*((linkflow(l)/this%bpr_cap(l))**bpr_beta)) 
        ! end do 
        forall(l=1:nl)
            linktime(l) = this%bpr_t0(l)*(1+bpr_alpha*((linkflow(l)/this%bpr_cap(l))**bpr_beta)) 
        end forall
        return 
    end if

    n = congestion_n
    vehicle_mean = this%scost
    vehicle_var = this%svar
    wait_mean=1/this%sf
    wait_var=(1/this%sf)**2

    cflow=0.0
    ! do i=1,nl
    do i=1,nl
        cflow(i)=cflow(i)+linkflow(i)  !own section
        if (this%numcom(i)>0) then  ! num of competting section 1
            do kj = this%locatecompete(i),this%locatecompete(i) + this%numcom(i) - 1
                j = this%competesec(kj)
                cl = this%competesec_line(kj)
                cflow(i) = cflow(i) + linkflow(j)*this%mylines(cl)%fre/this%sf(j)
           enddo 
        endif
    enddo 

    congest_mean=bs*fact(n)*((cflow/(gama*capk*this%sf))**n)
    congest_var=(bs**2)*(fact(2*n)-fact(n)**2)*(cflow/(gama*capk*this%sf))**(2*n)
    linktime = (vehicle_mean+wait_mean+congest_mean) &
        + rio*(vehicle_var+wait_var+congest_var) + this%fare

    return 
    end subroutine 

    
    subroutine update_section_cost(this)
    ! theis is to update expected section cost when the frequency is updated
    use mylineclass
    implicit none 
    class(graphclass)::this
    integer::tail,head,lid
    real*8::t,v,f
    integer::s,l
    this%scost = 0
    this%svar = 0
    this%fare = 0

    do s = 1, nl
        do l = 1, maxsecline
            if (this%sl(s,l).le.0) then 
                exit
            end if 
            lid = this%sl(s,l)
            tail = this%anode(s)
            head = this%bnode(s)
            call this%mylines(lid)%get_stop_costs(tail,head,t,v,f)
            this%scost(s) = this%scost(s) + t*this%slf(s,lid)
            this%svar(s) =  this%svar(s) + v*this%slf(s,lid)
            this%fare(s) = this%fare(s) + f*this%slf(s,lid)
        enddo 
    end do 

    end subroutine

    subroutine update_sec_fre(this)
    implicit none 
    class(graphclass)::this 
   
    call gc_update_secfre(this%mylines%fre,this%slc,this%sl,this%sf,this%slf)
    end subroutine

    
    subroutine readsmall(this,numlink,numline,numnode,maxcomval,maxseclineval,maxlinestopsval)
    use constpara
    implicit none   
    class(graphclass):: this
    integer,intent(in)::numlink,numline,numnode,maxcomval,maxseclineval,maxlinestopsval
    integer::i,j,k,countline,odpair,l,countcompete
    integer,dimension(numlink,maxcomval)::compete,temp_compete
    real*8::dta(4)  ! temperal data for reading file
    real*8,dimension(numnode,numnode)::lcost! link travel cost matrix each pair of node
    real*8,dimension(numnode,numnode)::lvar     ! link variance
    real*8::tempreal	! section cost
    real*8,DIMENSION(numlink)::tfa,tva    ! seems to be a temp cost
    !integer::head,tail,head1,head2
    ! tail --> head
    !integer::section(nn,nn)
    integer::s,sc
    integer,dimension(numlink,maxseclineval)::section_line ! section_line index
    integer,dimension(numlink)::an,bn  ! head and tail node based on section index
    integer,dimension(numlink)::section_line_count! section line counter

    open(1,file='c:\gitcodes\BTNDP\input\testnetwork\odpairs.txt')
    open(2,file='c:\gitcodes\BTNDP\input\testnetwork\numlinestops.txt')
    open(4,file='c:\gitcodes\BTNDP\input\testnetwork\destnodeset.txt')
    open(8,file='c:\gitcodes\BTNDP\results\fortran_linecostandvar.txt')
    ! step 1
    ! read od pairs
    do i=1,nod
        read(1,*) dta(:)
        odpair = dta(1)
        this%origin(odpair) = dta(2)
        this%dest(odpair) = dta(3)
        this%demand(odpair) = dta(4)
    enddo
    close(1)

    !read root nodes for the assignment

    do i = 1,ndest
        read(4,*) j,k
        this%roots(j) = k
    enddo
    close(4)

    if (isHardCodePathBcm) then 
       path_link(:,:) = -1
       path_link(1,1) = 3
       path_link(2,1) = 1
       path_link(2,2) = 4
       path_link(2,3) = 6
       path_link(3,1) = 1
       path_link(3,2) = 5
       path_link(4,1) = 2
       path_link(4,2) = 6
    end if 
   do i = 1, nline
        read(6,*) tempreal
        !this%fre(i) = tempreal
        this%mylines(i)%fre = tempreal
        this%mylines(i)%fre = this%mylines(i)%fre/60.0
    enddo
    close(6)

    ! this is the initial frequency setting
    ! TODO create route section network with line data
    this%sindex=0
    this%slc=0
    this%sl=0
    an=0
    bn=0
    ! network structure
    an(1) = 1
    bn(1) = 2
    this%slc(1) = 1
    this%sl(1,1) = 2

    an(2) = 1
    bn(2) = 3
    this%slc(2) = 1
    this%sl(2,1) = 2

    an(3) = 1
    bn(3) = 4
    this%slc(3) = 1
    this%sl(3,1) = 1

    an(4) = 2
    bn(4) = 3
    this%slc(4) = 2
    this%sl(4,1) = 2
    this%sl(4,2) = 3

    an(5) = 2
    bn(5) = 4
    this%slc(5) = 1
    this%sl(5,1) = 3

    an(6) = 3
    bn(6) = 4
    this%slc(6) = 2
    this%sl(6,1) = 3
    this%sl(6,2) = 4

    ! input compete section index
    !compete=0  record the number of compete sections
    this%numcom = 0
    compete = 0
    this%numcom(1) = 1
    compete(1,1) = 2

    this%numcom(2) = 1
    compete(2,1) = 1

    this%numcom(3) = 0

    this%numcom(4) = 2
    compete(4,1) = 5
    compete(4,2) = 2

    this%numcom(5) = 1
    compete(5,1) = 4

    this%numcom(6) = 1
    compete(6,1) = 5
    
    !call gc_updatesectioncost(this%fre,this%tsl,this%scost,this%svar,this%fare)
    this%anode = an
    this%bnode = bn
    call ini_lines(this%mylines)
    call gc_update_secfre(this%mylines%fre,this%slc,this%sl,this%sf,this%slf)

    call this%update_section_cost
    
    !************************************************
    write(7,*) "scost,svar,fare" 
    do i = 1, size(this%scost)
        write (7,'(f6.2,a,f6.2,a,f6.2)') this%scost(i),',', this%svar(i),',',this%fare(i)
    enddo
    close(7)

    lcost(1,2) = this%scost(1)
    lcost(1,3) = this%scost(2)
    lcost(1,4) = this%scost(3)
    lcost(2,3) = this%scost(4)
    lcost(2,4) = this%scost(5)
    lcost(3,4) = this%scost(6)


    lvar(1,2) = this%svar(1)
    lvar(1,3) = this%svar(2)
    lvar(1,4) = this%svar(3)
    lvar(2,3) = this%svar(4)
    lvar(2,4) = this%svar(5)

    !************************************************************************************

    sc = 6
    countline=1
    do i=1,nn
        this%firstout(i)=countline
        do j=1,nn
            if(lcost(i,j)>0.0) then
                !lcost(tail,head)=scost(i)
                this%anode(countline)=i
                this%bnode(countline)=j
                tfa(countline)=lcost(i,j)
                tva(countline)=lvar(i,j)
                do k=1,sc
                    !if (an(k)==j.and.bn(k)==i) then
                    if (bn(k)==j.and.an(k)==i) then
                        this%sindex(countline)=k
                    end if
                enddo
                countline=countline+1
            end if
        end do
        this%lastout(i)=countline-1
    end do

    countline=1
    do j=nn,1,-1
        this%firstin(j)=countline
        do i=1,nn
            if(lcost(i,j)>0.0) then
                this%backanode(countline)=j
                this%backbnode(countline)=i
                countline=countline+1
            end if
        end do
        this%lastin(j)=countline-1
    end do

    do l=1,nl
        do j=1,nl
            if ((this%backanode(l).eq.this%bnode(j)).and.(this%backbnode(l).eq.this%anode(j))) then
                this%backtoforward(l)=j
            end if
        end do
    enddo
    !		write(*,*) "check consit"

    do i=1,nl
        l = this%backtoforward(i)
        if ((this%anode(l).ne.this%backbnode(i)).or.(this%bnode(l).ne.this%backanode(i))) then
            write(*,*) "bug inconsit"
        end if
    end do
    !	dif = lastin - firstin

    ! check compete section by line segement

    do i=1,nl
        s=this%sindex(i)
        section_line(i,:)=this%sl(s,:)
        section_line_count(i)=this%slc(s)
    enddo
    this%sl=section_line
    this%slc=section_line_count

    countcompete = 1
    do i = 1,nl
        this%locatecompete(i) = countcompete
        if (sum(compete(i,:)).eq.0) then
            this%locatecompete(i) = 0
        else
            do j = 1,10
                if (compete(i,j).ne.0)then
                   this%competesec(countcompete) = compete(i,j)
                    countcompete = countcompete + 1
                end if
            end do
        end if
    enddo

    this%scost = tfa
    this%svar = tva
    do i = 1,nn
        do j = this%firstout(i),this%lastout(i)
            if ((i.ne.this%anode(j))) then
                write(*,*) "anode(j) err"
            end if
        end do
    end do

    do i = 1,nn
        do l = this%firstin(i),this%lastin(i)
            if (this%bnode(this%backtoforward(l)).ne.i) then
                write(*,*) "firstin err"
            end if
        end do
        do l = this%firstout(i),this%lastout(i)
            if (this%anode(l).ne.i) then
                write(*,*) "firstout err"
            end if
        end do
    end do

    call gc_update_secfre(this%mylines%fre,this%slc,this%sl,this%sf,this%slf)
    call this%update_section_cost

    end subroutine
    
    
    subroutine copynwk(this, rhs)
    implicit none 
    integer l
    class(graphclass)::this, rhs
        this%roots = rhs%roots
        this%firstin= rhs%firstin
        this%lastin = rhs%lastin
        this%firstout = rhs%firstout
        this%lastout = rhs%lastout
        this%anode = rhs%anode        
        this%bnode = rhs%bnode
        this%backanode = rhs%backanode
        this%backbnode = rhs%backbnode
        this%backtoforward = rhs%backtoforward
        this%dest = rhs%dest
        this%origin = rhs%origin
        this%locatecompete = rhs%locatecompete
        this%competesec =  rhs%competesec
        this%scost = rhs%scost
        this%svar = rhs%svar
        this%fare = rhs%fare
        this%numcom = rhs%numcom
        this%demand = rhs%demand
        this%sf = rhs%sf
        this%slf = rhs%slf
        this%sindex = rhs%sindex
        this%sl = rhs%sl
        this%slc = rhs%slc
        this%caseindex = rhs%caseindex 

         do l =1, nline
          call this%mylines(l)%copy(rhs%mylines(l))
         enddo 


    end subroutine


    subroutine printnwk(this)
    use constpara
    implicit none
    class(graphclass):: this

    integer::n, l, j
    !print links
    open(1,file='c:\gitcodes\BTNDP\results\checknwk.txt')
    write(1,"(a5,a6,a6,a7)") "link,","anode,","bnode,","numslc,"
   
    do l = 1, nl
        write(1,*) l,this%anode(l),this%bnode(l), this%slc(l)
    enddo 
    write(1,"(a5,a5,a5)") "node,","fout,","lout,"
    ! print node
    do n=1, nn
        write(1,*) n, this%firstout(n), this%lastout(n)
    enddo 
    write(1,*) "******contained lines********"


    write(1,*) "Sec,Line"
    do l = 1, nl 
        do j =1, this%slc(l)
            write(1,*) l, this%sl(l,j)
        enddo 
    enddo 

    ! write(1,*) "******compete lines********"

    do l =1, nl
        do j =this%locatecompete(l), this%locatecompete(l)+this%numcom(l)-1
            write(1,*) l, this%competesec(j)
        enddo 
    enddo 


    close(1)
    end subroutine

    subroutine outputsub(this,nr)
    implicit none 
    class(graphclass)::this  
    
    integer, intent(in):: nr  !:: destiantion s
    integer:: n,l
    integer::node

    write(*,*) "output torder nodes"
    do n = nn, 1, -1
        node = this%torder(n,nr)
        if (node.ne.0) then 
            write(*,*) node
        end if
    end do 

    write(*,*) "output sublinks"

    do l =  1, nl
        if (this%sublink(l,nr)) then 
            write(*,*) this%anode(l), this%bnode(l)
        end if
    end do 
    end subroutine


    ! based on order connet all the links
    subroutine getsuebush(this)
    ! create bush subnetwork for the sue assignment 
    use constpara
    implicit none 
    class(graphclass)::this

    integer::now, next
    integer::i, j, nr, l

    this%sublink = .false.
    this%subnode = .false.

    do nr = 1, ndest
        do i = 1, nn-1
            do j = i+1,nn
               now = this%torder(i,nr) 
               next = this%torder(j,nr)
               if ((now.gt.0).and.(next.gt.0)) then 
                  this%subnode(now, nr) = .true.
                  this%subnode(next,nr) = .true.
                    do l = this%firstout(now), this%lastout(now)
                        if (this%bnode(l).eq.next) then 
                            this%sublink(l, nr) = .true. 
                        end if
                    enddo 
               end if
            end do 
        end do 
    end do
    end subroutine


    ! subroutine createcompetenwk(this,numlink,numline,numnode,maxcomval,maxseclineval,maxlinestopsval)
    subroutine createcompetenwk(this)
    use constpara
    implicit none 
    class(graphclass)::this 

    ! integer,intent(in)::numlink,numline,numnode,maxcomval,maxseclineval,maxlinestopsval
    integer::s,s1,s2,s3,m,mstop,now,next,sc,i,j,head1,head2,l,tail,head,checkmaxsecline,checkmaxcom,countcompete,lid
    integer,dimension(nl,maxcom)::compete
    integer,dimension(nl,maxcom)::compete_lines
    real*8,dimension(nn,nn)::lcost! link travel cost matrix each pair of node
    real*8,dimension(nn,nn)::lvar     ! link variance
    real*8,dimension(nl)::tfa,tva    ! seems to be a temp cost
    integer,dimension(nl)::an,bn  ! head and tail node based on section index
    integer,dimension(nn,nn)::section
    real*8,dimension(nl)::scost,svar
    integer,dimension(nl)::slc
    integer::a_stop,b_stop,k
    real*8::dta(3)  ! temperal data for reading file
    integer,dimension(nl,maxsecline)::sl
    real*8::tt,tvar,ff
   
    select case(networktype) 
    case(-2)
        open(1,file='c:/GitCodes/OpenTransportData/STOCH/demand.txt')
        open(2,file='c:/GitCodes/OpenTransportData/STOCH/dests.txt')
    case(-1)
        open(1,file='C:\GitCodes\OpenTransportData\cycle\Demand.txt')
        open(2,file='C:\GitCodes\OpenTransportData\cycle\dests.txt')
    case(0) 
        open(1,file='c:\gitcodes\BTNDP\input\testnetwork\Demand.txt')
        open(2,file='c:\gitcodes\BTNDP\input\testnetwork\dests.txt')
    case(1)
        open(1,file='c:\gitcodes\OpenTransportData\SiouxFallNet\Transit_Toy\Demand.txt')
        open(2,file='c:\gitcodes\OpenTransportData\SiouxFallNet\Transit_Toy\dests.txt')
    case(2)
        open(1,file='c:\gitcodes\OpenTransportData\SiouxFallNet\Transit_AllOD\Demand.txt')
        open(2,file='c:\gitcodes\OpenTransportData\SiouxFallNet\Transit_AllOD\dests.txt')
    case(3)
        open(1,file='c:\gitcodes\OpenTransportData\SiouxFallNet\Transport_Toy\Demand.txt')
        open(2,file='c:\gitcodes\OpenTransportData\SiouxFallNet\Transport_Toy\dests.txt')
    case(4)
        open(1,file='c:\gitcodes\OpenTransportData\SiouxFallNet\Transport_AllOD\Demand.txt')
        open(2,file='c:\gitcodes\OpenTransportData\SiouxFallNet\Transport_AllOD\dests.txt')
    end select
    
    ! if (networktype.eq.0) then 
    !     open(1,file='c:\gitcodes\logitassign\input\testnetwork\Demand.txt')
    !     open(2,file='c:\gitcodes\LogitAssign\input\testnetwork\dests.txt')
    ! end if 
    ! if (networktype.eq.1) then 
    !     open(1,file='c:\gitcodes\OpenTransportData\SiouxFallNet\Demand.txt')
    !     open(2,file='c:\gitcodes\OpenTransportData\SiouxFallNet\dests.txt')
    ! endif 
    ! ! step 1
    ! read od pairs
    do i=1,nod
        read(1,*) dta(:)
        this%origin(i) = Int(dta(1))
        this%dest(i) = Int(dta(2))
        this%demand(i) = dta(3)
    enddo
    close(1)

    do i = 1,ndest
        read(2,*) j,k
        this%roots(j) = k
    enddo
    close(2)
    
    call ini_lines(this%mylines)
    sc = 0
    slc = 0
    sl = 0
    section = 0
    scost = 0.0
!	forword check 
    do l = 1, nline
        do s1 = 1,this%mylines(l)%numstops
            do s2 = s1 + 1, this%mylines(l)%numstops 
                tail = this%mylines(l)%stops(s1)
                head = this%mylines(l)%stops(s2)
                mstop = s2 - s1   ! number of middle stops
                if (section(tail,head).eq.0) then ! create a new section  
                    section(tail,head) = 1
                    sc = sc + 1
                    an(sc) = tail
                    bn(sc) = head
				    slc(sc) = slc(sc) + 1
                    sl(sc,slc(sc)) = l
                    ! get section cost
					now = s1
                    do m=1,mstop
                        next = now + 1
                        a_stop = this%mylines(l)%stops(now)
                        b_stop = this%mylines(l)%stops(next)
                        call this%mylines(l)%get_stop_costs(a_stop, b_stop, tt,tvar)
                        ! temp set cost 
                        scost(sc) = tt
                        svar(sc) = tvar
                        now=next
                    end do 
                else if (section(tail,head).eq.1) then 
                ! if already defined section then find the secion
                    do j = 1, sc
                        ! get the corresponding line index
                        if ((an(j).eq.tail).and.(bn(j).eq.head)) then 
                            slc(j) = slc(j)+1
                            sl(j,slc(j) )= l
                            now = s1
                            do m = 1, mstop
                                next = now + 1
                                a_stop = this%mylines(l)%stops(now)
                                b_stop = this%mylines(l)%stops(next)
                                call this%mylines(l)%get_stop_costs(a_stop,b_stop,tt,tvar)
                                scost(sc) = tt
                                svar(sc)  = tvar
                                now=next
                            end do
                            exit
                        endif 
                    end do 
                end if 
            end do 
        end do 
    enddo 
    
    lcost = 0
    do s =1, sc
       lcost(an(s),bn(s)) = scost(s)
       lvar(an(s),bn(s)) = svar(s)
    end do 

    call this%construct(lcost,nn,lvar)

    checkmaxsecline = 0
    do i =1,sc
	    if (slc(i)>checkmaxsecline) then
            checkmaxsecline = slc(i)
	    end if 
    end do 
    write(*,*)  "maxmum no. of lines in one section is ", checkmaxsecline 

    ! convert section cost to the this anode 
    do l = 1, sc
        do s = 1, sc
            if ((this%anode(l).eq.an(s)).and.(this%bnode(l).eq.bn(s))) then 
                this%slc(l) = slc(s)
                this%sl(l,:) = sl(s,:)
                exit
            end if
        enddo
        this%sf(l) = 0.0
        this%scost(l) = 0.0
        this%svar(l) = 0.0
        this%fare(l) = 0.0
        do j = 1,this%slc(l)
            lid = this%sl(l,j)
            this%sf(l) = this%sf(l) + this%mylines(lid)%fre
            a_stop = this%anode(l) 
            b_stop = this%bnode(l)
            call this%mylines(lid)%get_stop_costs(a_stop,b_stop,tt,tvar,ff)
            this%scost(l) = this%scost(l) + tt*this%mylines(lid)%fre
            this%fare(l) = this%fare(l) + ff*(this%mylines(lid)%fre)
            this%svar(l) = this%svar(l) + tvar*(this%mylines(lid)%fre**2)
        end do 
        if (this%sf(l).eq.0.0) then
            write(*,*) "Walk cost is not expected"
            this%scost(l) = walkcost
            pause
        else
            this%scost(l) = this%scost(l)/this%sf(l)
            this%fare(l) = this%fare(l)/this%sf(l)
            this%svar(l) = this%svar(l)/(this%sf(l)**2)
        end if
    end do

    ! compute compete section 
    compete = 0
    compete_lines = 0
    this%numcom = 0
    do l = 1, nline
        !write(*,*) "build compete section for line ",l
        do s1 = 1, this%mylines(l)%numstops-2
            tail = this%mylines(l)%stops(s1)
            do s2 = s1 + 1, this%mylines(l)%numstops - 1
                head1 = this%mylines(l)%stops(s2)
                do s3 = s2 + 1, this%mylines(l)%numstops
                    head2 = this%mylines(l)%stops(s3)
                    ! write(*,*) "s1=",s1,"s2=",s2,"s3=",s3
                    do i = 1, sc
                    ! mutually compete case
                        if ((this%anode(i).eq.tail).and.(this%bnode(i).eq.head1)) then
                            do j = 1, sc
                                ! write(*,*) "i=",i,"j=",j
                                ! if (s1==1.and.s2==2.and.s3==3.and.i==1.and.j==2) then 
                                    ! write(*,*) "wtf"
                                ! endif
                                if ((this%anode(j).eq.tail).and.this%bnode(j).eq.head2) then 
                                    this%numcom(i) = this%numcom(i) + 1
                                    compete(i,this%numcom(i)) = j
                                    compete_lines(i,this%numcom(i)) = l
                                    this%numcom(j) = this%numcom(j) + 1
                                    compete(j,this%numcom(j)) = i
                                    compete_lines(j, this%numcom(j)) = l
                                endif 
                            enddo 
                        endif
                    enddo 
                    ! write(*,*) "done mutually compete"
                    ! down stream stop compete with upper stream
                    do i = 1, sc
                        if ((this%anode(i).eq.tail).and.(this%bnode(i).eq.head2)) then 
                            do j = 1,sc
                                if ((this%anode(j).eq.head1).and.(this%bnode(j).eq.head2)) then 
                                    this%numcom(j) = this%numcom(j)+1
                                    compete(j,this%numcom(j)) = i
                                    compete_lines(j,this%numcom(j)) = l
                                endif 
                            enddo 
                        endif 
                    enddo 
                enddo 
            enddo 
        enddo 
    enddo 
    
	countcompete = 1
    this%competesec = 0
    this%competesec_line = 0
	do i = 1, nl
		this%locatecompete(i) = countcompete
		if (sum(compete(i,:)).eq.0) then 
            this%locatecompete(i) = 0
        else
            J = 1
            DO WHILE (compete(i,j).ne.0)
                if (countcompete.gt.max_total_compete_sec) then 
                    write(*,*) "warning:need to improve parameter:max_total_compete_sec"
                endif 
               this%competesec(countcompete) = compete(i,j)
               this%competesec_line(countcompete) = compete_lines(i,j)
               countcompete = countcompete + 1
               j = j + 1
            end do
        end if
        enddo 
        write(*,*) "total number compete = ",countcompete
!*****************************check line section code************************************
!   DO NOT DELETE THE FOLLOWING CODE
 !   write(*,*) "check compete section results"
 !   do i = 1, nl
 !       write(*,*)  "Link=",i," :", this%anode(i),"->",this%bnode(i)
 !       do j = this%locatecompete(i),this%locatecompete(i)+this%numcom(i) - 1 
 !           write(*,*) "  --Sec:", this%competesec(j),":", this%anode(this%competesec(j)),"->",&
 !               this%bnode(this%competesec(j)),"--Line:",this%competesec_line(j)
 !       end do 
 !   end do
 !
	!open(2,file='C:\GitCodes\LogitAssign\Input\TestNetwork\writecom1.txt')
	!open(3,file='C:\GitCodes\LogitAssign\Input\TestNetwork\writecom2.txt')
	!checkmaxcom = 0
	!do i =1,nl
	!	if (this%numcom(i)>checkmaxcom) then
	!		checkmaxcom = this%numcom(i)
	!	end if
    !       write(2,*) this%numcom(i)
    !       ! TODO : Ouput competing sections to check
    !       ! do j =1, 20
	!		! write(3,*) compete(i,(j-1)*50+1:j*50)
	!	! end do 
    !   end do   
    !   close(2)
    !   close(3)
    
!************************************************************************************************

    end subroutine 

    subroutine BFS_torder(this)
    implicit none 
    
    class(graphclass)::this
    integer::nr,l
    integer::u,arc,subarc
    integer::now_node
    integer::next_node(nn)
    integer::que(nn)
    integer::current_level
    integer::from_level(nn),from_node(nn)
    integer::arc_status(nl)
    integer::m,count
    integer::quepos,pop
    !step 1: ini
    this%toder_level = -1
    do nr=1, ndest
        current_level=0
        from_level = 0
        from_node = 0
        arc_status = 0
        que = 0
        do l = 1, nl
           if (this%sublink(l,nr)) then 
            arc_status(l) = 1
           endif
        end do
        now_node = this%roots(nr)
        quepos = 1
        from_level(now_node) = -1
        this%toder_level(now_node,nr) = current_level
        current_level = this%toder_level(now_node,nr) + 1
        do u = this%firstin(now_node), this%lastin(now_node)
            arc = this%backtoforward(u)
            m = this%anode(arc)
            if (.not.this%sublink(arc,nr)) then 
                CYCLE
            endif 
            arc_status(arc) = 0
            count = 0
            do l = this%firstin(m),this%lastin(m)
                subarc = this%backtoforward(l)
                count = count + arc_status(subarc)
            enddo
            from_level(m) = this%toder_level(now_node,nr)
            from_node(m) = now_node
            this%toder_level(m,nr) = this%toder_level(now_node,nr) + 1
            if (count.gt.0) then
                que(quepos) = m
                quepos = quepos + 1
            end if
        enddo 
        pop = 1
        do while (sum(que).gt.0)
            now_node = que(pop) 
            que(pop) = 0
            do u = this%firstin(now_node), this%lastin(now_node)
                arc = this%backtoforward(u)
                if (.not.this%sublink(arc,nr)) then 
                    CYCLE
                endif
                arc_status(arc) = 0
                m = this%anode(arc)
                if (this%toder_level(m,nr).eq.-1) then 
                    count = 0
                    do l = this%firstin(m),this%lastin(m)
                        arc = this%backtoforward(l)
                        count = count + arc_status(arc)
                    enddo
                    from_node(m) = now_node
                    from_level(m) = this%toder_level(now_node,nr)
                    this%toder_level(m,nr) =this%toder_level(now_node,nr) + 1
                    if (count.gt.0) then
                        que(quepos) = m
                        quepos = quepos + 1
                    end if
                end if
            enddo 
            pop = pop + 1
        end do
    end do
   
    

    
    end subroutine

    ! recursive subroutine dfs_visit(this,color,du,fu,pie,time, u,nr)
    ! implicit none 
    ! class(graphclass)::this
    ! integer::u,nr,v,arc
    ! integer::color(nn,ndest)
    ! integer::du(nn,ndest)
    ! integer::pie(nn,ndest)
    ! integer::fu(nn,ndest)
    ! integer::time
    ! color(u,nr) = 1
    ! time = time +1
    ! du(u,nr) = time
    ! do arc = this%firstout(u), this%lastout(u)
    !     if (this%sublink(arc,nr)) then
    !         v = this%bnode(arc)
    !         if (this%subnode(v,nr)) then 
    !             if (color(v,nr).eq.0) then 
    !                 pie(v,nr) = u
    !                 call this%DFS_visit(color,du,fu,pie,time,v,nr)
    !             end if
    !         endif 
    !     endif
    ! enddo
    ! color(u,nr) = 2
    ! ! time = time + 1
    ! fu(u,nr) = time

    ! end subroutine




    end module 

    real*8 function fact(n)
    implicit none
    integer j,n
    fact=1
    do j=2,n
	    fact=fact*j
	end do
	return
	end function
  
