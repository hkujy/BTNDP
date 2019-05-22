    module GraphLib
    use constpara 
    use mylineclass
    implicit none 

    type, public::graphclass
        logical::connect(nn,nn,ndest)
        integer::numconnect(nn,nn,ndest)
        integer::roots(ndest)
        integer::torder(nn,ndest) ! topoloy oreder
        logical::sublink(nl,ndest),subnode(nn,ndest)
        integer::firstin(nn),lastin(nn)
        integer::firstout(nn),lastout(nn)
        integer::anode(nl),bnode(nl)
        integer::backanode(nl),backbnode(nl)
        integer::backtoforward(nl)
        integer::dest(nod),origin(nod)
        integer::locatecompete(nl+1)
        integer::competesec(maxcom)
        real*8::scost(nl),svar(nl) ! section cost and section variance
        integer::numcom(nl)
        real*8::fare(nl)
        real*8::demand(nod)
        integer::linestops(nline)
        integer::line(nline,maxlinestops)   ! bus lines
        real*8::fre(nline)
        real*8::sf(nl)       ! section frequency sum
        real*8::slf(nl,nline)   ! section frequency sum: section line frequency
        integer sindex(nl)! relate section and link index
        integer::sl(nl,maxsecline)
        integer::slc(nl)
        integer::caseindex=1
        !real*8:: tsl(8,4) ! cost, var, fare, fre
        real*8::ndist(nn,ndest)		! the shortest distance from node all
        integer pa(nn)
        logical::isUEconverge
        type(lineclass),dimension(nline)::mylines
    contains

    procedure,pass::readnwt=>readnwt
    procedure,pass::minspantree=>minspantree
    procedure,pass::getorder=>getorder
    procedure,pass::order=>order
    procedure,pass::link_time=>link_time
    procedure,pass::countconect=>countconect
    procedure,pass::updatesub=>updatesub
    procedure,pass::updatesub_bcm=>updatesub_bcm
    procedure,pass::update_section_cost=>update_section_cost
    procedure,pass::update_sec_fre=>update_sec_fre
    procedure,pass::copynwk=>copynwk

    end type graphclass
    
    contains

    subroutine readnwt(this,rhsnwk)
    use mylineclass
    implicit none
   
    class(graphclass)::this 
    class(graphclass),OPTIONAL::rhsnwk
    integer::i,j,k,o,d,countline,odpair,l,countcompete
    integer::compete(nl,maxcom), temp_compete(nl,maxcom)
    real*8::dta(4)  ! temperal data for reading file
    real*8::lcost(nn,nn)	! link travel cost matrix each pair of node
    real*8::lvar(nn,nn)     ! link variance
    real*8::tempreal	! section cost
    real*8::tfa(nl),tva(nl)     ! seems to be a temp cost
    integer::sindexreverse(nl)  ! this only for the small network to compute
    !integer::head,tail,head1,head2
    ! tail --> head
    !integer::section(nn,nn)
    integer::s,s1,s2,m,mstop,now,next
    integer::sc	        !section counter
    integer::section_line(nl,maxsecline) ! section_line index
    integer::an(nl),bn(nl)  ! head and tail node based on section index
    integer::section_line_count(nl)! section line counter
    integer::inputstops(maxlinestops)
    integer, allocatable :: newseed (:), oldseed(:)
    integer::tempnumcom(nl)
    real*8::inifre(nline)


    if (present(rhsnwk)) then 
        call this%copynwk(rhsnwk)
        call gc_update_secfre(this%fre,this%slc,this%sl,this%sf,this%slf)
        call this%update_section_cost
    end if 



    call random_seed ( )  ! processor reinitializes the seed
    ! randomly from the date and time
    call random_seed (size = i)  ! i is set to the size of
    ! the seed array
    allocate (newseed(i))
    allocate (oldseed(i))
    call random_seed (get=oldseed(1:i)) ! gets the current seed
    newseed = inputseed
    call random_seed (put=newseed(1:i)) ! sets seed from array
    !		do i =1, 10
    call random_number(tempreal)
    !		write(*,*) r
    !		end do

    !open(1,file='..\input\trips.txt')
    !open(1,file='..\largenetworkinput\alllinks.txt')
    !open(1,file='..\testnetwork\alllinks.txt')
    !open(2,file='..\input\cost.txt')
    !open(3,file='..\input\demand.txt')
    open(1,file='c:\gitcodes\BTNDP\input\testnetwork\odpairs.txt')
    open(2,file='c:\gitcodes\BTNDP\input\testnetwork\numlinestops.txt')
    open(3,file='c:\gitcodes\BTNDP\input\testnetwork\putstops.txt')
    open(4,file='c:\gitcodes\BTNDP\input\testnetwork\destnodeset.txt')
    !open(5,file='c:\gitcodes\BTNDP\input\testnetwork\linesectiondata.txt')
    open(6,file='c:\gitcodes\BTNDP\input\testnetwork\inifre.txt')
    open(7,file='c:\gitcodes\BTNDP\results\fortran_6linkdata.txt')
    open(8,file='c:\gitcodes\BTNDP\results\fortran_linecostandvar.txt')
    ! step 1
    ! read od pairs
    do i=1,nod
        read(1,*) dta(:)
        odpair=dta(1)
        this%origin(odpair)=dta(2)
        this%dest(odpair)=dta(3)
        this%demand(odpair)=dta(4)
    enddo
    close(1)
    ! step 2: read line and stops

    !read line stops number
    do i = 1, nline
        read(2,*) j,k
        this%linestops(j) = k
    enddo
    close(2)

    !read line stops
    do i = 1, nline
        inputstops = 0
        read(3,*) inputstops(1:this%linestops(i))
        do j = 1, this%linestops(i)
            this%line(i,j) = inputstops(j)
        end do
    enddo
    do i = 1, nline
        do j = 1, this%linestops(i)
            if (this%line(i,j)==0) then
                write(*,*) "input line s =0"
            endif
        enddo
    enddo
    close(3)

    !read root nodes for the assignment

    do i = 1,ndest
        read(4,*) j,k
        this%roots(j) = k
    enddo
    close(4)

    ! read line sectine cost variance and fare data
    !do i = 1, 8
    !    read(5,*) this%tsl(i,1:3)   ! cost, var, fare
    !enddo
    !close(5)
    ! this is the initial frequency setting
    do i = 1, nline
        read(6,*) tempreal
        this%fre(i) = tempreal
        !inifre(i) = tempreal
    enddo
    this%fre=this%fre/60.0	  !frequency per minute
    close(6)

    ! TODO create route section network with line data
    this%sindex=0
    this%slc=0
    this%sl=0
    an=0
    bn=0
    this%line=0


    ! network structure
    an(1) = 1
    bn(1) = 4
    this%slc(1) = 1
    this%sl(1,1) = 1

    an(2) = 1
    bn(2) = 2
    this%slc(2) = 1
    this%sl(2,1) = 2

    an(3) = 2
    bn(3) = 3
    this%slc(3) = 2
    this%sl(3,1) = 2
    this%sl(3,2) = 3

    an(4) = 3
    bn(4) = 4
    this%slc(4) = 2
    this%sl(4,1) = 3
    this%sl(4,2) = 4

    an(5) = 1
    bn(5) = 3
    this%slc(5) = 1
    this%sl(5,1) = 2

    an(6) = 2
    bn(6) = 4
    this%slc(6) = 1
    this%sl(6,1) = 3

    ! input compete section index
    !compete=0  record the number of compete sections
    this%numcom = 0
    compete = 0

    this%numcom(2) = 1
    compete(2,1) = 5
    this%numcom(3) = 2
    compete(3,1) = 6
    compete(3,2) = 5
    this%numcom(4) = 1
    compete(4,1) = 6
    this%numcom(5) = 1
    compete(5,1) = 2
    this%numcom(6) = 1
    compete(6,1) = 3
    
    !call gc_updatesectioncost(this%fre,this%tsl,this%scost,this%svar,this%fare)
    this%anode = an
    this%bnode = bn
    call ini_lines(this%mylines)
    call gc_update_secfre(this%fre,this%slc,this%sl,this%sf,this%slf)
    call this%update_section_cost


    
    
    !subroutine updatesectioncost(fre,tsl,scost,svar,fare)

    !***************read link cost and set link var***************************
    !************************************************
    write(7,*) "scost,svar,fare" 
    do i = 1, size(this%scost)
        write (7,'(f6.2,a,f6.2,a,f6.2)') this%scost(i),',', this%svar(i),',',this%fare(i)
    enddo
    close(7)

    ! this just initial cost
    lcost(1,4) = this%scost(1)
    lcost(1,2) = this%scost(2)
    lcost(2,3) = this%scost(3)
    lcost(3,4) = this%scost(4)
    lcost(1,3) = this%scost(5)
    lcost(2,4) = this%scost(6)


    lvar(1,4) = this%svar(1)
    lvar(1,2) = this%svar(2)
    lvar(2,3) = this%svar(3)
    lvar(3,4) = this%svar(4)
    lvar(1,3) = this%svar(5)
    lvar(2,4) = this%svar(6)


    !compete (i.j) the jth completive section in section i
    !call gc_update_secfre(this%fre,this%slc,this%sl,this%sf,this%slf)

    !subroutine update_secfre(fre,sl,sf,slf)
    !subroutine update_secfre(fre,sf)

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

    ! add code to revise compete

    sindexreverse(:)=(/3,1,4,6,2,5/)
    do i=1,nl
        s=this%sindex(i)
        tempnumcom(i) = this%numcom(s)
        do j =1, maxcom
            if (compete(s,j) > 0) then
                temp_compete(i,j)=sindexreverse(compete(s,j))
            endif
        enddo
    enddo
    compete = temp_compete
    this%numcom =tempnumcom


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



    !***************************************************************
    ! todo: check the dimension of sl and section_line

    !sf=0.0
    !do i=1,nl
    !    do j=1,slc(i)
    !        sf(i)=sf(i)+fre(sl(i,j))
    !    enddo
    !    ! if sf = 0, then it is walking link
    !    if (sf(i).eq.0) then
    !        sf(i)=1000000.0
    !    end if
    !enddo
    !
    !slf=0.0
    !do i=1,nl
    !    do j=1,slc(i)
    !        slf(i,sl(i,j))=fre(sl(i,j))/sf(i)
    !    enddo
    !end do


    this%scost=tfa
    this%svar=tva
    do i=1,nn
        do j =this%firstout(i),this%lastout(i)
            if ((i.ne.this%anode(j))) then
                write(*,*) "anode(j) err"
            end if
        end do
    end do

    do i=1,nn
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

    end subroutine

    

    
    
    subroutine gc_update_secfre(fre,slc,sl,sf,slf)
    use constpara
    implicit none
    integer i,j
    real*8,intent(in)::fre(nline)
    integer,intent(in)::slc(nl)
    integer,intent(in)::sl(nl,maxsecline)
    real*8,intent(inout)::sf(nl)
    real*8,intent(inout)::slf(nl,nline)

    sf=0.0
    do i=1,nl
        do j=1,slc(i)
            sf(i)=sf(i)+fre(sl(i,j))
        enddo
        ! if sf = 0, then it is walking link
        if (sf(i).eq.0) then
            sf(i)=1000000.0
        end if
    enddo

    slf=0.0
    do i=1,nl
        do j=1,slc(i)
            slf(i,sl(i,j))=fre(sl(i,j))/sf(i)
        enddo
    end do
    end subroutine

    ! get topoplogical order of the network 
    subroutine getorder(this)
    use constpara
    implicit none
    class(graphclass)::this
    integer::nr,i
    integer::node, link, j
    
    
    do nr = 1,ndest
        call this%order(nr,this%ndist(:,nr))
    end do 
! find the laregest cost lable
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
    subroutine updatesub_bcm(this,lt,del,xfa,fx)
    implicit none 
    class(graphclass)::this
    real*8,intent(in)::lt(nl)
    logical,intent(in)::del
    real*8,intent(in)::xfa(nl,ndest)
    real*8,intent(in)::fx(nl,ndest)
    integer::nr,l,a,b
 
    ! step 1: del link
    if(.not.del) then 
        return 
    endif 
    do nr = 1,ndest
        do l = 1, nl
            if (this%sublink(l,nr)) then 
                if ((xfa(l,nr).le.0.1*flow_eps).and.(fx(l,nr).lt.large)) then   
                    a = this%anode(l)
                    b = this%bnode(l)
                    ! this is importance to maintain the connection of the network 
                    if (this%connect(a,b,nr).and.(this%numconnect(a,b,nr)-1.ge.1)) then
                        this%sublink(l,nr) = .false.
                        this%numconnect(a,b,nr) = this%numconnect(a,b,nr) - 1
                        write(*,*) "Del link = ",nr
                    end if 
                end if
            end if 
        enddo 
    enddo 
    end subroutine


    
    subroutine updatesub(this,lt,del,xfa)
    implicit none 
    
    class(graphclass)::this
	!step 
    real*8,intent(in)::lt(nl)
    logical,intent(in)::del
    real*8,intent(in)::xfa(nl,ndest)
    integer::nr,node,i,j,link,a,b,l
! first add new links 	
    if (isusebcm) then 
        !this%updatesub_bcm(lt,det,xfa)
        RETURN
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

    if (islogit.or.isusebcm) then 
        return 
    end if
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
    
    subroutine minspantree(this)
    implicit  none
    class(graphclass)::this
    integer::root,nr

    do nr =1,ndest
        root = this%roots(nr)
        call rsp(root,this%scost,this%firstin,this%lastin,this%pa,this%backbnode,this%backtoforward)
        !subroutine rsp(r,link_time,firstout,lastout,pa,backbnode,backtoforward)
		this%ndist(:,nr)=dist(:)  ! Shortest distance
    enddo
    end subroutine
    
    subroutine countconect(this)
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
    real*8,INTENT(OUT)::linktime(nl)
    real*8::vehicle_mean(nl),vehicle_var(nl),wait_mean(nl),wait_var(nl),congest_mean(nl),congest_var(nl)
    real*8::cflow(nl)
    integer::i,j,l1,l2
    real*8::fact
    integer::n,kj

    n = congestion_n
    vehicle_mean = this%scost
    vehicle_var = this%svar
    wait_mean=1/this%sf
    wait_var=(1/this%sf)**2

    cflow=0.0
    do i=1,nl
        cflow(i)=cflow(i)+linkflow(i)  !own section
	!	do kj=1,numcom(i)
        if (this%numcom(i)>0) then  ! num of competting section 1
            do kj = this%locatecompete(i),this%locatecompete(i) + this%numcom(i) - 1
                !j=compete(i,kj)
                j = this%competesec(kj)
                do l1=1,this%slc(i)
                    do l2=1,this%slc(j)
                        if (this%sl(i,l1)==this%sl(j,l2).and.this%sl(i,l1)/=0) then 
                            cflow(i)=cflow(i)+linkflow(j)*this%fre(this%sl(i,l1))/this%sf(j)
                        endif 
                    enddo 
                enddo 
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
    use mylineclass
    implicit none 
    class(graphclass)::this
    integer::tail,head,lid
    real*8::t,v,f,s,l
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
    integer::l
   
    call gc_update_secfre(this%fre,this%slc,this%sl,this%sf,this%slf)
    
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
        this%linestops = rhs%linestops
        this%line = rhs%line
        this%fre = rhs%fre
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
  
