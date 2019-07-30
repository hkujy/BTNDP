    ! the following code is referred to
    ! http://www.lahey.com/docs/lfenthelp/nlmovelconstructors.htm
    !todo
    module mysolclass
    use constpara
    use mylineclass
    use dpsolverlib
    implicit none
    type, public::solclass
        integer::id
        ! integer::fleet(nline)
        real*8::fitness
        type(lineclass),allocatable::mylines(:)
        type(dpsolver)::dp
        real*8::ttc
        real*8::fair
        real*8,allocatable::odcost(:)
    contains 
    procedure, pass::set_fleet_and_fre=>set_fleet_and_fre
    procedure, pass::generate=>generate
    procedure, pass::evaluate=>evaluate
    procedure, pass::get_obj=>get_obj
    procedure, pass::assign_fleet=>assign_fleet
    procedure, pass::remedy=>remedy
    procedure, pass::get_neigh=>get_neigh
    end type
    
    contains 
   
    subroutine set_fleet_and_fre(this, newfleet)
    use GraphLib
    implicit none
    class(solclass)::this
    integer,intent(in)::newfleet(nline)
    integer l
    do l=1, nline
        ! this%fleet(l) = newfleet(l)
        this%mylines(l)%fleet = newfleet(l)
        call this%mylines(l)%get_line_fre
    enddo
    end subroutine 


    subroutine evaluate(this,basenwk)
    use graphlib
    use dpsolverlib
    implicit none
    class(solclass),intent(inout)::this
    class(graphclass)::basenwk
    type(lineclass),DIMENSION(nline)::templines
    integer::l
 

    ! step 1: set lines 
    do l = 1, nline
        call templines(l)%copy(basenwk%mylines(l))
        call basenwk%mylines(l)%copy(this%mylines(l))
    end do 


    call this%dp%solver(basenwk)
    call this%get_obj
    write(*,*) "Total Cost = ", this%ttc

    do l = 1, nline
        call basenwk%mylines(l)%copy(templines(l))
    enddo 

    end subroutine

    subroutine get_od_cost(mydp, odpie)
    use dpsolverlib
    use GraphLib
    implicit none 
    integer::w,o,d,j,nr,l
    class(dpsolver)::mydp
    real*8::odpie(nod)
    do w = 1, nod
        o = mydp%nwk%origin(w)
        d = mydp%nwk%dest(w)
        do j= 1, ndest
            if (mydp%nwk%roots(j).eq.d) then 
                nr = j
                exit
            endif
        end do 
        do l = mydp%nwk%firstout(o),mydp%nwk%lastout(o)
            if (mydp%x(l,nr).gt.zero) then 
                odpie(w) = mydp%fx(l,nr)
                exit
            end if 
        enddo
    enddo 
    end subroutine

    subroutine get_obj(this)    
    use GraphLib
    use dpsolverlib
    implicit none
    class(solclass):: this
    integer::w
    this%ttc = 0

    call get_od_cost(this%dp, this%odcost)
    do w = 1, nod
        write(*,*) "OD = ", w, " Pie = ", this%odcost(w)
        this%ttc = this%ttc + this%odcost(w)*this%dp%nwk%demand(w)
    enddo 

    end subroutine

    subroutine generate(this)
    implicit none 
    ! type(solclass), intent(inout)::sol
    class(solclass)::this
    integer::remain,l,i
    integer::temp_sum
    ! this%fleet = fleet_lb
    do l =1, nline
        this%mylines(l)%fleet = fleet_lb(l)
    enddo 

    temp_sum = 0
    do l=  1, nline
        temp_sum = temp_sum + this%mylines(l)%fleet
    enddo
    remain = int(fleetsize - temp_sum)

    if (remain.le.0) then 
        write(*,*) "The lower bound is greater than the total fleet"
        pause
    endif 

    if (remain.ge.(sum(fleet_ub)-temp_sum))then 
        write(*,*) "Total fleet size is too large to be all allocated"
        write(*,*) "check file, abc.f90"
        pause
    end if

    ! call assign_remain(this%fleet)

    end subroutine

    subroutine assign_fleet(this,assignfleet)
    implicit none
    class(solclass)::this
    integer,INTENT(IN)::assignfleet
    integer::l, i
    integer::remain
    real*8::ran
    remain = assignfleet
    
    ! remain = fleetsize - sum(now)
    if (remain.eq.0) then 
        return 
    end if
    do i = 1, remain
5       call random_number(ran)
        l = int(ran*nline + 1)
        ! if (this%fleet(l) + 1.gt.fleet_ub(l)) then 
        if (this%mylines(l)%fleet + 1.gt.fleet_ub(l)) then 
            goto 5
        else 
            ! this%fleet(l) =this%fleet(l) + 1
            this%mylines(l)%fleet = this%mylines(l)%fleet + 1
        end if
    end do 
    end subroutine


    subroutine remedy(this)
    implicit none 
    class(solclass)::this
    integer::l
    integer::add_sum, reduce_sum
    logical::isRemedy
    isRemedy = .false.
    
    do l=1, nline
        ! if ((this%fleet(l).lt.fleet_lb(l)).or.(this%fleet(l).gt.fleet_ub(l))) then 
        if ((this%mylines(l)%fleet.lt.fleet_lb(l)).or. &
            (this%mylines(l)%fleet.gt.fleet_ub(l))) then 
            isRemedy = .true. 
            exit
        endif
    enddo 

    if (.not.isRemedy) then
        return 
    endif
    add_sum = 0
    reduce_sum = 0
    do l = 1,nline
        do while(this%mylines(l)%fleet.lt.fleet_lb(l))
            this%mylines(l)%fleet = this%mylines(l)%fleet + 1
            add_sum = add_sum + 1 
        end do
        do while(this%mylines(l)%fleet.gt.fleet_ub(l))
            this%mylines(l)%fleet = this%mylines(l)%fleet - 1
            reduce_sum = reduce_sum + 1
        end do
    end do 
    end subroutine

    subroutine get_neigh(this,replaced,basenwk)
    use mutelib
    use GraphLib
    implicit none 
    logical, INTENT(OUT)::replaced
    class(solclass)::this
    class(Graphclass)::basenwk
    integer::l
    ! integer,intent(in)::now
    type(lineclass),DIMENSION(nline)::templines
    real*8::temp_fit
    integer::neigh_fleet(nline),now_fleet(nline)

    do l = 1, nline
        call templines(l)%copy(this%dp%nwk%mylines(l))
        now_fleet(l) = this%mylines(l)%fleet
    end do 

    temp_fit = this%fitness
    ! genertate new fleet    
    call mute_increa(now_fleet, neigh_fleet)
    call this%set_fleet_and_fre(neigh_fleet)
    call this%evaluate(basenwk)

    ! switch back the new fitness values is wrose
    if (temp_fit.gt.this%fitness) then 
        replaced = .false.
        do l =1, nline
        call this%mylines(l)%copy(templines(l))
        end do
        this%fitness = temp_fit
    else
        replaced = .true.
    end if 

    do l = 1, nline
        call basenwk%mylines(l)%copy(templines(l))
    enddo 

    end subroutine 





    end module 






!    subroutine inilinks(ls)
!    ! initilize all links (ls stands for links)
!    ! based on the following data
!!  link	tail	head	line	time	var	fare
!!1	a	b	1	29	3	1.5
!!2	a	x	2	6	3	0.3
!!3	x	y	2	6	1	0.3
!!4	x	y	3	7	5	0.4
!!5	y	b	3	10	3	0.5
!!6	y	b	4	9	6	0.5
!
!    implicit none
!
!    integer l
!    type(linkclass),dimension(6), intent(inout)::ls
!
!    do l = 1,6
!        ls(l)%id = l
!    enddo
!    ls(1)%tail = "a"
!    ls(1)%head = "b"
!    ls(1)%line = 1
!    ls(1)%time = 29
!    ls(1)%var = 3
!    ls(1)%fare = 1.5
!
!    ls(2)%tail = "a"
!    ls(2)%head = "x"
!    ls(2)%line = 2
!    ls(2)%time = 6
!    ls(2)%var = 3
!    ls(2)%fare = 0.3
!
!    ls(3)%tail = "x"
!    ls(3)%head = "y"
!    ls(3)%line = 2
!    ls(3)%time = 6
!    ls(3)%var = 1
!    ls(3)%fare = 0.3
!
!
!    ls(4)%tail = "x"
!    ls(4)%head = "y"
!    ls(4)%line = 3
!    ls(4)%time = 7
!    ls(4)%var = 5
!    ls(4)%fare = 0.4
!
!
!    ls(5)%tail = "y"
!    ls(5)%head = "b"
!    ls(5)%line = 3
!    ls(5)%time = 10
!    ls(5)%var = 3
!    ls(5)%fare = 0.5
!
!
!    ls(6)%tail = "y"
!    ls(6)%head = "b"
!    ls(6)%line = 4
!    ls(6)%time = 9
!    ls(6)%var = 6
!    ls(6)%fare = 0.5
!
!    end subroutine
!
!    end module
