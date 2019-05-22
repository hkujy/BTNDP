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
        integer::fleet(nline)
        real*8::fitness
        type(lineclass)::mylines(nline)
        type(dpsolver)::dp
        real*8::ttc
        real*8::fair
        real*8::odcost(nod)
    contains 
    procedure, pass::set_fleet_and_fre=>set_fleet_and_fre
    procedure, pass::evaluate=>evaluate
    procedure, pass::get_obj=>get_obj
    end type
    
    contains 
   
    subroutine set_fleet_and_fre(this, newfleet)
    use GraphLib
    implicit none
    class(solclass)::this
    integer,intent(in)::newfleet(nline)
    integer l
    do l=1, nline
        this%fleet(l) = newfleet(l)
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
    write(*,*) "Total Cost is", this%ttc

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
        this%ttc = this%ttc + this%odcost(w)
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
    module myclass
    use mylineclass
    use mysolclass
    end module
