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
    contains 
    procedure, pass::set_fleet_and_fre=>set_fleet_and_fre
    procedure, pass::evaluate=>evaluate
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
        call this%mylines(l)%get_line_fre(this%dp%nwk)
    enddo
    end subroutine 


    subroutine evaluate(this)
    use graphlib
    use dpsolverlib
    implicit none
    class(solclass),intent(inout)::this
    
    call this%dp%solver
    end subroutine
    
    end module 
!
!  
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

   module mylineclass ! the module name defines the namespace
   use constpara
   type lineclass ! classname is the class prototype name
       integer::id
       real*8::fre
       integer::fleet
       real*8::exptime
       real*8::vartime
       !integer, dimension(:),allocatable::linksid    !
    contains
    procedure, pass::get_line_time=>get_line_time
    procedure, pass::get_line_fre =>get_line_fre
    procedure, pass::get_fleet => get_fleet
   end type lineclass

   contains

   subroutine ini_lines(mylines)
   implicit none
   type(lineclass), dimension(nline),intent(inout)::mylines
   integer::l
   do l = 1, nline
       mylines(l)%id = l
   enddo 
   end subroutine


   subroutine get_line_time(this,nwk)
   use GraphLib
   use constpara
   implicit none
   type(graphclass), intent(in)::nwk
   class(lineclass)::this
   ! this purpose of this subroutine is to get the expected travel time of lines
   integer::l
   !linksid
   this%exptime = 0
   this%vartime = 0
   
   select case(this%id) 
   case(1)
    this%exptime = 29
    this%vartime = 3
   case(2)
    this%exptime = 12
    this%vartime = 4
   case(3)
    this%exptime = 17
    this%vartime = 8
   case(4)
    this%exptime = 9
    this%vartime = 6
   end select


   return
   !do l=1, size(this%linksid)
   do l=1, size(line_links(this%id,:))
       if (line_links(this%id,l).lt.0) exit
       this%exptime =this%exptime + nwk%scost(line_links(this%id,l))
       this%vartime = this%vartime + nwk%svar(line_links(this%id,l))
   enddo

   end subroutine

    subroutine get_line_fre(this,nwk)
    use GraphLib
    implicit none
    class(graphclass), intent(in)::nwk
    class(lineclass)::this
    call this%get_line_time(nwk)
   ! computet the frequency
    this%fre = 60 * (this%fleet / this%exptime) *(1 + this%vartime/(this%exptime**2))
    end subroutine

    subroutine get_fleet(this,nwk,fre)
    use GraphLib
    implicit none
    class(lineclass)::this
    real*8, intent(in)::fre
    class(graphclass),INTENT(IN)::nwk
    call this%get_line_time(nwk)
    this%fleet = ceiling((this%exptime/60)*(fre)/(1+this%vartime/(this%exptime**2)))   


    end subroutine


    end module

    module myclass
    use mylineclass
    use mysolclass
    end module
