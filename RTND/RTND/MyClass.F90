    ! the following code is referred to
    ! http://www.lahey.com/docs/lfenthelp/NLMOvElConstructors.htm
    !TODO

    
    module MySolClass
    USE DEFINE
    implicit none
    type SolClass
        integer::ID
        integer::Fleet(NLINE)
        real*8::Fitness
    endtype
    contains 
    
    subroutine set_sol_fre(this, newFleet)
    ! set fleet size for the solution
    type(SolClass),intent(inout)::this
    
    integer l
    integer,intent(in)::NewFleet(Nline)
    do l=1, NLINE
        this%Fleet(l) = newFleet(l)
    enddo
    
    end subroutine 
    
    subroutine EvaluateSol(this,FIRSTOUT,LASTOUT,XFA)
    implicit none
    type(SolClass),intent(inout)::this
    INTEGER, INTENT(IN):: FIRSTOUT(NN), LASTOUT(NN)
    REAL*8, INTENT(INOUT)::XFA(NL,NDEST)
    call Test_One_Fleet_Sol(this%Fleet,FIRSTOUT,LASTOUT,XFA)

    end subroutine

    
    end module 
    
    module MyLinkClass
    type LinkClass
        integer::ID
        character(len=4)::Tail
        character(len=4)::Head
        Integer::Line
        Real*8::Time
        real*8::Var
        real*8::Fare
    end type
    contains

    subroutine ini_one_link(this,i_tail,i_head,i_line,i_time)
    implicit none
    type(LinkClass),intent(inout)::this

    character(len=4)::i_Tail
    character(len=4)::i_Head
    Integer::i_Line
    Real*8::i_Time

    this%tail = i_tail
    this%head = i_head
    this%line = i_line
    this%time = i_time

    end subroutine
    subroutine IniLinks(ls)
    ! initilize all links (ls stands for links)
    ! based on the following data
!  Link	Tail	Head	Line	Time	vAR	fare
!1	A	B	1	29	3	1.5
!2	A	X	2	6	3	0.3
!3	X	Y	2	6	1	0.3
!4	X	Y	3	7	5	0.4
!5	Y	B	3	10	3	0.5
!6	Y	B	4	9	6	0.5

    
    
    implicit none

    integer l
    type(LinkClass),DIMENSION(6), intent(inout)::ls

    do l = 1,6
        ls(l)%id = l
    enddo
    ls(1)%Tail = "A"
    ls(1)%Head = "B"
    ls(1)%Line = 1
    ls(1)%Time = 29
    ls(1)%Var = 3
    ls(1)%Fare = 1.5

    ls(2)%Tail = "A"
    ls(2)%Head = "X"
    ls(2)%Line = 2
    ls(2)%Time = 6
    ls(2)%Var = 3
    ls(2)%Fare = 0.3

    ls(3)%Tail = "X"
    ls(3)%Head = "Y"
    ls(3)%Line = 2
    ls(3)%Time = 6
    ls(3)%Var = 1
    ls(3)%Fare = 0.3


    ls(4)%Tail = "X"
    ls(4)%Head = "Y"
    ls(4)%Line = 3
    ls(4)%Time = 7
    ls(4)%Var = 5
    ls(4)%Fare = 0.4


    ls(5)%Tail = "Y"
    ls(5)%Head = "B"
    ls(5)%Line = 3
    ls(5)%Time = 10
    ls(5)%Var = 3
    ls(5)%Fare = 0.5


    ls(6)%Tail = "Y"
    ls(6)%Head = "B"
    ls(6)%Line = 4
    ls(6)%Time = 9
    ls(6)%Var = 6
    ls(6)%Fare = 0.5

    end subroutine

    end module

    module MyLineClass ! the module name defines the namespace
    use MyLinkClass
    use ConstPara
    type LineClass ! classname is the class prototype name
        integer::ID
        real*8::Fre
        integer::Fleet
        real*8::ExpTime
        real*8::VarTime
        !Integer, DIMENSION(:),allocatable::LinksID    !
        Type(LinkClass), DIMENSION(:),allocatable::Links  !
    end type lineClass

    contains

    subroutine Ini_MyLines(i_mylines)
    implicit none
    type(LineClass), dimension(Nline),intent(inout)::i_mylines
    integer::l
    do l = 1, NLine
        i_mylines(l)%id = l
    enddo 

    end subroutine

    subroutine ini_line_links(this,i_links)
    implicit none
    type(LineClass)::this
    type(LinkClass),dimension(NL):: i_links
    integer:: l
    ! to do, input the links associated with the lines
    !    Lines	links
    !1, (1)
    !2, (2	3)
    !3, (4	5)
    !4, (6)

    select case (this%id)
    case (1)
        allocate(this%links(1))
        this%links(1) =  i_links(1)
        !write(*,*) this%links(1
    case (2)
        allocate(this%links(2))
        this%links(1) = i_links(2)
        this%links(2) = i_links(3)
        write(*,*) this%id
    case (3)
        allocate(this%links(2))
        this%links(1) =  i_links(4)
        this%links(2) = i_links(5)
        write(*,*) this%id
    case (4)
        allocate(this%links(1))
        this%links(1) = i_links(6)
        write(*,*) this%id
    end select
    call get_line_time(this)

    end subroutine

    subroutine get_line_time(this)
    implicit none
    type(LineClass)::this
    ! this purpose of this subroutine is to get the expected travel time of lines
    integer::l
    !LinksID
    this%exptime = 0
    this%vartime = 0
    !do l=1, size(this%LinksID)
    do l=1, size(this%Links)
        this%exptime =this%exptime + this%links(l)%Time
        this%Vartime = this%vartime + this%links(l)%Var
    enddo

    end subroutine

    subroutine get_line_fre(this)
    implicit none
    type(LineClass)::this
    call get_line_time(this)
    ! computet the frequency
        this%fre = 60 * (this%Fleet / this%exptime) *(1 + this%VarTime/(this%exptime**2))
    end subroutine

    end module

    module MyClassModule
    use MyLineClass
    use MyLinkClass
    use MySolClass
    end module
