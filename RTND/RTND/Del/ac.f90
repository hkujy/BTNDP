! implepemnt the archive lib 
! this will be included in the ABC solution
module ACL
    implicit none
    type, public::ArchivedClass
        integer::id
        integer::xPos, yPos  ! Postion of the boxes
        integer::BoxNum
        real*8::obj(2)   ! objectvive values
        integer,allocatable::fleet(:)
    contains
        procedure,pass::iniarchive=>iniarchive
        procedure,pass::delarchive=>delarchive
        procedure,pass::copy => copy
        procedure,pass::set => set
    end type 

    contains

    subroutine iniarchive(this)
        use constpara
        implicit none
        class(archivedclass):: this
        this%id = -1
        this%BoxNum = -1
        this%xPos = -1
        this%yPos = -1
        this%obj = -1
        if (.not.ALLOCATED(this%fleet)) then
            allocate(this%fleet(nline))
        end if
    end subroutine
    subroutine delarchive(this)
        implicit none 
        class(archivedclass):: this
        this%id = -1
        this%BoxNum = -1
        this%xPos = -1
        this%yPos = -1
        this%obj = -1
        deallocate(this%fleet)
    end subroutine

    subroutine set(this,input_fleet,dim)
        use constpara
        implicit none 
        CLASS(ArchivedClass)::this
        integer::dim
        integer,dimension(dim)::input_fleet
        integer::l
        !this%obj = sol%obj
        do l = 1, nline
            this%fleet(l) = input_fleet(l)
        end do 

    end subroutine 
    subroutine copy(this, rhs)
        implicit none 
        class(archivedclass)::this
        type(ArchivedClass)::rhs
        this%xPos = rhs%xPos
        this%yPos = rhs%yPos
        this%BoxNum = rhs%BoxNum
        this%obj = rhs%obj
        this%fleet= rhs%fleet
    end subroutine
 



end module
