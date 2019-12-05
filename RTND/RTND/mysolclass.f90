    
    module mysolclass
    use constpara
    use mylineclass
    use dpsolverlib
    implicit none
    type,public::solclass
        integer::id
        real*8::fitness
        type(lineclass),allocatable::mylines(:)
        type(dpsolver)::dp
        integer::NumBeat ! Number of solutions dominated by this
        integer::NumLoss ! Number of solution  that are noted domintaed
        real*8::obj(2)  ! Two objectives
                        ! 1: Total cost, 2: fairness values
        real*8,allocatable::odcost(:)
    contains 
    procedure, pass::update_fleet_and_fre=>update_fleet_and_fre
    procedure, pass::generate=>generate
    procedure, pass::evaluate=>evaluate
    procedure, pass::get_obj_ttc=>get_obj_ttc
    procedure, pass::get_obj_fare=>get_obj_fare
    procedure, pass::assign_remain_fleet=>assign_remain_fleet
    procedure, pass::remedy=>remedy
    procedure, pass::get_neigh=>get_neigh
    procedure, pass::inisol=>inisol 
    procedure, pass::delsol=>delsol
    procedure, pass::printsol=>printsol
    procedure, pass::compare=>compare
    procedure, pass::add_to_Archive=>add_to_Archive
    !procedure, pass::isDominated =>isDominated  ! function is dominated or not
    end type
    
    type, public::ArchivedClass
        integer::id
        integer::xPos, yPos  ! Postion of the boxes
        integer::BoxNum
        real*8::obj(2)   ! objectvive values
        integer,allocatable::fleet(:)
    contains
        procedure,pass::iniarchive=>iniarchive
        procedure,pass::ClearArchive=>clearArchive
        procedure,pass::copyAcs => copyAcs
        procedure,pass::set => set
    end type 
    
    contains 
   
    function comparevec(vec1,vec2) result(status)
        implicit none 
        real*8,INTENT(IN)::vec1(2)
        real*8,INTENT(IN)::vec2(2)
        integer::status
        status = -1
        ! case 1 
        if ((vec1(1).lt.vec2(1)).and.(vec1(2).ge.vec2(2))) then 
            status = 1
            return 
        end if 
        if ((vec1(1).le.vec2(1)).and.(vec1(2).gt.vec2(2))) then 
            status = 1
            return
        end if

        if ((vec1(1).gt.vec2(1)).and.(vec1(2).le.vec2(2))) then 
            status = 2
            return 
        end if
        if ((vec1(1).ge.vec2(1)).and.(vec1(2).lt.vec2(2))) then 
            status = 2
            RETURN
        endif
        if ((vec1(1).lt.vec2(1)).and.(vec1(2).lt.vec2(2))) then 
            status = 3
            return 
        end if
        if ((vec1(1).gt.vec2(1)).and.(vec1(2).gt.vec2(2))) then
            status =  3
            return 
        end if
        if ((abs(vec1(1)-vec2(1)).lt.1E-6).and.(abs(vec1(2)-vec2(2)).lt.1E-6)) then 
            status = 3 
            return 
        end if
        if (status.eq.-1) then
            write(*,*) "this =",vec1(1), vec1(2) 
            write(*,*) "rhs =",vec2(1), vec2(2)
            write(*,*) "undetermined dominate relationship"
            pause
        end if
    end function

    function compare(this, rhs) result (status)
        implicit none
        ! 1 : win, 2:lose , 3: both nondominated 
        class(solclass)::this
        type(solclass),intent(in)::rhs 
        integer::status
        status = -1
        status = comparevec(this%obj,RHS%obj)
    end function

    subroutine inisol(this,basenwk)
      implicit none 
      class(solclass)::this
      type(graphclass),intent(in)::basenwk
      integer::l
      this%fitness = -99
      if(.not.allocated(this%odcost)) then 
        allocate(this%odcost(nod))
      end if
      if(.not.allocated(this%mylines)) then
        allocate(this%mylines(nline))
      endif
    
      do l = 1, nline
          call this%mylines(l)%copylines(basenwk%mylines(l))
      enddo

    end subroutine

    subroutine delsol(this)
    implicit none 
    class(solclass)::this
    this%fitness = -99
    deallocate(this%odcost)
    deallocate(this%mylines)
    call this%dp%del
    end subroutine

    subroutine update_fleet_and_fre(this, newfleet)
    use GraphLib
    implicit none
    class(solclass)::this
    integer,intent(in)::newfleet(nline)
    integer l
    do l=1, nline
        this%mylines(l)%fleet = newfleet(l)
        call this%mylines(l)%get_line_fre
    enddo
    end subroutine 

    subroutine evaluate(this,basenwk, baseSol)
      use dpsolverlib
      implicit none
      class(solclass),intent(inout)::this
      type(graphclass)::basenwk
      type(solclass),optional::basesol
      type(lineclass),dimension(nline)::templines
      integer::l
 
    ! step 1: set lines 
      do l = 1, nline
          call templines(l)%copylines(basenwk%mylines(l))
          call basenwk%mylines(l)%copylines(this%mylines(l))
      end do 

      call this%dp%ini
      call this%dp%solver(basenwk)
      call get_od_cost(this%dp, this%odcost)
      call this%get_obj_ttc
      if (present(basesol)) then 
          call this%get_obj_fare(BaseSol)
      end if
      if (isWriteDug) then 
          call this%printsol
      end if

      do l = 1, nline
          call basenwk%mylines(l)%copylines(templines(l))
      enddo 

    end subroutine


    ! get OD cost from Dissolution
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

    subroutine get_obj_ttc(this)    
    use GraphLib
    use dpsolverlib
    implicit none
    class(solclass):: this
    integer::w
    this%obj(1) = 0
    do w = 1, nod
        if (isWriteDug) then 
            write(*,*) "OD = ", w, " Pie = ", this%odcost(w)
        endif 
        this%obj(1)= this%obj(1) + this%odcost(w)*this%dp%nwk%demand(w)
    enddo 

    end subroutine

    subroutine get_obj_fare(this,BaseSol)
    implicit none

    class(solclass)::this
    type(solclass), INTENT(IN)::BaseSol
    integer::w
    this%obj(2) = 100000 
    do w = 1, nod
        this%obj(2) = min(this%obj(2), BaseSol%odcost(w)-this%odcost(w))
    end do 
     
    end subroutine


    subroutine generate(this,basenwk)
    implicit none 
    class(solclass)::this
    type(graphclass),intent(in)::basenwk
    integer::remain,l,i
    integer::temp_sum
    call this%inisol(basenwk)
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
        write(*,*) "check file, mysolclass.f90"
        pause
    endif 

    if (remain.ge.(sum(fleet_ub)-temp_sum))then 
        write(*,*) "Total fleet size is too large to be all allocated"
        write(*,*) "check file, mysolclass.f90"
        pause
    end if

    call this%assign_remain_fleet(remain)

    end subroutine

    subroutine assign_remain_fleet(this,remain)
    ! random select one line to add the additional fleet
    implicit none
    class(solclass)::this
    integer,intent(in)::remain
    integer::l, i
    real*8::ran
    
    if (remain.eq.0) then 
        return 
    end if

    do i = 1, remain
5       call random_number(ran)
        l = int(ran*nline + 1)
        if (this%mylines(l)%fleet + 1.gt.fleet_ub(l)) then 
            goto 5
        else 
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

    function get_neigh(this,basenwk,baseSol,AcSols,AcDim,LastIndex) result(isIncreaLimit)
        use mutelib
        use GraphLib
        implicit none 
        integer::domStatus   !1, win, 2 loss, 3 non-dom
        logical::isIncreaLimit
        class(solclass)::this
        type(solclass)::baseSol
        integer,intent(in)::acdim
        type(ArchivedClass),DIMENSION(AcDim)::AcSols
        type(lineclass),dimension(nline)::templines
        type(graphclass)::basenwk
        integer::LastIndex,l
        real*8::oldobj(2)
        integer::neigh_fleet(nline),now_fleet(nline)
        integer::stausval
        logical::addArchiveSatus
        do l = 1, nline
            call templines(l)%copylines(this%dp%nwk%mylines(l))
            now_fleet(l) = this%mylines(l)%fleet
        end do 
        oldobj = this%obj
        ! generate new fleet    
        call mutation_main(now_fleet,neigh_fleet)
        call this%update_fleet_and_fre(neigh_fleet)
        call this%evaluate(basenwk,baseSol)

        !domStatus = comparevec(oldobj,this%obj)
        domStatus = comparevec(this%obj,oldobj)
        select case (domStatus)
        case(1)
            addArchiveSatus = this%add_to_Archive(AcSols,AcDim,LastIndex)
            isIncreaLimit =.false.
        case(2)
            do l =1, nline
                call this%mylines(l)%copylines(templines(l))
            end do
            isIncreaLimit = .true.
        case(3)
            addArchiveSatus = this%add_to_Archive(AcSols,AcDim,LastIndex)
            if (addArchiveSatus) then 
                isIncreaLimit = .false.
            else
                isIncreaLimit = .true.
            end if
        end select
    end function


    subroutine printsol(this)
        implicit none
        class(solclass)::this
        integer::l
        write(*,*) "fleet ="
        do l = 1, nline
            write(*,*) this%mylines(l)%fleet
        end do
        write(*,*) "Od cost = "
        do l= 1, nod
            write(*,*) l, this%odcost(l)
        enddo
        write(*,*) "obj(1)=",this%obj(1)
        write(*,*) "obj(2)=",this%obj(2)
    end subroutine


    function add_to_Archive(this,AcSols,dim,LastIndex) result(isAdd)
        implicit none 
        class(solclass)::this
        integer,intent(in)::dim
        integer,intent(inout)::LastIndex
        type(archivedclass),dimension(dim)::AcSols
        type(archivedclass),dimension(dim)::TempAcsols
        logical::isAdd
        integer::i,statuval,AcNum
        logical,dimension(lastindex)::iskeep

        if (LastIndex.eq.0) then 
            call acsols(1)%set(this)
            LastIndex = 1
            isAdd = .true.
            return
        end if

        iskeep = .true.
        isAdd = .true.
        do i = 1, LastIndex
            ! compare with the AcSol
            statuval = comparevec(this%obj,acsols(i)%obj)
            ! 1 : win, 2:lose , 3: both nondominated 
            select case (statuval)
            case(1)
                iskeep(i) =.false.  ! delete the dominated solutio 
            case(2)
                ! the solution is domianted by some exsiting solutions 
                isAdd = .false.
            case(3)
                continue
                ! case 3: this solution is non dominated with any of other solutions
            end select
        end do 
        if (isAdd) then 
            do i = 1, LastIndex
                call TempAcsols(i)%copyAcs(acsols(i))
                call acsols(i)%ClearArchive
            end do
            AcNum = 1
            do i = 1, LastIndex
               if (iskeep(i)) then 
                   call acsols(AcNum)%copyAcs(TempAcsols(i))
                   AcNum =  AcNum + 1
               end if
            end do 
            call acsols(AcNum)%set(this)
            LastIndex = AcNum

            if (AcNum.gt.dim) then 
                write(*,*) "after adding new to archive. the total number of archived solution is more than the dimension"
                write(*,*) "file: solclass.f90"
                write(*,*) "if this happens, to remove from archive"
                pause
            end if
        endif
    end function 


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
    subroutine clearArchive(this)
        implicit none 
        class(archivedclass):: this
        this%id = -1
        this%BoxNum = -1
        this%xPos = -1
        this%yPos = -1
        this%obj = -1
        this%fleet = -1
    end subroutine

    subroutine set(this,sol)
        use constpara
        implicit none 
        CLASS(ArchivedClass)::this
        type(solclass)::sol
        integer::l
        this%obj = sol%obj
        do l = 1, nline
            this%fleet(l) = sol%mylines(l)%fleet
        end do 

    end subroutine 
    
    subroutine copyAcs(this, rhs)
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
