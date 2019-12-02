   
    module mylineclass ! the module name defines the namespace
    use constpara
    type,public::lineclass ! classname is the class prototype name
       integer::id
       real*8::fre
       integer::fleet
       real*8::exptime
       real*8::vartime
       integer::stops(10)  ! maximum number of stops 
       integer::numstops
       real*8::tt(10)    ! lines times 
       real*8::var(10)
       real*8::fare(10)
       !integer, dimension(:),allocatable::linksid    !
    contains
    procedure, pass::get_line_time=>get_line_time
    procedure, pass::get_line_fre =>get_line_fre
    procedure, pass::get_fleet => get_fleet
    procedure, pass::get_stop_costs=>get_stop_costs
    procedure,pass::copy=>copy
   end type lineclass

   contains


    subroutine read_lines(mylines)
    use constpara
    implicit none 
    integer::i,l,lid,ls
    real*8::val(5),f
    INTEGER::ss(4)
    type(lineclass),dimension(nline)::mylines
    
    select case(networktype) 
    case(0) 
        open(1,file='c:\gitcodes\BTNDP\input\testnetwork\Stops.txt')
        open(2,file='c:\gitcodes\BTNDP\input\testnetwork\LineSegData.txt')
        open(3,file='c:\gitcodes\BTNDP\input\testnetwork\IniFre.txt')
        num_line_seg_file_rows = 6
    case(1)
        open(1,file='c:\gitcodes\OpenTransportData\SiouxFallNet\Transit_Toy\Stops.txt')
        open(2,file='c:\gitcodes\OpenTransportData\SiouxFallNet\Transit_Toy\LineSegData.txt')
        open(3,file='c:\gitcodes\OpenTransportData\SiouxFallNet\Transit_Toy\IniFre.txt')
        num_line_seg_file_rows = 110

    case(2)
        open(1,file='c:\gitcodes\OpenTransportData\SiouxFallNet\Transit_AllOD\Stops.txt')
        open(2,file='c:\gitcodes\OpenTransportData\SiouxFallNet\Transit_AllOD\LineSegData.txt')
        open(3,file='c:\gitcodes\OpenTransportData\SiouxFallNet\Transit_AllOD\IniFre.txt')
        num_line_seg_file_rows = 110
    end select
    
    ! if (networktype.eq.0) then 
    !     open(1,file='c:\gitcodes\LogitAssign\input\testnetwork\Stops.txt')
    ! end if 
    ! if (networktype.eq.1) then 
    !     open(1,file='c:\gitcodes\OpenTransportData\SiouxFallNet\Stops.txt')
    ! endif

    do l = 1, nline
       read(1,*) ss
       lid = ss(1)
       do i = 2, 4
            if (ss(i).gt.0) then 
                mylines(lid)%stops(i-1) =ss(i)
            endif 
        enddo 
    enddo 
    do l =1, nline
        mylines(l)%numstops = 0
        do i = 1, 10
            if (mylines(l)%stops(i).gt.0) then 
                mylines(l)%numstops =  mylines(l)%numstops + 1
            end if 
        end do 
    enddo



    close(1)
!*****************************************!
    ! if (networktype.eq.0) then 
    !     open(1,file='c:\gitcodes\LogitAssign\input\testnetwork\LineSegData.txt')
    !     num_line_seg_file_rows = 6
    ! end if 
    ! if (networktype.eq.1) then
    !     open(1,file='c:\gitcodes\OpenTransportData\SiouxFallNet\LineSegData.txt')
    !     num_line_seg_file_rows = 110
    ! end if 

   do l = 1, num_line_seg_file_rows
        read(2,*) val 
        lid = IDINT(val(1))
        ls = IDINT(val(2))
        mylines(lid)%tt(ls) = val(3)
        mylines(lid)%var(ls) = val(4)
        mylines(lid)%fare(ls) = val(5)
    enddo
    close(2)

    ! if (networktype.eq.0) then  
    !     open(2,file='C:\GitCodes\LogitAssign\Input\TestNetwork\IniFre.txt')
    ! end if 
    ! if (networktype.eq.1) then 
    !     open(2,file='C:\GitCodes\OpenTransportData\SiouxFallNet\IniFre.txt')
    ! end if 
    do l = 1, nline
        read(3,*) f
        mylines(l)%fre = f/60.0
    end do
    close(3)

    end subroutine

    ! Give start and end point, return the cost, var, and fare
    subroutine get_stop_costs(this,start,ends,t,v,f)
    class(lineclass)::this 
    integer, intent(in)::start,ends
    real*8::t
    real*8,optional::v,f
    integer i, j
    logical::isfind
    isfind = .true.
    do i=1, size(this%stops)-1
        if (this%stops(i).eq.start) then 
            t=0
            if(present(v)) then 
                v=0
            end if
            if (present(f)) then
                f=0
            end if
            do j = i+1, size(this%stops)
                if (this%stops(j).eq.ends) then 
                    isfind = .true.
                    t = t + this%tt(j-1)
                    if(present(v)) then 
                        v = v + this%var(j-1)
                    endif
                    if (present(f)) then
                        f = f + this%fare(j-1)
                    end if
                    exit
                else 
                    t = t + this%tt(j-1)
                    if(present(v)) then 
                        v = v + this%var(j-1)
                    endif
                    if (present(f)) then
                        f = f + this%fare(j-1)
                    end if
                end if 
            end do 
        endif
    RETURN 
    end do

    if (.not.isfind) then 
        write(*,*)  " can not find the time location"
    end if

    end subroutine 

   subroutine ini_lines(mylines)
   implicit none
   type(lineclass), dimension(nline),intent(inout)::mylines
   integer::l
   do l = 1, nline
       mylines(l)%id = l
       mylines(l)%stops = 0
       mylines(l)%tt = -99
       mylines(l)%var = -99
   enddo 

   call read_lines(mylines)

   end subroutine


   subroutine get_line_time(this)
   use constpara
   implicit none
   class(lineclass)::this
   ! this purpose of this subroutine is to get the expected travel time of lines
   integer::i
   !linksid
   this%exptime = 0
   this%vartime = 0
    
   do i = 1, size(this%stops) -1
      if (this%stops(i+1).le.0) then 
         exit
      else 
         this%exptime = this%tt(i) + this%exptime
         this%vartime =  this%var(i) + this%vartime
      end if 
    enddo 
   return
   end subroutine

    subroutine get_line_fre(this)
    implicit none
    class(lineclass)::this
    call this%get_line_time
   ! computet the frequency
    this%fre = 60 * (this%fleet / this%exptime) *(1 + this%vartime/(this%exptime**2))
    end subroutine

    subroutine get_fleet(this,fre)
    implicit none
    class(lineclass)::this
    real*8, intent(in)::fre
    call this%get_line_time
    this%fleet = ceiling((this%exptime/60)*(fre)/(1+this%vartime/(this%exptime**2)))   
    end subroutine


    subroutine copy(this, rhs)
        implicit none 
        CLASS(lineclass):: this, rhs

        this%id = rhs%id
        this%fre = rhs%fre
        this%fleet = rhs%fleet
        this%exptime = rhs%exptime
        this%vartime = rhs%vartime
        this%stops = rhs%stops
        this%tt = rhs%tt
        this%var= rhs%var
        this%fare = rhs%fare

    end subroutine


    end module
