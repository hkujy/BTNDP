   
    module mylineclass ! the module name defines the namespace
    use constpara
    type lineclass ! classname is the class prototype name
       integer::id
       real*8::fre
       integer::fleet
       real*8::exptime
       real*8::vartime
       integer::stops(10)  ! maximum number of stops 
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
    implicit none 
    integer::i,l,lid,ls
    real*8::val(5)
    INTEGER::ss(4)
    type(lineclass),dimension(nline)::mylines

    open(1,file='c:\gitcodes\BTNDP\input\testnetwork\Stops.txt')

    do l = 1, nline
       read(1,*) ss
       lid = ss(1)
       do i = 2, 4
            if (ss(i).gt.0) then 
                mylines(lid)%stops(i-1) =ss(i)
            endif 
        enddo 
    enddo 
    close(1)
!*****************************************!
    open(1,file='c:\gitcodes\BTNDP\input\testnetwork\LineSegData.txt')


   do l = 1, 6
        read(1,*) val 
        lid = IDINT(val(1))
        ls = IDINT(val(2))
        mylines(lid)%tt(ls) = val(3)
        mylines(lid)%var(ls) = val(4)
        mylines(lid)%fare(ls) = val(5)
    enddo
    CLOSE(1)
    end subroutine

    subroutine get_stop_costs(this,start,ends,t,v,f)
    class(lineclass)::this 
    integer, intent(in)::start,ends
    real*8::t,v,f
    integer i 
    logical::isfind
    isfind = .true.
    do i=1, size(this%stops)-1
        if ((this%stops(i).eq.start).and.(this%stops(i+1).eq.ends)) then 
            isfind = .true.
            t = this%tt(i)
            v = this%var(i)
            f = this%fare(i)
            RETURN 
        end if 
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
   integer::l,i
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