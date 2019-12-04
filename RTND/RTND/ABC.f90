! Main code the ABC algorithm 
! Solution representation 
! each gen represents the number of fleet allocated to for a ABC

    module ABC
    use mysolclass
    use GraphLib
    implicit none 

    type,public::abcclass
    integer::npop   ! population size
    integer::onlooker
    integer::maxlimit
    integer::maxiter
    type(solclass)::BaseCaseSol
    type(graphclass)::basenwk
    real*8::best_fit
    integer::best_id
    integer::xnum,ynum
    integer::LastArchiveIndex
    real*8::minobj(2),maxobj(2)
    type(solclass),allocatable::chrom(:)
    integer,allocatable::limitcount(:)   ! count the number of limints
    integer,allocatable::best_fleet(:)
    real*8,allocatable::BaseODcost(:)
    real*8,allocatable::baselinkflow(:,:)
    type(archivedclass), allocatable::archivesols(:)
    integer::sizeofArchive

    contains 
    procedure,pass::abcmain=>abcmain
    procedure,pass::iniabc=>iniabc
    procedure,pass::delabc=>delabc
    procedure,pass::getBaseCaseOd=>getBaseCaseOd
    procedure,pass::employ_bee=>employ_bee
    procedure,pass::onlooker_bee=>onlooker_bee
    procedure,pass::scouts=>scouts
    procedure,pass::gen_sol=>gen_sol
    procedure,pass::getfitness=>gitfitness
    procedure,pass::update_archive=>update_archive
    procedure,pass::update_global_best=>update_global_best
    procedure,pass::printarchive=>printarchive
    end type abcclass
    contains

 

    subroutine iniabc(this,input_basenwk)
    implicit none 
    class(abcclass)::this
    type(graphclass),intent(in)::input_basenwk
    integer::val, i
    this%LastArchiveIndex = 0
    open(1,file='c:\gitcodes\BTNDP\input\testnetwork\abcpara.txt')
    do i = 1, 4
        read(1,*) val
        if (i==1) then 
            this%npop = val
        end if 
        if (i==2) then 
            this%onlooker = val
        end if 
        if (i==3) then 
            this%limitcount = val
        end if 
        if (i==4) then 
            this%maxiter =  val
        end if
    enddo 
    close(1)
    allocate(this%chrom(this%npop))
    allocate(this%limitcount(this%npop))
    this%limitcount = 0
    allocate(this%baselinkflow(nl,ndest))
    this%baselinkflow = -1
    allocate(this%BaseODcost(nod))
    this%BaseODcost = -1
    allocate(this%best_fleet(nline))
    this%best_fleet = -1
    write(*,*) "Num of Pop = ", this%npop
    write(*,*) "Num of Onlooker = ", this%onlooker
    write(*,*) "Num of Limit = ", this%limitcount
    write(*,*) "Max iter = ", this%maxiter
    call this%basenwk%inigraph
    call this%basenwk%copynwk(input_basenwk)
    ! set and initialize archive
    open(1, file="c:/GitCodes/BTNDP/Input/TestNetwork/ArchivePara.txt")
    do i = 1, 2
        if (i.eq.1) read(1,*) this%xnum
        if (i.eq.2) read(1,*) this%ynum
    end do 
    close(1)
    this%sizeofArchive = this%xnum*this%ynum
    allocate(this%archivesols(this%sizeofArchive)) 
    do i = 1, size(this%archivesols)
        call this%archivesols(i)%iniarchive
    end do 
    this%minobj = 1000000
    this%maxobj = 0
    end subroutine
    
    subroutine delabc(this)
    implicit none
    class(abcclass)::this
    deallocate(this%chrom)
    deallocate(this%limitcount)
    deallocate(this%baselinkflow)
    deallocate(this%BaseODcost)
    deallocate(this%best_fleet)
    deallocate(this%archivesols)
    end subroutine

    ! get base case OD cost
    ! this is for the computing fairness values
    subroutine getBaseCaseOd(this)
    implicit none 
    class(abcclass)::this
    integer l

    call this%BaseCaseSol%inisol(this%basenwk)
    call this%BaseCaseSol%dp%ini
    call this%BaseCaseSol%dp%solver(this%basenwk)
    call get_od_cost(this%BaseCaseSol%dp,this%BaseCaseSol%odcost)
   
    write(*,*) "Write initial fleet szie"
    do l = 1, nline 
      write(*,*) l, this%BaseCaseSol%mylines(l)%fleet 
    enddo

    write(*,*) "Write initial OD cost"
    do l = 1, nod
      write(*,*) l,this%BaseCaseSol%odcost(l)
    enddo 

    end subroutine

    subroutine abcmain(this,input_basenwk)
        use mysolclass
        implicit none
        class(abcclass)::this
        type(graphclass),intent(in)::input_basenwk
        integer:: iter
        call this%getBaseCaseOd
        call this%gen_sol
        iter = 1
        do while(iter.lt.this%maxiter) 
            write(*,*) "ABC iter = ",iter
            call this%employ_bee
            !Remark: fitness only call once, because it is only used in onlooker prob
            call this%getfitness      
            call this%onlooker_bee
            call this%scouts
            call this%update_archive
            call this%printarchive(iter)
            iter = iter + 1
        enddo 

    end subroutine


    ! generate initial solution between upper and lower bound
    subroutine gen_sol(this)
    implicit none
    class(abcclass)::this 
    integer::i
    logical::addArchiveSatus
    integer::residule

    do i=1,this%npop
        call this%chrom(i)%generate(this%basenwk)
        call this%chrom(i)%evaluate(this%basenwk,this%BaseCaseSol)
        addArchiveSatus = this%chrom(i)%add_to_Archive(this%archivesols,this%sizeofArchive,this%LastArchiveIndex)
        ! call this%update_global_best(i)
    end do 

    end subroutine

    subroutine employ_bee(this)
    implicit none 
    class(abcclass)::this
    integer::p
    logical::isIncreaLimit
    do p = 1, this%npop
       isIncreaLimit = this%chrom(p)%get_neigh(this%basenwk,this%BaseCaseSol,&
                       this%archivesols,this%sizeofArchive,this%LastArchiveIndex)
       if (isIncreaLimit) then 
           this%limitcount(p) = 0
       else
           this%limitcount(p) = this%limitcount(p) + 1
       end if
    enddo 
    end subroutine

    subroutine onlooker_bee(this)
        use mutelib
        implicit none 
        class(abcclass):: this
        real*8,allocatable::fits(:)
        integer::p,id
        integer,allocatable::selectedList(:) ! select list
        logical::isIncreaLimit

        allocate(fits(this%npop))
        allocate(selectedList(this%onlooker))
    
        do p = 1, this%npop
            fits(p) = this%chrom(p)%fitness
        enddo

        call roulette(fits,this%npop,selectedList,this%onlooker) 
        do p = 1, this%onlooker
            id = selectedList(p)
            write(*,*) "**********id = ",id
            isIncreaLimit = this%chrom(id)%get_neigh(this%basenwk,this%BaseCaseSol,this%archivesols,this%sizeofArchive,this%LastArchiveIndex)
            if (isIncreaLimit) then 
                this%limitcount(id) = this%limitcount(id) + 1
            else
                this%limitcount(id) = 0
            end if
        enddo 

        deallocate(fits)
        deallocate(selectedList)
    end subroutine


    subroutine scouts(this)
    implicit none
    class(abcclass)::this
    type(solclass)::basesol
    integer:: p,ts,l,residule

    do p = 1, this%npop 
        if (this%limitcount(p).gt.this%maxlimit) then 
            call this%chrom(p)%generate(this%basenwk)
            call this%chrom(p)%evaluate(this%basenwk,this%BaseCaseSol)
            this%limitcount(p) = 0
        end if
    enddo

    end subroutine

    ! function to return the box num location of the archive
    function get_box_num(xval,yval,xgridnum,ygridnum) result(boxnum)
        ! 沿着x 轴， 1,2,3,4,...一层一层的叠加 box
        implicit none
        real*8,intent(in)::xval,yval 
        integer,intent(in)::xgridnum,ygridnum
        integer::boxnum

        boxnum = (yval-1)*xgridnum + xval + 1

        if (boxnum.gt.xgridnum*ygridnum) then
            write(*,*) "the computation of the box beyond limit"
            write(*,*)  " computed box value = ", boxnum
            write(*,*) "xval = ",xval,"yval = ",yval
            write(*,*) "xgridnum = ",xgridnum, "ygridnum = ",ygridnum
            pause
        end if
    end function
   
    subroutine update_archive(this)
        implicit none
        class(abcclass):: this
        integer::i, j
        logical,dimension(this%lastarchiveindex+1)::iskeep
        integer::xpos(this%npop),ypos(this%npop)
        type(archivedclass),dimension(this%LastArchiveIndex+1)::TempAc
        integer::AcNum
        real*8::eps(2)      ! esp value of the two objectives
        real*8::distI, distJ     ! distance when compare the two objective values
        ! step 0: read archive parameters
   
       ! Step 1: define grid of the archive 
       ! step 1.1: find the max and min of the values
       do i = 1, this%npop
            this%minobj(1) = min(this%minobj(1), this%chrom(i)%obj(1))
            this%minobj(2) = min(this%minobj(2), this%chrom(i)%obj(2))
            this%maxobj(1) = max(this%maxobj(1), this%chrom(i)%obj(1))
            this%maxobj(2) = max(this%maxobj(2), this%chrom(i)%obj(2))
       end do
       ! step 1.2. compute the eps values
       eps(1) = (this%maxobj(1) - this%minobj(1))/this%xnum
       eps(2) = (this%maxobj(2) - this%minobj(2))/this%ynum
       ! step 1.3 compute the box coordinate for all the values
       do i = 1, this%npop
            xpos(i) = floor((this%chrom(i)%obj(1) - this%minobj(1))/eps(1))
            ypos(i) = ceiling((this%chrom(i)%obj(2) - this%minobj(2))/eps(2))
       enddo
       do i = 1, this%npop
            write(*,*) i,xpos(i),ypos(i)
       enddo 
       ! step 1.4. update existing archive pos
       do i = 1, this%LastArchiveIndex
            this%archivesols(i)%xpos = floor((this%archivesols(i)%obj(1)- this%minobj(1))/eps(1))
            this%archivesols(i)%ypos = floor((this%archivesols(i)%obj(2)- this%minobj(2))/eps(2))
            this%archivesols(i)%BoxNum = get_box_num(this%archivesols(i)%obj(1),this%archivesols(i)%obj(2),&
                                        this%xnum,this%ynum)
       enddo 
       ! step 3: update the solutions in each box
       iskeep = .true.  
       do i =1, this%LastArchiveIndex - 1
            do j = i + 1, this%LastArchiveIndex
                if (iskeep(i).and.iskeep(j)) then   
                    if (this%archivesols(i)%BoxNum.eq.this%archivesols(i)%BoxNum) then
                        distI = (this%archivesols(i)%obj(1) - this%minobj(1))**2 + &
                                (this%archivesols(i)%obj(2) - this%maxobj(2))**2
                        distJ = (this%archivesols(j)%obj(1) - this%minobj(1))**2 + &
                                (this%archivesols(j)%obj(2) - this%maxobj(2))**2
                        if (distI.gt.distJ) then 
                            iskeep(i) = .false.
                        else if (distI.lt.distJ) then
                            iskeep(j) = .false.
                        else if (distI.eq.distJ) then
                            write(*,*) " the two points in one box has equal distance"
                            write(*,*) " have not prepared for this"
                            write(*,*) " file = abc.f90"
                            pause
                        endif
                    end if 
                end if
            end do 
       enddo 

       do i = 1, this%LastArchiveIndex
             call TempAc(i)%copyAcs(this%archivesols(i))
             call this%archivesols(i)%clearArchive
       end do
       AcNum = 1
       do i = 1, this%LastArchiveIndex
            if (iskeep(i)) then 
                call this%archivesols(AcNum)%copyAcs(TempAc(i))
                AcNum = AcNum + 1
            end if
       enddo
       this%LastArchiveIndex = AcNum - 1
    end subroutine

    subroutine gitfitness(this)
      implicit none
      class(abcclass)::this
      integer::i,j, statval
      do i = 1, this%npop
          this%chrom(i)%NumBeat = 0
          this%chrom(i)%NumLoss = 0
          this%chrom(i)%fitness = 0
      end do
      do i = 1, this%npop-1
          do j = i+1, this%npop
              statval = this%chrom(i)%compare(this%chrom(j))
              if (statval.eq.1) then 
                  this%chrom(i)%NumBeat = this%chrom(i)%NumBeat + 1
                  this%chrom(j)%NumLoss = this%chrom(i)%NumLoss + 1
              end if
              if (statval.eq.2) then
                  this%chrom(i)%NumLoss = this%chrom(i)%NumLoss + 1
                  this%chrom(i)%NumBeat = this%chrom(i)%NumBeat + 1
              endif
          end do 
      end do
      do i = 1, this%npop
          this%chrom(i)%fitness = real(this%chrom(i)%NumBeat)/real(this%npop)
          write(*,*) "Sol,",i,"fit=",this%chrom(i)%fitness
      enddo
    end subroutine

    subroutine printarchive(this,iter)
        implicit none 
        class(abcclass)::this
        integer,INTENT(IN)::iter
        integer::i
        open(1,file="/results/fortran_archive.txt",position="append", action="write")

        do i = 1, this%LastArchiveIndex
            write(1,"(I4,a1,f8.2,a1,f8.2)") iter,",",this%archivesols(i)%obj(1),",",this%archivesols(i)%obj(2)
        enddo

        close(1)
    end subroutine

! the following could delete
    subroutine update_global_best(this,pid)
    implicit none 
    class(abcclass)::this
    integer, intent(in)::pid
    integer::l
    if (this%chrom(pid)%fitness.lt.this%best_fit) then 
        this%best_fit = this%chrom(pid)%fitness
        this%best_id = pid
        do l = 1, nline
            this%best_fleet(l)=this%chrom(pid)%mylines(l)%fleet
        enddo 
    end if
    end subroutine
    
end module


