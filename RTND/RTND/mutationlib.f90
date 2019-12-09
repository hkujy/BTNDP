!mutsation lib for the fleet 
    
    module mutelib
    use constpara
    contains 
   
    !TODO : write other type of mutation operators

    function checkfeet(fe) result(isOk)
        implicit none 
        integer,intent(in)::fe(nline)
        integer::i
        integer::subfe
        LOGICAL::isOk
        
        subfe = sum(fe)
        if(subfe.gt.fleetsize) then 
            isOk = .false.
        else
            isOk = .true.
        end if

    end function

    subroutine mutation_main(now,nei)
        implicit none
        integer,intent(in)::now(nline)
        integer,intent(out)::nei(nline)
        integer::index
        real*8::ran
        call random_number(ran)
        index = int(4*ran + 1)
        write(*,*) "mutation index = ", index 
        select case (index)
        case (1)
            call mute_increa_by1(now,nei)
        case (2)
            call mute_swap(now,nei)
        case (3)
            call mute_incre_decre(now,nei,2)
        case (4)
            call mute_incre_decre(now,nei,3)
        end select
        
        !nei(1) = 1 
        !nei(2) = 2 
        !nei(3) = 2 
        !nei(4) = 2 

        call remedy(nei)
        if (.not.checkfeet(nei)) then 
            write(*,*) "mutation err, fleetsize constraint is violated"
            write(*,*) "check file: mutationlib.f90"
            pause
        end if

    end subroutine

    subroutine select_two_dif_line(L1,L2)
        implicit none
        integer,INTENT(OUT)::L1,L2
        real*8::ran
        integer::count
        count = 0 
    10  call random_number(ran)
        L1 = int(nline*ran+1)

    15  call random_number(ran)
        L2 = int(nline*ran + 1)
        if (L1.eq.L2) then 
            count = count + 1
            if (count.ge.100) then 
                write(*,*) "can not find a line to increase"
            end if 
            goto 15
        end if 
    end subroutine

    subroutine mute_swap(now,nei)
        implicit none
        integer,intent(in)::now(nline)
        integer,intent(out)::nei(nline)
        integer::L1,L2
        !todo: swap the two lines 

        nei =  now
        call select_two_dif_line(L1,L2)
        nei(L1) = now(L2) 
        nei(L2) = now(L1)

    end subroutine

    subroutine mute_incre_decre(now,nei,num)
        implicit none 
        integer,intent(in)::now(nline)
        integer,intent(out)::nei(nline)
        integer,intent(in)::num
        integer::L1,L2
        nei =  now
        call select_two_dif_line(L1,L2)
        nei(L1) = max(nei(L1) - num,fleet_lb(L1))
        nei(L2) = min(nei(L2) + num,fleet_ub(L2))
    end subroutine

    subroutine mute_increa_by1(now,nei)
    ! increase one line and reduce other line
    implicit none 
    integer,intent(in)::now(nline)
    integer,intent(out)::nei(nline)
    integer::count
    real*8::ran
    integer::rl,il    !reduce and increase line

    nei =  now
    count = 0 
10  call random_number(ran)
    rl = int(nline*ran+1)
    if (nei(rl)-1.lt.fleet_lb(rl)) then 
        count = count + 1
        if (count.ge.100) then 
            write(*,*) " can not find a line to remove"
        end if 
        goto 10
    end if 
    count = 0
15  call random_number(ran)
    il = int(nline*ran + 1)
    if ((il.eq.rl).or.(nei(il)+1.gt.fleet_ub(il))) then 
        count = count + 1
        if (count.ge.100) then 
            write(*,*) "can not find a line to increase"
        end if 
        goto 15
    end if 

    nei(rl) = nei(rl) - 1
    nei(il) = nei(il) + 1

    end subroutine

   
    !call roulette(fits,this%npop,selectedList,this%onlooker) 
    subroutine roulette(fits,popsize,ListSelect,ListSelectSize) 
      implicit none 
      integer,intent(in)::popsize,ListSelectSize
      real*8,intent(in)::fits(popsize)
      integer::id,p,j
      real*8::ts,ran,cum_sum(popsize),prob(popsize)
      integer::ListSelect(ListSelectSize)
    
      ts = sum(fits)
      prob(:) = fits(:)/ts
      cum_sum(1) = prob(1)
      do p = 2, popsize
          cum_sum(p)= cum_sum(p-1) + prob(p)
      end do 

      ListSelect(:) = -1
      do j = 1, ListSelectSize
          id = -1
          call random_number(ran)
          if (ran.le.cum_sum(1)) then 
            id = 1
          else
            do p = 2, popsize
              if ((ran.gt.cum_sum(p-1)).and.(ran.le.cum_sum(p))) then 
                  id =  p
                  exit
              endif 
            enddo
          endif
          if (id.lt.0) then
              write(*,*)  "roulette err: cannot find valid id"
              write(*,*)  "file: mutationlib.f90"
              pause
          else
              ListSelect(j) = id
          end if
      enddo
    end subroutine
    

    function selectAline(ChangeWay,now) result(lindex)
        implicit none 
    !changeway: 1 reduce, 2 increase 
    integer::ChangeWay
    integer::lindex,l
    integer::now(4)
    real*8::ran
    integer::cc
    if(size(now).ne.4) then 
        write(*,*) "function: Select_aline dimension,file = mutationlib.f90"
    end if
10  call random_number(ran)
    lindex = int(nline*ran+1)
    select case (changeway)
    case (1)
      cc = 0
      do while ((cc.le.100).or.(now(lindex)-1.ge.fleet_lb(lindex)))
          call random_number(ran)
          lindex = int(nline*ran+1)
          cc = cc + 1
      end do
      if (cc.ge.100) then 
        lindex = -1
        do l = 1, nline
            if (now(l)-1.ge.fleet_lb(l)) then 
                lindex = l
                exit
            endif
        enddo
        if (lindex.eq.-1) then
            write(*,*) " can not find a line to reduce"
        endif
      end if
    case (2)
      cc = 0
      do while ((cc.le.100).or.(now(lindex)+1.lt.fleet_ub(lindex)))
          call random_number(ran)
          lindex = int(nline*ran+1)
          cc = cc + 1
      enddo
      if (cc.ge.100) then 
        lindex = -1
        do l = 1, nline
            if (now(l)+1.le.fleet_ub(l)) then 
                lindex = l
                exit
            end if
        enddo
        if (lindex.eq.-1) then 
            write(*,*) " can not find a line to increase"
            pause
        endif
      endif
    end select

    end function

    subroutine remedy(newlines)
    implicit none 
    integer,intent(inout)::newlines(nline)
    integer::l,add_sum, reduce_sum,gap,count
    logical::isRemedyBound
    real*8::ran
    integer::lindex
    isRemedyBound = .false.
    do l=1, nline
        if ((newlines(l).lt.fleet_lb(l)).or.(newlines(l).gt.fleet_ub(l))) then 
            isRemedyBound = .true. 
        endif
    enddo 
  
    if (isRemedyBound) then
        add_sum = 0
        reduce_sum = 0
        do l = 1, nline
            do while(newlines(l).lt.fleet_lb(l))
                newlines(l)= newlines(l) + 1
                add_sum = add_sum + 1 
            end do
            do while(newlines(l).gt.fleet_ub(l))
                newlines(l) = newlines(l) - 1
                reduce_sum = reduce_sum + 1
            end do
        end do 
        gap  = add_sum - reduce_sum
        if (gap.gt.0) then 
            ! more feet are added and need to reduce
            do while (gap.gt.0)
                count = 0
            10  call random_number(ran)
                l = int(nline*ran+1)
                if(newlines(l)-1.lt.fleet_lb(l)) then 
                    count = count + 1
                    if (count.ge.100) then 
                        write(*,*) " can not find a line to remove"
                    end if 
                    goto 10
                else
                    newlines(l) = newlines(l) - 1
                    gap = gap - 1 
                endif
            end do
        end if
        if (gap.lt.0) then 
            gap = abs(gap)
            !more lines are reduces, so need to add it back
            do while (gap.gt.0)
                count = 0
            15  call random_number(ran)
                l = int(nline*ran + 1)
                if ((newlines(l) + 1).gt.fleet_ub(l)) then 
                    count = count + 1
                    if (count.ge.100) then 
                        write(*,*) "can not find a line to increase"
                    end if 
                    goto 15
                else
                    newlines(l) = newlines(l) + 1
                    gap = gap - 1
                endif
            end do
        end if 
    endif

  
4    if (sum(newlines).gt.fleetsize) then 
        lindex = selectAline(1,newlines)
        newlines(lindex) = newlines(lindex) - 1
        goto 4
    endif
6  if (sum(newlines).lt.fleetsize) then
        lindex = selectAline(2,newlines)
        newlines(lindex)  = newlines(lindex) + 1
        goto 6
    endif   


    do l = l, nline
        if (newlines(l).gt.fleet_ub(l).or.newlines(l).lt.fleet_lb(l)) then 
            write(*,*) "remedy procedure fails"
            write(*,*) "check file mutationlib.f90"
            pause
        end if
    enddo




    end subroutine   
    


    end module
