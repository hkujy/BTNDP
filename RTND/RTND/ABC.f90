! Main code the ABC algorithm 
! Solution representation 
! each gen represents the number of fleet allocated to for a ABC
    module ABC
    use myclass
    use constpara
    implicit none 
    integer,parameter::npop = 10   ! population size
    real*8::ran   !random number
    type(solclass)::chrom(npop)
    contains 

    subroutine abcmain
    implicit none

    ! call gen_sol

    end subroutine

    ! generate initla solution between upper and lower bound
    subroutine gen_sol_one(sol)
    implicit none 
    type(solclass), intent(inout)::sol
    integer::remain,l,i
    sol%fleet = fleet_lb
    remain = int(fleetsize - sum(sol%fleet))

    if (remain.le.0) then 
        write(*,*) "The lower bound is greater than the total fleet"
        pause
    endif 

    if (remain.ge.(sum(fleet_ub)-sum(sol%fleet))) then 
        write(*,*) "Total fleet size is too large to be all allocated"
        write(*,*) "check file, abc.f90"
        pause
    end if

    call assign_remain(sol%fleet)

    end subroutine

    subroutine gen_sol
    implicit none
    
    integer i 
    do i=1,npop
        call gen_sol_one(chrom(i))
    end do 
    end subroutine

    subroutine remedy(now)
    implicit none 
    integer,intent(inout)::now(nline)
    integer::l
    integer::add_sum, reduce_sum
    logical::isRemedy
    isRemedy = .false.
    
    do l=1, nline
        if ((now(l).lt.fleet_lb(l)).or.(now(l).gt.fleet_ub(l))) then 
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
        do while(now(l).lt.fleet_lb(l))
            now(l) = now(l) + 1
            add_sum = add_sum + 1 
        end do
        do while(now(l).gt.fleet_ub(l))
            now(l) = now(l) - 1
            reduce_sum = reduce_sum + 1
        end do
    end do 
    end subroutine

    subroutine assign_remain(now)
    implicit none
    integer,intent(inout)::now(nline)
    integer::l, i
    integer::remain
    
    remain = fleetsize - sum(now)
    if (remain.eq.0) then 
        return 
    end if
    do i = 1, remain
5       call random_number(ran)
        l = int(ran*nline + 1)
        if (now(l) + 1.gt.fleet_ub(l)) then 
            goto 5
        else 
            now(l) = now(l) + 1
        end if
    end do 
    end subroutine






    ! generate a neighbour by randomly increase the fleet size 
    subroutine mute_increa(now,nei)
    implicit none 
    integer,intent(in)::now(nline)
    integer,intent(out)::nei(nline)
    integer::count
    integer::rl,il    !reduce and increase line
    
    ! if ((now.eq.fleet_lb).OR.(now.eq.fleet_ub)) then 
        ! write(*,*) "no space to remove or add"
    ! end if 

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



! subroutine employ_bee()
! end subroutine 


! subroutine on_looker()

! end subroutine 



end module 


