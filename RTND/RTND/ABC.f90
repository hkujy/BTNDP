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

    call gen_sol

    end subroutine


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

    do i = 1,remain
5       call random_number(ran)
        l = int(ran*nline + 1)
        if (sol%fleet(l) + 1.gt.fleet_ub(l)) then 
            goto 5
        else 
            sol%fleet(l) = sol%fleet(l)
        end if
    end do 

    end subroutine

    subroutine gen_sol
    implicit none
    
    integer i 
    do i=1,npop
        call gen_sol_one(chrom(i))
    end do 
    end subroutine

! subroutine employ_bee()
! end subroutine 


! subroutine on_looker()

! end subroutine 



end module 


