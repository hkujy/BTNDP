
! use brute force method to solve
    module BruteForce
    use constpara
    use mysolclass
    implicit none
        
    integer::totalfea
    integer, allocatable, dimension(:,:)::pool

    contains

    subroutine bfmain(basenwk)
        use GraphLib
        use mysolclass
    implicit none 
    integer::p
    type(solclass)::sol       
    class(graphclass),intent(in)::basenwk
    call get_pool
    caseindex = 1
    do p = 1, totalfea
        call sol%update_fleet_and_fre(pool(1,:))
        write(*,*) "CreateBaseSol in Brute Force"
        !call sol%evaluate(basenwk)
        call sol%dp%outputod(sol%dp%x,sol%dp%fx,nl,ndest)
        call sol%dp%outputx
    enddo 
    end subroutine

    ! get the solution pool
    subroutine get_pool
    implicit none 

    integer::l1,l2,l3,l4
    totalfea = 0
    do l1 = fleet_lb(1), fleet_ub(1)
        do l2 =  fleet_lb(2),fleet_ub(2)
            do l3 = fleet_lb(3), fleet_ub(3)
                do l4 =  fleet_lb(4),fleet_ub(4)
                    if (l1+l2+l3+l4.le.fleetsize) then 
                        totalfea =  totalfea + 1
                    end if 
                end do 
            enddo
        end do 
    end do 
    
    write(*,*) "total feasible pool = ",totalfea

    allocate(pool(totalfea,4))
    totalfea = 0
    do l1 = fleet_lb(1), fleet_ub(1)
        do l2 =  fleet_lb(2),fleet_ub(2)
            do l3 = fleet_lb(3), fleet_ub(3)
                do l4 =  fleet_lb(4),fleet_ub(4)
                    if (l1+l2+l3+l4.le.fleetsize) then 
                        totalfea = totalfea + 1
                        pool(totalfea,1) = l1
                        pool(totalfea,2) = l2
                        pool(totalfea,3) = l3
                        pool(totalfea,4) = l4
                    end if 
                end do 
            enddo
        end do 
    end do 
 



    end subroutine

    end module