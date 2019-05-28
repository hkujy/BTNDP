
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
    integer::p,l
    type(solclass)::sol       
    class(graphclass),intent(in)::basenwk
    call get_pool
    caseindex = 0
    do l = 1, nline 
        call sol%mylines(l)%copy(basenwk%mylines(l))
    enddo
    !do p = totalfea, 1,-1
    do p = 1, 5
        write(*,*) pool(p,:)
        call sol%set_fleet_and_fre(pool(p,:))
        call sol%evaluate(basenwk)
        call sol%dp%outputod(sol%dp%xfa,sol%dp%fx)
        call sol%dp%outputx
        caseindex= caseindex + 1
    enddo 
    end subroutine


    subroutine bfmain_given_fre(basenwk)
    use GraphLib
    use mysolclass
    implicit none 
    integer::p,l
    type(solclass)::sol       
    class(graphclass),intent(in)::basenwk
    call read_pool
    caseindex = 0
    do l = 1, nline 
        call sol%mylines(l)%copy(basenwk%mylines(l))
    enddo
    !do p = totalfea, 1,-1
    do p = 1, totalfea
        write(*,*) pool(p,:)
        !call sol%set_fleet_and_fre(pool(p,:))
        do l =1, nline
            sol%mylines(l)%fre = real(pool(p,l)/60.0)
        end do  
        call sol%evaluate(basenwk)
        call sol%dp%outputod(sol%dp%xfa,sol%dp%fx)
        call sol%dp%outputx
        caseindex= caseindex + 1
    enddo 
    end subroutine


    subroutine read_pool
    implicit none 

    real*8::tff(4)
    integer::i 
    open(1, file='C:\GitCodes\BTNDP\Input\TestNetwork\numcases.txt')
    read(1,*) totalfea
    close(1)
    allocate(pool(totalfea,4))
    open(1, file='C:\GitCodes\BTNDP\Input\TestNetwork\setfre.txt')

    do i = 1, totalfea 
        read(1,*) tff(:)
        pool(i,:) =  tff(:)
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