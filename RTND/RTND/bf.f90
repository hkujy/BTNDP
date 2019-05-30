
! use brute force method to solve
    module BruteForce
    use constpara
    use mysolclass
    implicit none
        
    integer::totalfea
    integer, allocatable, dimension(:,:)::pool
    real*8, allocatable, dimension(:,:)::fre_pool

    contains

    subroutine bf_enumerate_fleet(basenwk)
    use GraphLib
    use mysolclass
    implicit none 
    integer::p,l
    real*8::inifre(nline)
    type(solclass)::sol       
    real*8,ALLOCATABLE::tmp_fre(:,:)
    class(graphclass),intent(in)::basenwk
    
    call get_pool
    ALLOCATE(tmp_fre(totalfea+1,4))
    caseindex = 0
    do l = 1, nline 
        call sol%mylines(l)%copy(basenwk%mylines(l))
    enddo
    !do p = totalfea, 1,-1
    
    open(1, file="C:\GitCodes\BTNDP\Input\TestNetwork\inifre.txt")
    do l = 1, nline
        read(1, *) inifre(l)
    enddo 
    close(1)
    
    ! write cases
    open(1, file = "C:\GitCodes\BTNDP\Input\TestNetwork\numcases.txt")

    write(1,*) totalfea+1
    ! evaluate base case 
    do l =1, nline
        sol%mylines(l)%fre = inifre(l)/60.0
        tmp_fre(1,l) =  inifre(l)
    end do  

    call sol%evaluate(basenwk)
    call sol%dp%outputod(sol%dp%xfa,sol%dp%fx)
    call sol%dp%outputx
    caseindex= caseindex + 1

    
    do p = 1, totalfea
        write(*,*) pool(p,:)
        call sol%set_fleet_and_fre(pool(p,:))
        do l =1, nline
            ! sol%mylines(l)%fre = real(pool(p,l)/60.0)
            tmp_fre(p+1,l) =  sol%mylines(l)%fre
            sol%mylines(l)%fre = sol%mylines(l)%fre/60.0
        end do  
        write(*,*) 60*sol%mylines(1)%fre,60*sol%mylines(2)%fre,60*sol%mylines(3)%fre,60*sol%mylines(4)%fre
        call sol%evaluate(basenwk)
        call sol%dp%outputod(sol%dp%xfa,sol%dp%fx)
        call sol%dp%outputx
        caseindex= caseindex + 1
    enddo 
    
    open(2, file = "C:\GitCodes\BTNDP\Input\TestNetwork\setfre.txt")
    open(3, file = "C:\GitCodes\BTNDP\Results\enumeratefleet.txt")    
   
    do p=1, totalfea+1
        write(2,"(f8.3,a,f8.3,a,f8.3,a,f8.3)") tmp_fre(p,1),",",tmp_fre(p,2),",",tmp_fre(p,3),",",tmp_fre(p,4)
    enddo
    do p = 1, totalfea
       WRITE(3,"(i4,a,i4,a,i4,a,i4)") pool(p,1),",",pool(p,2),",",pool(p,3),",",pool(p,4)
       write(*,*) pool(p,:)
    enddo 
    close(1)
    close(2)
    close(3)
 
    DEALLOCATE(tmp_fre) 
    end subroutine


    subroutine bf_given_fre(basenwk)
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
        write(*,"(4(f6.2,2X))") fre_pool(p,:)
        !call sol%set_fleet_and_fre(pool(p,:))
        do l =1, nline
            ! sol%mylines(l)%fre = real(pool(p,l)/60.0)
            sol%mylines(l)%fre = fre_pool(p,l)/60.0
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
    allocate(fre_pool(totalfea,4))
    open(1, file='C:\GitCodes\BTNDP\Input\TestNetwork\setfre.txt')

    do i = 1, totalfea 
        read(1,*) tff(:)
        fre_pool(i,:) =  tff(:)
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