    include "myclass.f90"
    program bilevel

    use constpara
    use GraphLib
    use dpsolverlib
    use myclass
    use BruteForce
    implicit none
    integer:: i,l
    integer allseed(20),seed_cycle,seed1(1)
    real*8::totalcost
    real*8,external::get_totalcost
    !type(solclass):: test_sol
    integer::newfleet(nline)
    type(dpsolver)::dp(1)
   
    
    fre_lb = 4
    fre_ub = 20
    fleetsize = 12
    call set_line_links
    seed1=1
    call random_seed(put=seed1(:))
    open (unit=logfileno,file='c:\GitCodes\BTNDP\RESULTS\log.txt',status='replace',action="write")
    ! step 1 read input data
    call cleanfiles
    call readpara
    call writepara
    call get_fleet_range
 
    write (*,*) "lower bound = ", fleet_lb
    write (*,*) "upper bound = ", fleet_ub
    !do i =1, 100
    !call dp(i)%nwk%readnwt
    !enddo
    
    call get_pool
    write(*,*) "good luck"
    
    
    

    end program


    subroutine cleanfiles
    implicit none
    
    open(1,file='c:\gitcodes\BTNDP\results\fortran_checkmadf.txt')
    write(1,*) 'case,i,anode,dest,x,y,ndest,maxdif' 
    close(1)
    end subroutine


    subroutine get_fleet_range
    use myclass
    use constpara
    implicit none 
    integer l
    type(solclass)::sol
    
    call sol%dp%nwk%readnwt
    call ini_lines(sol%mylines)
    do l = 1, nline
        call sol%mylines(l)%get_fleet(sol%dp%nwk,fre_lb(l))
        fleet_lb(l) = sol%mylines(l)%fleet
        call sol%mylines(l)%get_fleet(sol%dp%nwk,fre_ub(l))
        fleet_ub(l) = sol%mylines(l)%fleet
    end do 


    end subroutine