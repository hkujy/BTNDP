    !include "myclass.f90"
    program bilevel
    use constpara
    use GraphLib
    use dpsolverlib
    use BruteForce
    implicit none
    integer:: i,l
    integer allseed(20),seed_cycle,seed1(1)
    real*8::totalcost
    real*8,external::get_totalcost
    !type(solclass):: test_sol
    integer::newfleet(nline)
    type(dpsolver)::dp
    type(graphclass)::Basenwk
     
    
    fre_lb = 4
    fre_ub = 20
    fleetsize = 12
    ! call set_line_links
    seed1=1
    call random_seed(put=seed1(:))
    open (unit=logfileno,file='c:\GitCodes\BTNDP\RESULTS\log.txt',status='replace',action="write")
    ! step 1 read input data
    call cleanfiles
    call readpara
    call writepara

    call Basenwk%readnwt
    call get_fleet_range(Basenwk)
 
    write (*,*) "lower bound = ", fleet_lb
    write (*,*) "upper bound = ", fleet_ub
    !do i =1, 100
    !call dp(i)%nwk%readnwt
    !enddo
    
    call bfmain(Basenwk)
    write(*,*) "good luck"
    
    
    

    end program


    subroutine cleanfiles
    implicit none
    
    open(1,file='c:\gitcodes\BTNDP\results\fortran_checkmadf.txt')
    write(1,*) 'case,i,anode,dest,x,y,ndest,maxdif' 
    close(1)


    open(1,file='c:\gitcodes\BTNDP\results\fortran_outputod.txt')
    write(1,*) "case,origin,dest,demand,y,flow"
    close(1)
  
    
    open(1,file='c:\gitcodes\BTNDP\results\fortran_output_link.txt')
    write(1,*) "method,case,dest,link,flow,fx,lt,xprob,logitprob,tail,head"
    close(1)
    
    open(1,file='c:\GitCodes\LogitAssign\Results\fortran_output_node.txt')
    write(1,*) "method,case,dest,node,fout,lout,label"
    close(1)
 

    end subroutine


    subroutine get_fleet_range(nwk)
    use constpara
    use GraphLib
    implicit none 
    type(graphclass)::nwk
    integer l
    
    do l = 1, nline
        call nwk%mylines(l)%get_fleet(fre_lb(l))
        fleet_lb(l) = nwk%mylines(l)%fleet
        call nwk%mylines(l)%get_fleet(fre_ub(l))
        fleet_ub(l) = nwk%mylines(l)%fleet
    end do 


    end subroutine