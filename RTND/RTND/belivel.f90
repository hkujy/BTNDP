    program bilevel
    use constpara
    use GraphLib
    use dpsolverlib
    use myclassmodule
    implicit none
    integer:: i,l
    integer allseed(20),seed_cycle,seed1(1)
    real*8::totalcost
    real*8,external::get_totalcost
    type(solclass):: test_sol
    integer::newfleet(nline)
    type(dpsolver)::dp
    
    seed1=1
    call random_seed(put=seed1(:))
    open (unit=logfileno,file='c:\GitCodes\BTNDP\RTND\RESULTS\log.txt',status='replace',action="write")
    ! step 1 read input data
    call cleanfiles
    call readpara
    call writepara
    call dp%nwk%readnwt
    call dp%solver
    
    !call ini_mylines(mylines)
    !! initialise  my new added links
    !call inilinks(mylinks)
    !do l=1, nline
        !call ini_line_links(mylines(l),mylinks)
    !enddo 
    
    !open (1, file='..\..\results\checklinevarcost.txt')
    !write(1,*) "lineno,cost,var"
    !do l = 1,nline
        !write(1,*) l, mylines(l)%exptime, mylines(l)%vartime
    !enddo    
    !close(1)
    
    !newfleet(1) = 2
    !newfleet(2) = 1
    !newfleet(3) = 2
    !newfleet(4) = 1
    
    !!call set_sol_fre(test_sol, newfleet)
    !!call evaluatesol(test_sol,firstout,lastout,xfa)
    
    

    end program

    !
    !
    !subroutine set_test_fleet
    !use define
    !implicit none 
    !
    !integer l
    !do l=1, nline
    !    mylines(l)%fleet = 5
    !enddo    
    !end subroutine 
    !
    !subroutine set_myline_fleet(sol_fleet)
    !use define
    !implicit none 
    !integer,dimension(nline), intent(in)::sol_fleet 
    !integer l
    !do l=1, nline
    !    mylines(l)%fleet = sol_fleet(l)
    !enddo    
    !end subroutine 
    !
    !
    !subroutine test_one_fleet_sol(newfleet,firstout,lastout,xfa)
    !! input is vector of solution fleet
    !use define
    !use graph
    !implicit none
    !integer, intent(in):: firstout(nn), lastout(nn)
    !real*8, intent(inout)::xfa(nl,ndest)
    !
    !integer,dimension(nline), intent(in)::newfleet 
    !integer l
    !
    !call set_myline_fleet(newfleet)
    !call update_linefre(newfleet)
    !call update_secfre
    !call lowerlevel(xfa,firstout, lastout)
    !call outputod(xfa, fx)
    !!totalcost =  get_totalcost(xfa,fx)
    !
    !
    !end subroutine

    subroutine cleanfiles
    implicit none
    
    open(1,file='c:\gitcodes\BTNDP\results\fortran_checkmadf.txt')
    write(1,*) 'case,i,anode,dest,x,y,ndest,maxdif' 
    close(1)
    end subroutine