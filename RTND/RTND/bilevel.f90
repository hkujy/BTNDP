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
    call Basenwk%readnwt
    call get_fleet_range(Basenwk)
 
    write (*,*) "lower bound = ", fleet_lb
    write (*,*) "upper bound = ", fleet_ub
    !do i =1, 100
    !call dp(i)%nwk%readnwt
    !enddo
    
    call bfmain_given_fre(Basenwk)
    call Basenwk%printnwk
    !call bfmain(Basenwk)
    write(*,*) "good luck"
    
    
    

    end program


    subroutine cleanfiles
    implicit none
    
    open(1,file='c:\gitcodes\BTNDP\results\fortran_checkmadf.txt')
    write(1,"(a5,a2,a6,a5,a2,a2,a6,a6)") "case,","i,","anode,","dest,","x,","y,","ndest,","maxdif"
    close(1)


    open(1,file='c:\gitcodes\BTNDP\results\fortran_output_od.txt')
    write(1,"(a4,a,a6,a,a4,a,a6,a,a,a,a4)") "case",",","origin",",","dest",",","demand",",","y",",","flow"
    close(1)
  
    
    open(1,file='c:\gitcodes\BTNDP\results\fortran_output_link.txt')
    write(1,"(a7,a5,a5,a5,a5,a3,a3,a6,a10,a5,a4)") "method,","case,","dest,","link,","flow,","fx,","lt,","xprob,","logitprob,","tail,","head"
    close(1)
    
    open(1,file='c:\GitCodes\BTNDP\results\fortran_output_node.txt')
    write(1,"(a7,a5,a5,a5,a5,a5,a5)") "method,","case,","dest,","node,","fout,","lout,","label"
    close(1)
 

    open(1,file='c:\gitcodes\BTNDP\results\fortran_dp_para1.txt')
    write(1,"(a5,a5,a4,a)") "case,","lama,","miu,","v"
    close(1)

    open(1,file='c:\gitcodes\BTNDP\results\fortran_dp_para2.txt')
    write(1,"(a5,a5,a5,a8,a6,a6)") "case,","beta,","solc,","cputime,","error,","diserr"
    close(1)
    !open(17,file='..\..\results\fortran_finalerr.txt',status='old',position='append')
    open(1,file='c:\gitcodes\BTNDP\results\fortran_finalerr.txt')
    write(1,"(a5,a3)") "case,","err" 
    close(1)
    
    open(1,file='c:\gitcodes\BTNDP\results\fortran_dp_converge.txt')
    write(1,"(a5,a5,a3)") "case,","solc,","err"
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