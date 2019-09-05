    
    program bilevel
    use constpara
    use GraphLib
    use dpsolverlib
    use BruteForce
    implicit none
    integer:: i,l
    integer allseed(20),seed_cycle,seed1(1)
    real*8::totalcost
    !real*8,external::get_totalcost
    !type(solclass):: test_sol
    ! integer,allocatable::newfleet(:)
    ! type(dpsolver)::dp
    type(graphclass)::Basenwk
    integer::exp_id 
    
    open(unit=logfileno,file='c:\GitCodes\BTNDP\RESULTS\log.txt',status='replace',action="write")
    open(1,file='c:\gitcodes\BTNDP\input\testnetwork\testindex.txt')
    read(1,*) exp_id
    close(1)
    
    call read_test_para
    
    seed1 = 1
    !call random_seed(put=seed1(:))
    ! step 1 read input data
    call cleanfiles
    call readpara
    call Basenwk%inigraph
    call Basenwk%readnwt
    call read_fleet_para

    call get_fleet_range(Basenwk)
    write (*,*) "lower bound = ", fleet_lb
    write (*,*) "upper bound = ", fleet_ub
    call Basenwk%printnwk

    if (exp_id==1) then 
        write(*,*) "Experiment: Given frequency"
        call test_given_fre(Basenwk)
    end if

    if (exp_id==2) then 
        write(*,*) "Experiment: Enumerate fleet"
        call test_enumerate_fleet(Basenwk)
    end if

    if (exp_id == 3) then 
        write(*,*) "Experiment: ABC bilevel"
        call test_abc(Basenwk)
    end if
    ! call bfmain_given_fre(Basenwk)
    !call bfmain(Basenwk)
    write(*,*) "good luck"
    
    end program


    subroutine test_given_fre(Basenwk)
    use BruteForce
    use GraphLib
    implicit none
    type(graphclass)::basenwk 
    ! call bf_given_fre(Basenwk)
    
    end subroutine
    
    subroutine read_fleet_para
    use GraphLib
    implicit none 
    integer::val
    integer::row 
    open(1,file='c:\gitcodes\BTNDP\input\testnetwork\testfleetpara.txt')
    do row =1, 3
       read(1,*) val
       if (row==1) then 
        fre_lb = real(val)
       endif 
       if (row==2) then 
        fre_ub = real(val)
       end if 
       if (row==3) then 
        fleetsize = val
       end if
    enddo 
    close(1)

    end subroutine 
    
    subroutine test_enumerate_fleet(Basenwk)
    use BruteForce
    use GraphLib
    implicit none 
    type(graphclass)::basenwk
    integer::val
    integer::row
    open(1,file='c:\gitcodes\BTNDP\input\testnetwork\testfleetpara.txt')
    do row =1, 3
       read(1,*) val
       if (row==1) then 
        fre_lb = real(val)
       endif 
       if (row==2) then 
        fre_ub = real(val)
       end if 
       if (row==3) then 
        fleetsize = val
       end if
    enddo 
    close(1)

    call get_fleet_range(Basenwk)
    write (*,*) "lower bound = ", fleet_lb
    write (*,*) "upper bound = ", fleet_ub
    ! call bf_enumerate_fleet(Basenwk)
    end subroutine

    
    
    subroutine test_abc(basenwk)
    use GraphLib
    use ABC 
    implicit none
    type(graphclass)::basenwk
    type(abcclass):: bilevel_abc
    call bilevel_abc%abcmain(basenwk)
    end subroutine


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
        call nwk%mylines(l)%get_fleet(fre_lb(l),islb=.true.)
        fleet_lb(l) = nwk%mylines(l)%fleet
        call nwk%mylines(l)%get_fleet(fre_ub(l),isub=.true.)
        fleet_ub(l) = nwk%mylines(l)%fleet
    end do 


    end subroutine