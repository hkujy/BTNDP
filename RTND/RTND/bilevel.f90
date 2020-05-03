!   code for the bi-level transit network design 
!   lower level problem is based on the approach based transit assignment 
!   upper level is ABC multi objective 
    program bilevel
    ! use constpara
    ! use GraphLib
    use dpsolverlib
    use BruteForce
    implicit none
    interface
      subroutine get_fleet_range(nwk,fre)
      use GraphLib
       type(graphclass)::nwk
       real*8,optional::fre(nline)
      end subroutine get_fleet_range   
    end interface
    integer:: i,l
    type(graphclass)::Basenwk
    integer, allocatable :: seed(:)
    integer::ns
    real*8,ALLOCATABLE::checkfre(:)
    integer::exp_id  ! id for the experiments
    open(unit=logfileno,file='c:\gitcodes\btndp\results\log.txt',status='replace',action="write")
   
    call read_test_para
    call cleanfiles
    call readpara
    call Basenwk%inigraph
    call Basenwk%readnwt
    call read_fleet_para

    allocate(checkfre(nline))
    do l = 1, nline
        checkfre(l) = Basenwk%mylines(l)%fre
    end do
    call get_fleet_range(Basenwk,checkfre)
    call get_fleet_range(Basenwk)
    write (*,*) "Lowerbound = ", fleet_lb
    write (*,*) "Upperbound = ", fleet_ub
    write(*,*) "minium fleetsize  = ", sum(fleet_lb)
    write(*,*) "maximum fleetsize  = ", sum(fleet_ub)
    call Basenwk%printnwk
   
    select case(networktype) 
    case(0) 
         open(1,file='c:\gitcodes\BTNDP\input\testnetwork\testindex.txt')
         read(1,*) exp_id
         close (1)
    case(1)
         open(1,file='c:\gitcodes\OpenTransportData\SiouxFallNet\Transit_Toy\testindex.txt')
         read(1,*) exp_id
         close (1)
    case(2)
         open(1,file='c:\gitcodes\OpenTransportData\SiouxFallNet\Transit_AllOD\testindex.txt')
         read(1,*) exp_id
         close (1)
    end select

    if (exp_id == 1) then 
        write(*,*) "*******Experiment: Given frequency"
        call test_given_fre(Basenwk)
    end if

    if (exp_id == 2) then 
        write(*,*) "********Experiment: Enumerate fleet"
        call test_enumerate_fleet(Basenwk)
    end if

    if (exp_id == 3) then 
        write(*,*) "********Experiment: ABC bilevel"
        call test_abc(Basenwk)
    end if
    write(*,*) "Good Luck"
    end program
    
    subroutine read_fleet_para
    use GraphLib
    implicit none 
    integer::val
    integer::row 
    select case (networktype)
    case(0)
        open(1,file='c:\gitcodes\BTNDP\input\testnetwork\testfleetpara.txt')
    case(1)
        open(1,file='c:\GitCodes\OpenTransportData\SiouxFallNet\Transit_Toy\testfleetpara.txt')
    case(2)
        open(1,file='c:\GitCodes\OpenTransportData\SiouxFallNet\Transit_AllOD\testfleetpara.txt')
    end select
    do row =1, 3
       read(1,*) val
       if (row == 1) then 
        fre_lb = real(val)
       endif 
       if (row == 2) then 
        fre_ub = real(val)
       end if 
       if (row == 3) then 
        fleetsize = val
       end if
    enddo 
    close(1)

    write(*,*) "Fre Lower Bound = ",fre_lb
    write(*,*) "Fre Upper Bound = ",fre_ub
    write(*,*) "Fleet Size  = ", fleetsize

    end subroutine 
    
    subroutine test_enumerate_fleet(Basenwk)
    use BruteForce
    implicit none 
     interface
      subroutine get_fleet_range(nwk,fre)
      use GraphLib
       type(graphclass)::nwk
       real*8,optional::fre(nline)
      end subroutine get_fleet_range   
    end interface
    type(graphclass)::basenwk
    integer::val
    integer::row

    call read_fleet_para
    call get_fleet_range(Basenwk)
    ! write (*,*) "lower bound = ", fleet_lb
    ! write (*,*) "upper bound = ", fleet_ub
    ! write(*,*) "Fleet Size  = ", fleetsize
    call bf_enumerate_fleet(Basenwk)
    end subroutine
    
    subroutine test_abc(basenwk)
    use ABC 
    implicit none
    type(graphclass)::basenwk
    type(abcclass):: bilevel_abc
    integer::ns,i,s
    real*8::start, finish,ct
    integer,allocatable::seed(:)
    ! todo:: read seed
    call random_seed(size = ns)
    allocate(seed(ns))
    call random_seed(get=seed)
    seed = 101 * (/ (i, i = 1, ns) /)
    call random_seed(put = seed)
   
    call bilevel_abc%iniabc(basenwk)
    do s =1, bilevel_abc%TotalNumSeed
        call random_seed(put = bilevel_abc%SeedVal(s,:))
        call random_seed(get=seed)
        bilevel_abc%CurrentSeedNum = s
        if (isWriteDug) then 
            write (*, *) "-----Test seed = ",seed
        endif
        call cpu_time(start)
        call bilevel_abc%abcmain(basenwk)
        call cpu_time(finish)
        bilevel_abc%CpuTime(s)=finish-start
    enddo
    open(1,file="c:\GitCodes\BTNDP\Results\cpu_abc.txt",position="append",action="write")
        do s =1, bilevel_abc%TotalNumSeed
            write(1,"(I4,a1,f14.2)") s,",",bilevel_abc%CpuTime(s)
            write(*,"(I4,a1,f14.2)") s,",",bilevel_abc%CpuTime(s)
        enddo
    close(1)
    call bilevel_abc%delabc 

    end subroutine

    subroutine cleanfiles
    implicit none
    open(1,file='c:\gitcodes\BTNDP\results\fortran_checkmadf.txt')
    write(1,"(a5,a2,a6,a5,a2,a2,a6,a6)") "case,","i,","anode,","dest,","x,","y,","ndest,","maxdif"
    close(1)
    open(1,file="c:/GitCodes/BTNDP/Results/Fortran_archive.txt")
    write(1,*) "SeedNum,Iter,SolId,LineId,Fleet,TTC,Fare"
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

    open(1,file="c:\GitCodes\BTNDP\Results\cpu_abc.txt")
    write(1,*) "SeedNum,CpuTime"
    close(1)
    

    end subroutine

    
    subroutine get_fleet_range(nwk,fre)
    use constpara
    use GraphLib
    implicit none 
    type(graphclass)::nwk
    real*8,optional::fre(nline)
    integer l
    real*8::sumval
    
    do l = 1, nline
        call nwk%mylines(l)%get_fleet(fre_lb(l))
        fleet_lb(l) = nwk%mylines(l)%fleet
        call nwk%mylines(l)%get_fleet(fre_ub(l))
        fleet_ub(l) = nwk%mylines(l)%fleet
    end do 

    if(PRESENT(fre)) then 
        sumval = 0
        do l = 1, nline
            call nwk%mylines(l)%get_fleet(fre(l))
            write(*,*) "Check: l=",l,",","fre=",nwk%mylines(l)%fre,",", "fleet=",nwk%mylines(l)%fleet
            sumval =  sumval + nwk%mylines(l)%fleet
        enddo
        write(*,*) "Check: Fre Total Fleet Szie = ", sumval
        !pause
    end if

    
    end subroutine


    subroutine test_given_fre(Basenwk)
    ! wirte as a sepeerate function 
    ! just in case i want to add something 
    use BruteForce
    use GraphLib
    implicit none
    type(graphclass)::basenwk 
    call bf_given_fre(Basenwk)
    
    end subroutine

