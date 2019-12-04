    include 'genetic.f90' 
    ! include 'msasolver.f90'
    program la
    !use GraphLib
    !use SolverLib
    use dpsolverlib
    use msasolverlib
    use pathbcmlib
    ! use STOCHLoading
    use csramsolverlib
    implicit none

    type(dpsolver)::dp
    type(msasolver)::msa
    type(pbcmsolver)::pbcm
    type(csramsolver)::csram
    ! type(DialLoad)::dial

    integer::i,j
    real*8::l,b,ls,ss
    ls = 2.0
    ss = 1.0
    
    call read_test_para

    open (unit=logfileno,file='c:\gitcodes\logitassign\results\log.txt',status='replace',action="write")
    open (unit=gapfileno,file='c:\gitcodes\logitassign\results\gap.txt',status='replace',action="write")
    
    write(gapfileno,*) "method,solc,ncperr,disterr"

    call cleanfiles
    call readpara

    ! remark : I think msa can not handle bcm because of the changing for path size
    ! if we need to do msa, we need to udpate and subnetwork iteratively
    if (isHardCodePathBcm) then 
        call pbcm%ini
        call pbcm%solver 
        call pbcm%path_bcm_outputx 
        call pbcm%del
        goto 999
    endif 

    select case (solver_index)
    case (1)
        if (tune_solver.eq.1) then
            do i = 1,20
                l = 0.1D0*(real(i))
                do j = 1,10
                    b = 0.10D0*(real(j))
                    call dp%ini
                    call dp%tunesolver(lam=l,beta0=b)
                    call dp%del
                end do 
            enddo 
        else 
            call dp%ini
            call dp%readpare
            call dp%dpmain
            call dp%del
        end if
    case (2)
        call msa%ini
        call msa%solver 
        call msa%outputx
        call msa%del
    case (3)
        call csram%ini
        call csram%solver(ls,ss)
        call csram%backward_update_fx(csram%fx,csram%logitprob,d1=nl,d2=ndest)
        csram%ncperr = csram%getncperr(csram%x,csram%xfa,csram%fx)

        call csram%del
    case default
        write(*,*) "wtf problem you want to solve"
    end select
    !call dp%cal_fx(dp%x,dp%fx)


999  write(*,*) "bless no bug"

    end program
     
    ! clean and open files 
    subroutine cleanfiles
    use constpara
    implicit none
    
    open(1,file='c:\gitcodes\logitassign\results\fortran_checkmadf.txt')
    write(1,*) 'case,i,anode,dest,x,y,ndest,maxdif' 
    close(1)
    open(1,file="/results/fortran_archive.txt")
    write(1,*) 'Iter,ttc,fare'
    close(1)

    open(1,file='c:\gitcodes\LogitAssign\results\fortran_checkmadf.txt')
    write(1,"(a5,a2,a6,a5,a2,a2,a6,a6)") "case,","i,","anode,","dest,","x,","y,","ndest,","maxdif"
    close(1)

    open(1,file='c:\gitcodes\LogitAssign\results\fortran_output_od.txt')
    write(1,"(a4,a,a6,a,a4,a,a6,a,a,a,a4)") "case",",","origin",",","dest",",","demand",",","y",",","flow"
    close(1)
    
    open(1,file='c:\gitcodes\LogitAssign\results\fortran_output_link.txt')
    write(1,"(a7,a5,a5,a5,a5,a3,a3,a6,a10,a5,a4)") "method,","case,","dest,","link,","flow,","fx,","lt,","xprob,","logitprob,","tail,","head"
    close(1)
    
    open(1,file='c:\GitCodes\LogitAssign\results\fortran_output_node.txt')
    write(1,"(a7,a5,a5,a5,a5,a5,a5)") "method,","case,","dest,","node,","fout,","lout,","label"
    close(1)

    open(dp_tune_para_file_part1,file='c:\gitcodes\LogitAssign\results\fortran_dp_para1.txt')
    write(dp_tune_para_file_part1,"(a5,a5,a4,a)") "case,","lama,","miu,","v"
    close(dp_tune_para_file_part1)

    open(dp_tune_para_file_part2,file='c:\gitcodes\LogitAssign\results\fortran_dp_para2.txt')
    write(dp_tune_para_file_part2,"(a5,a5,a5,a8,a6,a6)") "case,","beta,","solc,","cputime,","error,","diserr"
    close(dp_tune_para_file_part2)
    !open(17,file='..\..\results\fortran_finalerr.txt',status='old',position='append')
    open(1,file='c:\gitcodes\LogitAssign\results\fortran_finalerr.txt')
    write(1,"(a5,a3)") "case,","err" 
    close(1)
    
    open(1,file='c:\gitcodes\LogitAssign\results\fortran_dp_converge.txt')
    write(1,"(a5,a5,a3)") "case,","solc,","err"
    close(1)
   
    
    open(dp_converge_file,file='c:\gitcodes\LogitAssign\results\converge_dp_para.txt')
    write(dp_converge_file,*) "lama,miu,v,inibeta,betastep,solc,cputime,ncperr"
    close (dp_converge_file)
    end subroutine


 
