    include 'genetic.f90' 
    include 'msasolver.f90'
    module la
    !use GraphLib
    !use SolverLib
    use dpsolverlib
    use msasolverlib
    implicit none

    type(dpsolver)::dp
    type(msasolver)::msa

    open (unit=logfileno,file='c:\gitcodes\logitassign\results\log.txt',status='replace',action="write")
    open (unit=gapfileno,file='c:\gitcodes\logitassign\results\gap.txt',status='replace',action="write")
    
    write(gapfileno,*) "method,solc,ncperr"

    
    call cleanfiles
    call readpara
    ! remark : I think msa can not handle bcm because of the changing for path size
    ! if we need to do msa, we need to udpate and subnetwork iteratively
    !call msa%solver 
    call dp%solver
    !call dp%cal_fx(dp%x,dp%fx)
    call dp%outputx

    write(*,*) "bless no bug"

    end program
     
    ! clean and open files 
    subroutine cleanfiles
    implicit none
    
    open(1,file='c:\gitcodes\logitassign\results\fortran_checkmadf.txt')
    write(1,*) 'case,i,anode,dest,x,y,ndest,maxdif' 
    close(1)
    end subroutine


    end module la
