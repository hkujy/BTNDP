    !TODO: the function read and write default parameters for the program
    subroutine readpara
    use constpara
    implicit none
    integer i
    real*8::value

    open(1,file='c:\gitcodes\logitassign\input\testnetwork\para.txt')
    open(2,file='c:\gitcodes\logitassign\input\testnetwork\bsvalue.txt')
    open(3,file='c:\gitcodes\logitassign\results\fortran_bsvalue.txt' )
    do i = 1,4
        read(1,*) value 
        if (i==1) then
            congestion_n = IDINT(value)
        endif
        if (i==2) then
            write(logfileno,*) "read_pare: use the same bs value for all"
            !bs = value
        endif
        if (i==3) then
            capk = value
        endif
        !if (i==4) then
        !    rio = value
        !endif
    enddo

   ! read beta value file 
    do i=1, nl
        read (2,*) bs(i)
    enddo
   
    write(3,*) "linkid,bsvalue"

    do i=1,nl
        write(3,*) i,",",bs(i)
    enddo

    close(1)
    close(2)
    close(3)
    call writepara
    end subroutine

    subroutine writepara
    use constpara   
    implicit none
    !open(1,file='..\..\results\fortran_para.txt',status='old', position='append' )
    open(1,file='c:\gitcodes\logitassign\results\fortran_para.txt' )
    
    write(1,'(i3, a, f6.2)') caseindex, ', congestion_para_n', congestion_n
    !write(1,'(i3, a, f6.2)') caseindex, ',', bs 
    write(1,'(i3, a, f6.2)') caseindex, ', capacity_k=', capk
    write(1,'(i3, a, f6.2)') caseindex, ', risk_rio= ', rio
    close(1)
    
    end subroutine 
