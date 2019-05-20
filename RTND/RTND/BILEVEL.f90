    INCLUDE 'lowermain.F90'

    PROGRAM BILEVEL
    USE la
    use MyClassModule
    IMPLICIT NONE

    INTEGER:: FIRSTOUT(NN), LASTOUT(NN)
    INTEGER:: I,l
	integer allseed(20),seed_cycle,seed1(1)
    REAL*8::XFA(NL,NDEST)
    REAL*8::TotalCost
    real*8,external::Get_TotalCost
    type(SolClass):: test_sol
    integer::NewFleet(NLINE)
    !TODO: CHANGE THE INPUT OF THE OUTER RALTED THE THE FLEET SIZE
    
    !TODOD: set read seed from file and set the seed cycle
    seed1=1
    call random_seed(put=seed1(:))

    OPEN (UNIT=LOGFILENO,FILE='..\..\RESULTS\LOG.TXT',STATUS='REPLACE',ACTION="WRITE")

    ! STEP 1 READ INPUT DATA
    CALL READPARA
    CALL WRITEPARA
    CALL READNETWORK(FIRSTOUT, LASTOUT)
    call Ini_MyLines(mylines)
    ! Initialise  my new added links
    call IniLinks(mylinks)
    do L=1, NLINE
        call ini_line_links(mylines(l),mylinks)
    enddo 
    
    open (1, file='..\..\RESULTS\CheckLineVarCost.TXT')
    write(1,*) "LineNo,Cost,Var"
    do l = 1,NLine
        write(1,*) l, mylines(l)%ExpTime, Mylines(l)%VarTime
    enddo    
    close(1)
    
    NewFleet(1) = 2
    NewFleet(2) = 1
    NewFleet(3) = 2
    NewFleet(4) = 1
    
    call set_sol_fre(test_sol, NewFleet)
    call EvaluateSol(test_sol,FIRSTOUT,LASTOUT,XFA)
    
    

    END PROGRAM


    
    subroutine set_Test_fleet
    use DEFINE
    implicit none 
    
    integer l
    do l=1, nline
        mylines(l)%Fleet = 5
    enddo    
    end subroutine 
   
    subroutine set_myline_fleet(sol_fleet)
    use DEFINE
    implicit none 
    integer,dimension(NLINE), intent(in)::sol_fleet 
    integer l
    do l=1, nline
        mylines(l)%Fleet = sol_fleet(l)
    enddo    
    end subroutine 
    
    
    subroutine Test_One_Fleet_Sol(NewFleet,FIRSTOUT,LASTOUT,XFA)
    ! input is vector of solution fleet
    use DEFINE
    use GRAPH
    implicit none
    INTEGER, INTENT(IN):: FIRSTOUT(NN), LASTOUT(NN)
    REAL*8, INTENT(INOUT)::XFA(NL,NDEST)
    
    integer,dimension(NLINE), intent(in)::NewFleet 
    integer L
    
    call set_myline_fleet(NewFleet)
    call Update_LineFre(NewFleet)
    call Update_SecFre
    CALL LOWERLEVEL(XFA,FIRSTOUT, LASTOUT)
    CALL OUTPUTOD(XFA, FX)
    !TotalCost =  Get_TotalCost(XFA,FX)
    
    
    end subroutine
