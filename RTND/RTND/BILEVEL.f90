    !INCLUDE 'LOWERLEVEL.F90'

    PROGRAM BILEVEL
    USE GRAPH
    use MyClassModule
    IMPLICIT NONE

    INTEGER:: FIRSTOUT(NN), LASTOUT(NN)
    INTEGER:: I,l
    REAL*8::XFA(NL,NDEST)
    REAL*8::TotalCost
    real*8,external::Get_TotalCost
    type(SolClass):: test_sol
    integer::NewFleet(NLINE)
    !TODO: CHANGE THE INPUT OF THE OUTER RALTED THE THE FLEET SIZE
    
    
    OPEN (UNIT=LOGFILENO,FILE='..\..\RESULTS\LOG.TXT',STATUS='REPLACE',ACTION="WRITE")

    ! STEP 1 READ INPUT DATA
    CALL READPARA
    CALL WRITEPARA
    CALL READNETWORK(FIRSTOUT, LASTOUT)
    call Ini_MyLines(mylines)
    ! intilize my new added links
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
    
    !do l = 1, NLINE
    !    NewFleet(L) = 5
    !ENDDO 
    
    NewFleet(1) = 2
    NewFleet(2) = 1
    NewFleet(3) = 2
    NewFleet(4) = 1
    
    call set_sol_fre(test_sol, NewFleet)
    call EvaluateSol(test_sol,FIRSTOUT,LASTOUT,XFA)

    !call Test_One_Fleet_Sol(NewFleet,FIRSTOUT,LASTOUT,XFA)

    !CALL Get_TotalCost(XFA,FX)
    WRITE(*,*) "TT = ", TotalCost
    
    
    


    END PROGRAM


    function Get_TotalCost(FLOW,Y) result(TT)
    !SUBROUTINE Get_TotalCost(FLOW,Y)
    USE  GRAPH
    IMPLICIT NONE
    INTEGER I,J
    REAL*8:: TT
    REAL*8::FLOW(NL,NDEST),Y(NL,NDEST)
    INTEGER Q,W
    TT = 0

    OPEN(33,FILE='..\..\RESULTS\OD_UE.TXT')
    DO Q = 1, NOD
        DO W=1,NDEST
            IF (ROOTS(W).EQ.DEST(Q)) THEN
                DO J = 1, NL
                    IF (ANODE(J).EQ.ORIGIN(Q)) THEN
                        IF (FLOW(J,W)>0.0001) THEN
                            TT = Y(J,W) * FLOW(J,W) + TT
                            WRITE(33,"(I3,A,I2,A,I3,A,F8.4,A,F8.4,A,F8.4)") &
                                CASEINDEX,',',ORIGIN(Q),',',DEST(Q),',', DEMAND(Q),',',Y(J,W),',',FLOW(J,W)
                            EXIT
                        ENDIF
                    ENDIF
                ENDDO
            ENDIF
        ENDDO
    ENDDO
    close(33)

    END FUNCTION

    
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
    
    ! step 1: update test fleet for each line
    call set_myline_fleet(NewFleet)
    ! step 2: update line frequency 
    call Update_LineFre(NewFleet)
    ! step 3: update sec frequency
    call Update_SecFre
    
    CALL LOWERLEVEL(XFA,FIRSTOUT, LASTOUT)
    CALL OUTPUTOD(XFA, FX)
    !TotalCost =  Get_TotalCost(XFA,FX)
    
    
    end subroutine