	
    REAL*8 FUNCTION FACT(N)
 	IMPLICIT NONE
	INTEGER J,N
	FACT=1
	DO J=2,N
		FACT=FACT*J
	END DO
	RETURN
	END FUNCTION
    
    SUBROUTINE LINK_TIME(X0,LTIME)
	USE  GRAPH
	IMPLICIT NONE 
	REAL*8:: X0(NL)  ! THIS IS LINK FLOW 
	REAL*8:: VEHICLE_MEAN(NL),VEHICLE_VAR(NL)
	REAL*8:: WAIT_MEAN(NL),WAIT_VAR(NL)
	REAL*8:: CONGEST_MEAN(NL),CONGEST_VAR(NL)
	REAL*8:: CFLOW(NL)
	REAL*8,INTENT(OUT):: LTIME(NL)
	INTEGER I,J,L1,L2
	REAL*8:: FACT
	INTEGER::N,KJ


	N=CONGESTION_N

	VEHICLE_MEAN=SCOST
	VEHICLE_VAR=SVAR

	WAIT_MEAN=1/SF
	WAIT_VAR=(1/SF)**2

	CFLOW=0.0
	DO I=1,NL
		CFLOW(I)=CFLOW(I)+X0(I)  !own section
	!	DO KJ=1,NumCom(I)
        if (NumCom(i)>0) then  ! num of competting section 1
            DO KJ=LocateCompete(I),LocateCompete(I) + NumCom(I) - 1
            !		J=COMPETE(I,KJ)
                J=CompeteSec(KJ)
                    DO L1=1,SLC(I)
                        DO L2=1,SLC(J)
                            IF (SL(I,L1)==SL(J,L2).AND.SL(I,L1)/=0) THEN 
                                CFLOW(I)=CFLOW(I)+X0(J)*FRE(SL(I,L1))/SF(J)
                            ENDIF 
                        ENDDO 
                    ENDDO 
            ENDDO 
        endif
	ENDDO 
	
	CONGEST_MEAN=BS*FACT(N)*((CFLOW/(gama*Capk*SF))**N)
	CONGEST_VAR=(BS**2)*(FACT(2*N)-FACT(N)**2)*(CFLOW/(gama*Capk*SF))**(2*N)
	
	LTIME=(VEHICLE_MEAN+WAIT_MEAN+CONGEST_MEAN)+RIO*(VEHICLE_VAR+WAIT_VAR+CONGEST_VAR) + fare
    
!	LTIME=VEHICLE_MEAN+WAIT_MEAN+CONGEST_MEAN
	
	RETURN 
	END SUBROUTINE 
	
	
 
