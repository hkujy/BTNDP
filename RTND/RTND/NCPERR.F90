!this contains two subroutine to measure the error
		SUBROUTINE NCP_ERROR(MADF,XFA,INFX)
        !NCP_ERROR(NCPERR,X0,NCPFX)
		USE GRAPH
		IMPLICIT NONE

		REAL*8,INTENT(OUT)::MADF
		INTEGER I,NR
		REAL*8,INTENT(IN)::XFA(NL,NDEST),INFX(NL,NDEST)
		OPEN(11,FILE='..\..\RESULTS\Fortran_CheckMadf.txt') 
       ! OPEN(11,FILE='..\RESULTS\CheckMadf.txt',position="append")
        !OPEN(12,FILE='..\RESULTS\CheckMadfViaDP.txt',position="append")
		MADF=0.0
		DO NR=1,NDEST
			DO I=1,NL
				IF (SUBLINK(I,NR).AND.SUBNODE(ANODE(I),NR).AND.SUBNODE(BNODE(I),NR)) THEN
					IF (ANODE(I)/=ROOTS(NR)) THEN
						!IF (XFA(I,NR).GT.FLOW_EPS.AND.(INFX(I,NR)-NDIST(ANODE(I),NR))>MADF) THEN 
  !                      IF(I.EQ.4481.AND.ANODE(I).EQ.308.AND.NR.EQ.44) THEN
   !                         WRITE(12,'(I5,2X,I5,2X,I5,2X,F6.2,2X,F6.2,2X,F6.2)') I,ANODE(I),NR,XFA(I,NR),INFX(I,NR),NDIST(ANODE(I),NR)
    !                    END IF
						IF (XFA(I,NR).GT.0.1.AND.(INFX(I,NR)-NDIST(ANODE(I),NR))>MADF) THEN 
							MADF=INFX(I,NR)-NDIST(ANODE(I),NR)
							WRITE(11,'(i3,a,I5,a,I5,a,I5,a,F6.2,a,F6.2,a,F6.2,a,F6.2)') &
                                caseindex,',',I,',',ANODE(I),',',NR,',',XFA(I,NR),',',INFX(I,NR),',',NDIST(ANODE(I),NR),',',MADF
						END IF 
					ENDIF
				ENDIF
			END DO
		END DO 
		CLOSE(11)
	!	PAUSE
    
     
        
        RETURN 
		END SUBROUTINE 


!		SUBROUTINE ERROR_Euclidean_distance(F0,F1,N1,N2,ERROR)
!		IMPLICIT NONE

!		INTEGER,INTENT(IN)::N1,N2
!		REAL*8,DIMENSION(N1,N2),INTENT(IN)::F0,F1
!		REAL*8::ERROR
!		INTEGER I,J
!		ERROR=0.0
!		DO I=1,N1
!			DO J=1,N2
!				ERROR=ERROR+(F1(I,J)-F0(I,J))**2
!			END DO 
!		ENDDO 
!		ERROR=sqrt(Error)
!	!	WRITE(*,*) ERROR
!		RETURN 
 !   END SUBROUTINE 

    
		SUBROUTINE ERROR_Euclidean_distance(F1,F0,N1,N2,ERROR)
		IMPLICIT NONE

		INTEGER,INTENT(IN)::N1,N2
		REAL*8,DIMENSION(N1,N2),INTENT(IN)::F0,F1
		REAL*8::ERROR
		INTEGER I,J
        real*8::sum
        sum = 0.0
		ERROR=0.0
		DO I=1,N1
			DO J=1,N2
				ERROR=ERROR+abs(F1(I,J)-F0(I,J))
                sum=sum+F0(I,J)
			END DO 
		ENDDO 
		ERROR=Error/sum
	!	WRITE(*,*) "err = ", ERROR
		RETURN 
		END SUBROUTINE 
