!projection on the simplex splace
		SUBROUTINE SIMPLEX_PROJECTION(X,Z,D,N)
		IMPLICIT NONE
		INTEGER,INTENT(IN)::N
		REAL*8,INTENT(IN)::D
		REAL*8,DIMENSION(N),INTENT(IN)::Z
		REAL*8,DIMENSION(N),INTENT(OUT)::X
		REAL*8:: SUM_K,GET_SUM
		INTEGER::I
		LOGICAL ST
		REAL*8 dnor
		INTEGER,DIMENSION(N)::SET_I
		X=0.0
	

		DO I=1,N
		!	X(I)=Z(I)+(D-SUM(Z(:)))/(N*1.0)
			X(I)=Z(I)+(D-GET_SUM(Z(1:N),N))/(N*1.0)
		END DO 
		
10		SET_I=0
		ST=.TRUE.
		SUM_K=0.0
		DO I=1,N
			IF (X(I)>0.0) THEN
				SET_I(I)=1
				SUM_K=SUM_K+X(I)
			ENDIF 
			IF (X(I)<0.0) THEN 
				ST=.FALSE.
			END IF 
		END DO
		IF (ST) THEN
			RETURN
		END IF 
		dnor = (SUM(SET_I(:))*1D0)
		DO I=1,N
			IF (SET_I(I)==0) THEN 
				X(I)=0.0
			ELSE IF (SET_I(I)==1) THEN 
				X(I)=X(I)+(D-SUM_K)/dnor
			END IF 
		END DO 
		GO TO 10 

		END SUBROUTINE



		REAL*8 FUNCTION GET_SUM(X,N)
		IMPLICIT NONE 
		INTEGER N
		REAL*8 X(N)
		INTEGER I
		GET_SUM=0.0
		DO I=1,N
			GET_SUM=GET_SUM+X(I)
		END DO 
		RETURN 
		END 