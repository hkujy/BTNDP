		SUBROUTINE READ_DATA(FIRSTOUT,LASTOUT)
		USE  GRAPH
		IMPLICIT NONE

		REAL*8 C
	!	REAL*8 F,H,ICOUNT,JCOUNT,L1,L2,N,N1,N2,VA
		INTEGER,PARAMETER::INPUTSEED = 1
		INTEGER::I,J,K
		INTEGER::O,D,COUNTLINE
		REAL*8::DTA(4)
		REAL*8::R
		INTEGER checkmaxcom
		INTEGER,intent(out)::FIRSTOUT(NN),LASTOUT(NN)
		INTEGER,PARAMETER::NS=70   ! MAX NUMBER OF STOPS FOR ON LINE
		INTEGER::ODPAIR,L
		INTEGER::COMPETE(NL,MaxCom)
		INTEGER::COUNTCOMPETE
		REAL*8::LCOST(NN,NN)	! LINK TRAVEL COST MATRIX each pair of node
		REAL*8::LVAR(NN,NN)
		REAL*8::SLF(NL,NLINE)   ! SECTION FREQUENCY SUM; SECTION LINE FREQUENCY
		! NUMBER OF LINES,NUMBER OF STOPS OF A LINE
		! NN NUMBER OF NODES
		REAL*8::TEMP	! SECTION COST
	!	REAL*8::DEM(NN,NN)
		REAL*8::TFA(NL),TVA(NL)     ! seems to be a temp cost
	! MAX NUMBER OF SECTIONS
		INTEGER::LINE(NLINE,NS)
		INTEGER::HEAD,TAIL,HEAD1,HEAD2
	! HEAD IS THE END NODE
	! TAIL IS THE START NODE
		INTEGER::SECTION(NN,NN)
		INTEGER::S,S1,S2
		INTEGER::M,MSTOP,NOW,NEXT
	! SECTION(BNODE,ANODE)
		INTEGER::SC			!SECTION COUNTER
		INTEGER::SECTION_LINE(NL,MaxSecLine) ! SECTION_LINE INDEX
		INTEGER::AN(NL),BN(NL)  ! HEAD AND TAIL NODE BASED ON SECTION INDEX
		INTEGER::SECTION_LINE_COUNT(NL)! SECTION LINE COUNTER
		INTEGER::INPUTSTOPS(NS)	
!		INTEGER DIF(NN)
		integer::CheckMaxSecLine
		INTEGER, ALLOCATABLE :: new (:), old(:)
 
		CALL RANDOM_SEED ( )  ! Processor reinitializes the seed
                       ! randomly from the date and time
		CALL RANDOM_SEED (SIZE = I)  ! I is set to the size of
                              ! the seed array
		ALLOCATE (new(I))
		ALLOCATE (old(I))
		CALL RANDOM_SEED (GET=old(1:I)) ! Gets the current seed
		new = INPUTSEED
		CALL RANDOM_SEED (PUT=new(1:I)) ! Sets seed from array
                                 ! new
!		DO I =1, 10
		CALL RANDOM_NUMBER(R)
!		WRITE(*,*) R
!		END DO 
!
        
		!OPEN(1,FILE='..\Input\trips.txt')
		!OPEN(1,FILE='..\LargeNetworkInPut\AllLinks.txt')
		OPEN(1,FILE='C:\GitCodes\ApproachCode\TestNetwork\AllLinks.txt')
		!OPEN(2,FILE='..\Input\cost.txt')
		!OPEN(3,FILE='..\Input\demand.txt')
	
!		write(*,*) "Start read data"

		SINDEX=0
		FRE=4.0
		FRE=FRE/60.0	  !frequency per minute

!***************Read link cost and set link var***************************
		DO I = 1, 2369
			READ(1,*) O,D,C
			LCOST(O,D)=C
			CALL RANDOM_NUMBER(R)
			LVAR(O,D)=LCOST(O,D)*R
		ENDDO 
		close(1)
!************************************************
		SLC=0
		SL=0
		AN=0
		BN=0
		LINE=0
! ReadLine stops NUMBER
		
        !OPEN(1,FILE='..\LargeNetworkInPut\NumLineStops.txt')
        OPEN(1,FILE='C:\GitCodes\ApproachCode\TestNetwork\NumLineStops.txt')
		DO I = 1, NLINE
			READ(1,*) J,K
			LineStops(J) = K
		ENDDO 
		CLOSE(1)


		! ReadLinE STOPS 
		!OPEN(1,FILE='..\LargeNetworkInPut\PutStops.txt')	
		OPEN(1,FILE='C:\GitCodes\ApproachCode\TestNetwork\PutStops.txt')	
		DO I = 1, NLINE
			INPUTSTOPS = 0
			READ(1,*) INPUTSTOPS(1:LineStops(I))
			DO J = 1, LineStops(I)
				LINE(I,J) = INPUTSTOPS(J)
			END DO 
		ENDDO 
		DO I = 1, NLINE
			DO J = 1, LineStops(I)
				IF (LINE(I,J)==0) THEN 
					WRITE(*,*) "INPUT LINE S =0"
				ENDIF
			ENDDO 
		ENDDO 
		close(1)


!Read OD pairs
		!OPEN(1,FILE='..\LargeNetworkInPut\ODpairs.txt')
		OPEN(1,FILE='C:\GitCodes\ApproachCode\TestNetwork\ODpairs.txt')
		DO I=1,NOD
			READ(1,*) DTA(:)
			ODPAIR=DTA(1)
			ORIGIN(ODPAIR)=DTA(2)
			DEST(ODPAIR)=DTA(3)
			DEMAND(ODPAIR)=DTA(4)
		ENDDO 
		CLOSE(1)
            
		!Read route
		!OPEN(1,FILE='..\LargeNetworkInPut\DestNodeSet.txt')
		OPEN(1,FILE='C:\GitCodes\ApproachCode\TestNetwork\DestNodeSet.txt')
		DO I = 1,NDEST
			READ(1,*) J,K
			ROOTS(J) = K
		ENDDO 

! output for the following code procedures
! manualy set these to be input
!        NumCom = 


        AN(1) = 1
        BN(1) = 4
        SLC(1) = 1
        SL(1,1) = 1
      
        AN(2) = 1
        BN(2) = 2
        SLC(2) = 1
        SL(2,1) = 2

        AN(3) = 2
        BN(3) = 3
        SLC(3) = 2
        SL(3,1) = 2
        SL(3,2) = 3

        AN(4) = 3
        BN(4) = 4
        SLC(4) = 2
        SL(4,1) = 3
        SL(4,2) = 4 

        AN(5) = 1
        BN(5) = 3
        SLC(5) = 1
        SL(5,1) = 2

        AN(6) = 2
        BN(6) = 4
        SLC(6) = 1
        SL(6,1) = 3
    
        SCOST(1) = 25
        SCOST(2) = 7
        SCOST(3) = 5.4
        SCOST(4) = 9
        SCOST(5) = 13
        SCOST(6) = 8

        SVAR(1) = 3
        SVAR(2) = 12
        SVAR(3) = 6.8
        SVAR(4) = 15.8
        SVAR(5) = 35
        SVAR(6) = 14
        

        LCOST(1,4) = 25
        LCOST(1,2) = 7
        LCOST(2,3) = 5.4 
        LCOST(3,4) = 9
        LCOST(1,3) = 13 
        LCOST(2,4) = 8 

        
        LVAR(1,4) = 25
        LVAR(1,2) = 7
        LVAR(2,3) = 5.4 
        LVAR(3,4) = 9
        LVAR(1,3) = 13 
        LVAR(2,4) = 8 
        
        

		!COMPETE=0  record the number of compete sections
        COMPETE = 0 
        NumCom(2) = 1
        COMPETE(2,1) = 5
        NumCom(3) = 2
        COMPETE(3,1) = 6 
        COMPETE(3,2) = 5
        NumCom(4) = 1
        COMPETE(4,1) = 6
        NumCom(5) = 1
        COMPETE(5,1) = 2
        NumCom(6) = 1
        COMPETE(6,1) = 3

        !Compete (i.j) the jth competive section in section i

!*******************************************************************

        NumCom = 0 
        SC=0
        SECTION=0
        SCOST=0.0
!		FORWORD CHECK 
        DO I=1,NLINE
              DO S=1,NS-1
                    DO S2=S+1,NS
                          IF(LINE(I,S2)/=0) THEN 
                                TAIL=LINE(I,S)
                                HEAD=LINE(I,S2)
                                MSTOP=S2-S  ! middle stops
                                IF (SECTION(TAIL,HEAD)==0) THEN 
                                  SECTION(TAIL,HEAD)=1
                                  SC=SC+1
                                  AN(SC)=HEAD
                                  BN(SC)=TAIL
                                  SLC(SC)=SLC(SC)+1
                                  SL(SC,SLC(SC))=I
                                      ! get cost of the section
                                  NOW=S
                                  DO M=1,MSTOP
                                    NEXT=NOW+1
                                    SCOST(SC)=SCOST(SC)+FRE(I)*LCOST(LINE(I,NOW),LINE(I,NEXT))
                                    SVAR(SC)=SVAR(SC)+FRE(I)*FRE(I)*LVAR(LINE(I,NOW),LINE(I,NEXT))
                                    NOW=NEXT
                                  END DO 
                                ELSE IF (SECTION(TAIL,HEAD)==1) THEN 
                                ! if already defined section then find the secion
                                  DO J=1,SC
                                  ! get the corresponding line index
                                    IF (AN(J)==HEAD.AND.BN(J)==TAIL) THEN 
                                      SLC(J)=SLC(J)+1
                                      SL(J,SLC(J))=I
                                      NOW=S
                                      DO M=1,MSTOP
                                        NEXT=NOW+1
                                        SCOST(J)=SCOST(J)+FRE(I)*LCOST(LINE(I,NOW),LINE(I,NEXT))
                                        SVAR(J)=SVAR(J)+FRE(I)*FRE(I)*LVAR(LINE(I,NOW),LINE(I,NEXT))
                                        NOW=NEXT
                                      END DO
                                        ENDIF 
                              END DO 
                            END IF 
                          ENDIF 
				END DO 
			END DO 
		ENDDO 
! append walking links to the sections		
		OPEN(5,FILE='..\LargeNetworkInPut\WalkLinks.txt')
		DO I = 1,795
			READ(5,*) O,D,C
			HEAD=O
			TAIL=D
			SC = SC + 1
			AN(SC) = HEAD
			BN(SC) = TAIL
			SLC(SC)= 0 
			LCOST(TAIL,HEAD)=C
			LVAR(TAIL,HEAD)=0.0
		enddo 
		close(5)

	CheckMaxSecLine = 0
	DO I =1,SC
		IF (SLC(I)>CheckMaxSecLine) THEN
		CheckMaxSecLine = SLC(I)
		END IF 
	END DO 
!	WRITE(*,*) "chekcMaxSl = ",CheckMaxSecLine


! CONVERT SECTION COST
		DO I=1,SC
			TEMP=0.0
			DO J=1,SLC(I)
				TEMP=TEMP+FRE(SL(I,SLC(I)))  
				! GET THE FREQUENCY SUMMATION
			END DO 
			IF (TEMP==0.0) THEN
				SCOST(I) = WalkCost
			ELSE
				SCOST(I)=SCOST(I)/TEMP
				SVAR(I)=SVAR(I)/(TEMP*TEMP)
			END IF
		END DO 
		! RE INITILIZE LINE COST--WHICH IS EQUALS TO SECTION COST
		LCOST=0.0
		LVAR=0.0
		DO I=1,SC
			HEAD=AN(I)
			TAIL=BN(I)
			LCOST(TAIL,HEAD)=SCOST(I)
			LVAR(TAIL,HEAD)=SVAR(I)
		END DO 
! add waling king 

!************************************************************************************	
		COUNTLINE=1
		DO I=1,NN
			FIRSTOUT(I)=COUNTLINE
			DO J=1,NN
				IF(LCOST(I,J)>0.0) THEN 
!					LCOST(TAIL,HEAD)=SCOST(I)
					ANODE(COUNTLINE)=I
					BNODE(COUNTLINE)=J
					TFA(COUNTLINE)=LCOST(I,J)
					TVA(COUNTLINE)=LVAR(I,J)
					DO K=1,SC
						IF (AN(K)==J.AND.BN(K)==I) THEN 
						SINDEX(COUNTLINE)=K
						END IF 
					ENDDO 
					COUNTLINE=COUNTLINE+1
				END IF
			END DO 
			LASTOUT(I)=COUNTLINE-1
		END DO 

		COUNTLINE=1
		DO J=NN,1,-1
			FIRSTIN(J)=COUNTLINE
			DO I=1,NN
				IF(LCOST(I,J)>0.0) THEN 
					BACKANODE(COUNTLINE)=J
					BACKBNODE(COUNTLINE)=I
					COUNTLINE=COUNTLINE+1
				END IF
			END DO 
			LASTIN(J)=COUNTLINE-1
		END DO

		DO L=1,NL
			DO J=1,NL
				IF ((BACKANODE(L).EQ.BNODE(J)).AND.(BACKBNODE(L).EQ.ANODE(J))) THEN 
					BACKTOFORWARD(L)=J
				END IF 
			END DO 
		ENDDO
!		WRITE(*,*) "CHECK CONSIT"

		DO I=1,NL
			L = BACKTOFORWARD(I)
			IF ((ANODE(L).NE.BACKBNODE(I)).OR.(BNODE(L).NE.BACKANODE(I))) THEN 
!			WRITE(*,*) "BUG INCONSIT"
			END IF 
		END DO 
	!	DIF = LASTIN - FIRSTIN

! CHECK COMPETE SECTION BY LINE SECQUENCE
GO TO 24
		COMPETE=0
		DO L=1,NLINE
		write(*,*) L
			DO S=1,NS-2
				IF (LINE(L,S)/=0) THEN 
					TAIL=LINE(L,S)
					DO S1=S+1,NS-1
						IF(LINE(L,S1)/=0) THEN 
							HEAD1=LINE(L,S1)
							DO S2=S1+1,NS
								IF (LINE(L,S2)/=0) THEN 
									HEAD2=LINE(L,S2)
									DO I=1,SC
										IF (AN(I)==HEAD1.AND.BN(I)==TAIL) THEN 
											DO J=1,SC
												IF (AN(J)==HEAD2.AND.BN(J)==TAIL) THEN 
													!COMPETE(I,J)=1
													NumCom(I)=NumCom(I)+1
													COMPETE(I,NumCom(I)) = J
												ENDIF 
											ENDDO 
										ENDIF
									ENDDO 
									DO I=1,SC
										IF (AN(I)==HEAD2.AND.BN(I)==TAIL) THEN 
											DO J=1,SC
												IF (AN(J)==HEAD2.AND.BN(J)==HEAD1) THEN 
												!	COMPETE(J,I)=1
													NumCom(J)=NumCom(J)+1
													COMPETE(J,NumCom(J)) = I
												ENDIF 
											ENDDO 
										ENDIF 
									ENDDO 
								END IF 
							ENDDO 
						ENDIF 
					ENDDO 
				ENDIF 
			ENDDO 
		ENDDO 
	
	OPEN(2,FILE='..\LargeNetworkInPut\WriteCom1.txt')
	OPEN(3,FILE='..\LargeNetworkInPut\WriteCom2.txt')
	checkmaxcom = 0
	do i =1,NL
		if (NumCom(i)>checkmaxcom) then
			checkmaxcom = NumCom(i)
		end if
		write(2,*) NumCom(i)
		DO J =1, 20
			WRITE(3,*) COMPETE(I,(J-1)*50+1:J*50)
		END DO 
	end do 
!	stop
!	pause
	
22	OPEN(3,FILE='..\LargeNetworkInPut\WriteCom2.txt')
	do I =1,NL
!	WRITE(*,*) i
		DO J =1, 20
			READ(3,*) COMPETE(I,(J-1)*50+1:J*50)
		END DO 
	end do 
!	write(*,*) "Check Max Compete =",checkmaxcom
	OPEN(4,FILE='..\LargeNetworkInPut\CheckWriteCom2.txt')

	do I =1,NL	
		DO J =1, 20
			WRITE(4,*) COMPETE(I,(J-1)*50+1:J*50)
		END DO 
	end do 
	COUNTCOMPETE = 1
	DO I = 1,NL
		LocateCompete(I) = COUNTCOMPETE
		IF (SUM(COMPETE(I,:)).EQ.0) THEN 
			LocateCompete(I) = 0
		ELSE
			DO J = 1,1000
				IF (COMPETE(I,J).NE.0)THEN
				CompeteSec(COUNTCOMPETE) = COMPETE(I,J)
				COUNTCOMPETE = COUNTCOMPETE + 1
				END IF
			END DO 	
		END IF
	ENDDO 
	OPEN(3,FILE='..\LargeNetworkInPut\WriteLocatCompete.txt')
		DO I = 1,NL
			WRITE(3,*) LocateCompete(I)
		END DO 
	OPEN(4,FILE='..\LargeNetworkInPut\WriteCompetSec.txt')	
		DO I = 1,2626758
			WRITE(4,*) 	CompeteSec(I)
		End DO 
	close(3)
	CLOSE(4)
	
24	OPEN(3,FILE='..\LargeNetworkInPut\WriteLocatCompete.txt')
		DO I = 1,NL
			READ(3,*) LocateCompete(I)
		END DO 
		LocateCompete(NL+1) = 0
	OPEN(4,FILE='..\LargeNetworkInPut\WriteCompetSec.txt')	
		DO I = 1,262754
			READ(4,*) CompeteSec(I)
		End DO 
	close(3)
	CLOSE(4)
	
	
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!	
	DO I=1,NL
		S=SINDEX(I)
		SECTION_LINE(I,:)=SL(S,:)
		SECTION_LINE_COUNT(I)=SLC(S)
	ENDDO 
	SL=SECTION_LINE
	SLC=SECTION_LINE_COUNT 
	SF=0.0
	DO I=1,NL
		DO J=1,SLC(I)
			SF(I)=SF(I)+FRE(SL(I,J))
		ENDDO 
		! if SF = 0, THEN It is walking link
		IF (SF(I).EQ.0) THEN 
			SF(I)=1000000.0
		END IF
	ENDDO 
	SLF=0.0
	DO I=1,NL
		DO J=1,SLC(I)
			SLF(I,SL(I,J))=FRE(SL(I,J))/SF(I)
		ENDDO 
	END DO 
	

!	MAXOUT=0
!	DO I = 1,NN
!		IF ((LASTOUT(i) -  FIRSTOUT(I)).gt.maxout) then 
!			MAXOUT = LASTOUT(i) -  FIRSTOUT(I)
!			if (maxout.eq.366) then 
!				write(*,*) i
!			end if
!		end if 
!	end do 

	SCOST=TFA
	SVAR=TVA
	do i=1,Nn
		DO J =FIRSTOUT(I),LASTOUT(I)
			IF ((I.ne.anode(J))) then
			write(*,*) "anode(J) err"
			end if 
		END DO
	end do 
!	pause
	CLOSE(1)
	!CLOSE(2)
	!CLOSE(3)
	DO I=1,NN
		DO L = FIRSTIN(I),LASTIN(I)
			IF (BNODE(BACKTOFORWARD(L)).NE.I) THEN 
				WRITE(*,*) "FIRSTIN ERR"
			END IF
		END DO
		DO L = FIRSTOUT(I),LASTOUT(I)
			IF (ANODE(L).NE.I) THEN 
			WRITE(*,*) "FIRSTOUT ERR"
			END IF 
		END DO 
	END DO 

!	write(*,*) "finished readdate"
!		


	END SUBROUTINE 
	

