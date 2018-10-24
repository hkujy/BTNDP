
    ! the update tsl value given cost components
    
    
    subroutine UpdateSectionCost
    use ConstPara
    use DEFINE
    implicit none


    tsl(1,4) = 1
    tsl(2,4) = 1
    tsl(3,4) = fre(2)/(fre(2)+fre(3))
    tsl(4,4) = fre(3)/(fre(2)+fre(3))
    tsl(5,4) = fre(3)/(fre(3)+fre(4))
    tsl(6,4) = fre(4)/(fre(3)+fre(4))
    tsl(7,4) = 1
    tsl(8,4) = 1

    SCOST(1) = tsl(1,1)*tsl(1,4)
    scost(2) = tsl(2,1)*tsl(2,4)
    scost(3) = tsl(3,1)*tsl(3,4) + tsl(4,1)*tsl(4,4)
    scost(4) = tsl(5,1)*tsl(5,4) + tsl(6,1)*tsl(6,4)
    scost(5) = tsl(7,1)*tsl(7,4) + tsl(7,1)*tsl(7,4)
    scost(6) = tsl(8,1)*tsl(8,4)

    Svar(1) = tsl(1,2)*(tsl(1,4)**2)
    svar(2) = tsl(2,2)*(tsl(2,4)**2)
    svar(3) = tsl(3,2)*(tsl(3,4)**2) + tsl(4,2)*(tsl(4,4)**2)
    svar(4) = tsl(5,2)*(tsl(5,4)**2) + tsl(6,2)*(tsl(6,4)**2)
    svar(5) = tsl(7,2)*(tsl(7,4)**2) + tsl(7,2)*(tsl(7,4)**2)
    svar(6) = tsl(8,2)*(tsl(8,4)**2)

    fare(1) = tsl(1,3)*tsl(1,4)
    fare(2) = tsl(2,3)*tsl(2,4)
    fare(3) = tsl(3,3)*tsl(3,4) + tsl(4,3)*tsl(4,4)
    fare(4) = tsl(5,3)*tsl(5,4) + tsl(6,3)*tsl(6,4)
    fare(5) = tsl(7,3)*tsl(7,4) + tsl(7,3)*tsl(7,4)
    fare(6) = tsl(8,3)*tsl(8,4)

    end subroutine


    subroutine update_secFre
    use DEFINE
    implicit none
    INTEGER I,J

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


    end subroutine


    SUBROUTINE READNETWORK(FIRSTOUT,LASTOUT)
    USE GRAPH
    IMPLICIT NONE

    INTEGER::I,J,K,O,D,COUNTLINE,ODPAIR,L,COUNTCOMPETE
    INTEGER::COMPETE(NL,MaxCom), temp_compete(NL,MaxCom)
    REAL*8::DTA(4)
    INTEGER,intent(out)::FIRSTOUT(NN),LASTOUT(NN)
    REAL*8::LCOST(NN,NN)	! LINK TRAVEL COST MATRIX each pair of node
    REAL*8::LVAR(NN,NN)     ! LINK VARIANCE
    !REAL*8::SLF(NL,NLINE)   ! SECTION FREQUENCY SUM: SECTION LINE FREQUENCY
    REAL*8::TempReal	! SECTION COST
    REAL*8::TFA(NL),TVA(NL)     ! seems to be a temp cost
    INTEGER::SIndexReverse(nl)  ! this only for the small network to compute
    !INTEGER::HEAD,TAIL,HEAD1,HEAD2
    ! Tail --> Head
    !INTEGER::SECTION(NN,NN)
    INTEGER::S,S1,S2,M,MSTOP,NOW,NEXT
    ! SECTION(BNODE,ANODE)
    INTEGER::SC			!SECTION COUNTER
    INTEGER::SECTION_LINE(NL,MaxSecLine) ! SECTION_LINE INDEX
    INTEGER::AN(NL),BN(NL)  ! HEAD AND TAIL NODE BASED ON SECTION INDEX
    INTEGER::SECTION_LINE_COUNT(NL)! SECTION LINE COUNTER
    INTEGER::INPUTSTOPS(MaxLineStops)
    !	INTEGER DIF(NN)
    INTEGER, ALLOCATABLE :: NewSeed (:), OldSeed(:)
    INTEGER::TempNumCom(NL)
    real*8::IniFre(NLINE)







    CALL RANDOM_SEED ( )  ! Processor reinitializes the seed
    ! randomly from the date and time
    CALL RANDOM_SEED (SIZE = I)  ! I is set to the size of
    ! the seed array
    ALLOCATE (NewSeed(I))
    ALLOCATE (OldSeed(I))
    CALL RANDOM_SEED (GET=OldSeed(1:I)) ! Gets the current seed
    NewSeed = INPUTSEED
    CALL RANDOM_SEED (PUT=NewSeed(1:I)) ! Sets seed from array
    !		DO I =1, 10
    CALL RANDOM_NUMBER(TempReal)
    !		WRITE(*,*) R
    !		END DO
    !

    !OPEN(1,FILE='..\Input\trips.txt')
    !OPEN(1,FILE='..\LargeNetworkInPut\AllLinks.txt')
    !OPEN(1,FILE='..\TestNetwork\AllLinks.txt')
    !OPEN(2,FILE='..\Input\cost.txt')
    !OPEN(3,FILE='..\Input\demand.txt')

    ! step 1
    ! Read OD Pairs
    OPEN(1,FILE='..\..\TestNetwork\ODpairs.txt')
    DO I=1,NOD
        READ(1,*) DTA(:)
        ODPAIR=DTA(1)
        ORIGIN(ODPAIR)=DTA(2)
        DEST(ODPAIR)=DTA(3)
        DEMAND(ODPAIR)=DTA(4)
    ENDDO
    CLOSE(1)

    ! Step 2: Read Line and stops

    !ReadLine stops NUMBER
    OPEN(1,FILE='..\..\TestNetwork\NumLineStops.txt')
    DO I = 1, NLINE
        READ(1,*) J,K
        LineStops(J) = K
    ENDDO
    CLOSE(1)

    !ReadLinE STOPS
    OPEN(1,FILE='..\..\TestNetwork\PutStops.txt')
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

    !Read root nodes for the assignment
    OPEN(1,FILE='..\..\TestNetwork\DestNodeSet.txt')

    DO I = 1,NDEST
        READ(1,*) J,K
        ROOTS(J) = K
    ENDDO
    close(1)

    ! read line sectine cost variance and fare data
    OPEN(1,FILE='..\..\TestNetwork\LineSectionData.txt')
    do i = 1, 8
        read(1,*) tsl(i,1:3)   ! cost, var, fare
    enddo
    close(1)
    ! this is the initial frequency setting
    OPEN(1,FILE='..\..\TestNetwork\IniFre.txt')
    DO I = 1, NLINE
        READ(1,*) TempReal
        fre(i) = TempReal
        inifre(i) = TempReal
    ENDDO
    FRE=FRE/60.0	  !frequency per minute
    CLOSE(1)

    SINDEX=0
    SLC=0
    SL=0
    AN=0
    BN=0
    LINE=0


    ! network structure
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


    ! input compete section index
    !COMPETE=0  record the number of compete sections
    NumCom = 0
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


    call UpdateSectionCost

    !***************Read link cost and set link var***************************
    !************************************************

    OPEN(1,FILE='..\..\RESULTS\Fortran_6LINKDATA.txt')
    write(1,*) "Check the initial input for the  6link network data"

    do i = 1, 6
        write (1,'(f6.2,a,f6.2,a,f6.2)') SCOST(I),',', SVAR(I),',',fare(I)
    ENDDO
    close(1)

    OPEN(1,FILE='..\..\RESULTS\Fortran_LineCostAndVar.txt')
    write(1,*) "Check the initial input for the  6link network data"

    write(1,'(f6.2,a,f6.2)') tsl(1,1),',',tsl(1,2)  ! line 1
    write(1,'(f6.2,a,f6.2)') tsl(7,1),',',tsl(7,2)  ! line 2
    write(1,'(f6.2,a,f6.2)') tsl(8,1),',',tsl(8,2)  ! line 3
    write(1,'(f6.2,a,f6.2)') tsl(6,1),',',tsl(6,2)  ! line 4
    close(1)

    ! this just intitial cost
    LCOST(1,4) = SCOST(1)
    LCOST(1,2) = SCOST(2)
    LCOST(2,3) = SCOST(3)
    LCOST(3,4) = SCOST(4)
    LCOST(1,3) = SCOST(5)
    LCOST(2,4) = SCOST(6)


    LVAR(1,4) = SVAR(1)
    LVAR(1,2) = SVAR(2)
    LVAR(2,3) = SVAR(3)
    LVAR(3,4) = SVAR(4)
    LVAR(1,3) = SVAR(5)
    LVAR(2,4) = SVAR(6)


    !Compete (i.j) the jth competive section in section i
    call update_secFre

    !************************************************************************************
    SC = 6
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
                    !IF (AN(K)==J.AND.BN(K)==I) THEN
                    IF (BN(K)==J.AND.AN(K)==I) THEN
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

    DO I=1,NL
        S=SINDEX(I)
        SECTION_LINE(I,:)=SL(S,:)
        SECTION_LINE_COUNT(I)=SLC(S)
    ENDDO
    SL=SECTION_LINE
    SLC=SECTION_LINE_COUNT

    ! add code to revise compete

    SIndexReverse(:)=(/3,1,4,6,2,5/)
    DO I=1,NL
        S=SINDEX(I)
        TempNumCom(I) = NumCom(S)
        do j =1, maxcom
            if (compete(s,j) > 0) then
                temp_compete(I,j)=SIndexReverse(Compete(S,j))
            endif
        enddo
    ENDDO
    compete = temp_compete
    NumCom =TempNumCom


    COUNTCOMPETE = 1
    DO I = 1,NL
        LocateCompete(I) = COUNTCOMPETE
        IF (SUM(COMPETE(I,:)).EQ.0) THEN
            LocateCompete(I) = 0
        ELSE
            !WRITE(*,*) I,size(compete(I,:))
            DO J = 1,10
                IF (COMPETE(I,J).NE.0)THEN
                    CompeteSec(COUNTCOMPETE) = COMPETE(I,J)
                    COUNTCOMPETE = COUNTCOMPETE + 1
                END IF
            END DO
        END IF
    ENDDO



    !***************************************************************
    ! TODO: check the dimension of SL and Section_Line

    !SF=0.0
    !DO I=1,NL
    !    DO J=1,SLC(I)
    !        SF(I)=SF(I)+FRE(SL(I,J))
    !    ENDDO
    !    ! if SF = 0, THEN It is walking link
    !    IF (SF(I).EQ.0) THEN
    !        SF(I)=1000000.0
    !    END IF
    !ENDDO
    !
    !SLF=0.0
    !DO I=1,NL
    !    DO J=1,SLC(I)
    !        SLF(I,SL(I,J))=FRE(SL(I,J))/SF(I)
    !    ENDDO
    !END DO


    SCOST=TFA
    SVAR=TVA
    do i=1,Nn
        DO J =FIRSTOUT(I),LASTOUT(I)
            IF ((I.ne.anode(J))) then
                write(*,*) "anode(J) err"
            end if
        END DO
    end do

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

    END SUBROUTINE


