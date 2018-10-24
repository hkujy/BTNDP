
    !THIS THE OUTER CYCLE PROBLEM TO TEST THE EFFECT OF DIFFERENT PARAMTERS
    !INCLUDE 'DEFINEVAR.F90'
    INCLUDE 'DEFINEVAR_TESTNET.F90'
    INCLUDE 'READNETWORK.F90'
    INCLUDE 'LINKTIME.F90'
    INCLUDE 'SIMPLEXPROJECTION.F90'
    INCLUDE 'SP.F90'
    INCLUDE 'INITIALSOL.F90'
    INCLUDE 'NORMVAL.F90'
    INCLUDE 'NCPERR.F90'
    INCLUDE 'WRITEOUTPUT.F90'
    INCLUDE 'DFS.F90'
    INCLUDE 'BACKWARDUPDATE.F90'
    INCLUDE 'FORWARDUPDATE.F90'
    INCLUDE 'RSPATH.F90'
    INCLUDE 'GETFX.F90'
    INCLUDE 'DPMAIN.F90'
    INCLUDE	'UPDATESUB.F90'
    INCLUDE 'MINSP.F90'
    INCLUDE 'CONNECT.F90'
    INCLUDE 'TORDER.F90'
    !INCLUDE 'SAM.F90'
    INCLUDE 'MSAFX.F90'
    !INCLUDE 'MSA&SAM.F90'
    INCLUDE 'SUBRSP.F90'

    SUBROUTINE LOWERLEVEL(XFA,FIRSTOUT, LASTOUT)
    USE GRAPH
    IMPLICIT NONE
    INTEGER I,J
    REAL*8  BETA,LAMA,V,MIU,TAU
    INTEGER MB, NB
    REAL*8 ERROR,BETASTEP,DISTANCEERR
    REAL*8 CPUTIME
    INTEGER TESTS
    INTEGER,intent(in)::FIRSTOUT(NN), LASTOUT(NN)
    REAL*8,INTENT(OUT)::XFA(NL,NDEST)

    !INTEGER,DIMENSION(3000,100,30)::ROUTES
    !ROUTES = 0

    ! 2-- DOUBLE PROJECTION
    ! 1---SAM

    ! READ DATA
    !ALLOCATE(FIRSTOUT(NN),LASTOUT(NN))
    !CALL READPARA
    !CALL READNETWORK(FIRSTOUT,LASTOUT)
    !CALL WRITEPARA
    !	STEP 1 FIXED BETA ! TEST OTHER THREE
    OPEN(14,FILE='..\..\RESULTS\FORTRAN_DP_PARA1.TXT')
    OPEN(16,FILE='..\..\RESULTS\FORTRAN_DP_PARA2.TXT')
    OPEN(17,FILE='..\..\RESULTS\FORTRAN_FINALERR.TXT',STATUS='OLD',POSITION='APPEND')


    ! JUST USE THE DOUBLE PROJECT METHOD

    ISCONVERGE = .FALSE.
    DO MB = 1, 3
        IF (MB==1) THEN
            BETASTEP = 0.33D0
        END IF
        IF (MB==2) THEN
            BETASTEP = 0.67D0
        END IF
        IF (MB==2) THEN
            BETASTEP = 0.99D0
        END IF
        LAMA = 1.99D0
        BETA = 1.0D0
        DO I = 1, 10
            V = 0.1D0*I
            DO J = 1, 10
                MIU = 0.1D0*J
                IF (MIU.LT.V) THEN
                    CALL DPMAIN(BETA,LAMA,V,MIU,ERROR,CPUTIME,BETASTEP,DISTANCEERR,FIRSTOUT,LASTOUT,XFA)
                    !CALL DPMAIN(BETA,LAMA,V,MIU,ERROR,CPUTIME,BETASTEP,DISTANCEERR)
                    IF (ISCONVERGE) THEN
                        WRITE(17,'(I3,A,F8.4)') CASEINDEX,',',ERROR
                        GOTO 999
                    ENDIF
                    WRITE(14,'(I3,A,F4.2,A,F4.2,A,F4.2)') CASEINDEX,',', LAMA,',', MIU,',',V
                    WRITE(16,'(I3,A,F4.2,A,I6,A,F7.4,A,F8.4,A,F10.6)') &
                        CASEINDEX,',',BETASTEP,',',SOLC,',',CPUTIME,',',ERROR,',',DISTANCEERR
                END IF
            END DO
        ENDDO
    ENDDO
    
    ! Compute Total Cost
    
    
    
    
    GOTO 999





!    !**********************TEST V AN MIU********************************************
!    TESTS = 2
!    ! READ CASE INDEX
!    OPEN(1,FILE='..\..\TESTNETWORK\CASEINDEX.TXT')
!    READ(1,*) CASEINDEX
!    CLOSE(1)
!
!    ! ONLY USE THE PROJECTION METHOD
!10  SELECT CASE (TESTS)
!
!    CASE (1)
!        BETASTEP = 0.33D0
!        LAMA = 1.8D0
!        MIU = 0.1D0
!        BETA = 1.0D0
!        V = 0.2D0
!        CALL DPMAIN(BETA,LAMA,V,MIU,ERROR,CPUTIME,BETASTEP,DISTANCEERR,FIRSTOUT,LASTOUT)
!        !WRITE(14,'(F4.2,A,F4.2,A,F4.2)') LAMA,',', MIU,',',V
!        !WRITE(16,'(F4.2,A,I6,A,F7.4,A,F8.4,A,F10.6)') BETASTEP,',',SOLC,',',CPUTIME,',',ERROR,',',DISTANCEERR
!        WRITE(17,'(I3,A,F8.4)') CASEINDEX,',', ERROR
!
!    CASE (2)
!
!        ISCONVERGE = .FALSE.
!        DO MB = 1, 3
!            IF (MB==1) THEN
!                BETASTEP = 0.33D0
!            END IF
!            IF (MB==2) THEN
!                BETASTEP = 0.67D0
!            END IF
!            IF (MB==2) THEN
!                BETASTEP = 0.99D0
!            END IF
!
!            LAMA = 1.99D0
!            BETA = 1.0D0
!            DO I = 1, 10
!                V = 0.1D0*I
!                DO J = 1, 10
!                    MIU = 0.1D0*J
!                    IF (MIU.LT.V) THEN
!                        CALL DPMAIN(BETA,LAMA,V,MIU,ERROR,CPUTIME,BETASTEP,DISTANCEERR,FIRSTOUT,LASTOUT)
!                        !CALL DPMAIN(BETA,LAMA,V,MIU,ERROR,CPUTIME,BETASTEP,DISTANCEERR)
!                        IF (ISCONVERGE) THEN
!                            WRITE(17,'(I3,A,F8.4)') CASEINDEX,',',ERROR
!                            GOTO 999
!                        ENDIF
!                        WRITE(14,'(I3,A,F4.2,A,F4.2,A,F4.2)') &
!                            CASEINDEX,',', LAMA,',', MIU,',',V
!                        WRITE(16,'(I3,A,F4.2,A,I6,A,F7.4,A,F8.4,A,F10.6)') &
!                            CASEINDEX,',',BETASTEP,',',SOLC,',',CPUTIME,',',ERROR,',',DISTANCEERR
!                    END IF
!                END DO
!            ENDDO
!        ENDDO
!    END SELECT


999 WRITE(*,*) "DONE"
    END SUBROUTINE

