
    module ConstPara
    implicit none
    real*8,parameter::MaxFre = 17
    real*8,parameter::MinFre = 4
    integer,parameter::TotalFleet = 15
    integer,parameter::LogFileNo = 99
    INTEGER,PARAMETER::INPUTSEED = 1
    INTEGER,PARAMETER::SUBMAX = 0
    INTEGER,PARAMETER::NDEST=1  !NUMBER OF DISTINATIONS
    INTEGER,PARAMETER::NLINE=4	! NUMBER OF LINE
    INTEGER,PARAMETER::NOD=2	! NUMBER OF OD PAIRS
    INTEGER,PARAMETER::NN=4   ! NUMBER OF NODES
    INTEGER,PARAMETER::NL=6	!NUMBER OF LINKS
    REAL*8,PARAMETER::WalkCost = 0.01
    INTEGER,PARAMETER::MaxCom = 10
    INTEGER,PARAMETER::MaxSecLine=4
    INTEGER,PARAMETER::MaxLineStops = 4
    REAL*8,PARAMETER::gama=60.0D0
    INTEGER,PARAMETER::MAXITERATION=500
    REAL*8,PARAMETER::MAXCT = 1000
    INTEGER,PARAMETER::MACSOLC = 10000
    REAL*8,PARAMETER::MAXCPU = 3600 ! 10MINS
    REAL*8,PARAMETER::DELFLOWEPS = 1D-5
    !Constant Paramters
    REAL*8, PARAMETER:: LARGE = 1.0D20
    REAL*8, PARAMETER:: INF1=10D15
    REAL*8, PARAMETER::NCP_EPS=1D-6
    REAL*8, PARAMETER::FLOW_EPS=0.001D0
    END MODULE


    MODULE DEFINE
    use ConstPara
    use MyLineClass
    use MyLinkClass
    IMPLICIT NONE

    !NetworkParameters
    !INTEGER::CONGESTION_N=2
    real*8::CONGESTION_N
    REAL*8::BS(NL)
    REAL*8::Capk
    REAL*8::RIO  ! VARIANCE IS NOT CONSIDERED
    INTEGER SOLC,SUBCOUNT  ! SOLUTION COUNT
    LOGICAL UECONVERGE
    LOGICAL SUBCONVERGE
    LOGICAL SAM
    LOGICAL ISCONVERGE

    !Algorithm Parameters
    REAL*8 TIME_BEGIN,TIME_END
    LOGICAL::CONNECT(NN,NN,NDEST)
    INTEGER::NUMCONNECT(NN,NN,NDEST)

    !Network
    INTEGER::DEST(NOD),ORIGIN(NOD)
    !	INTEGER::COMPETE(NL,MaxCom)		!compete section index
    INTEGER::LocateCompete(NL+1)
    INTEGER::CompeteSec(10)

    INTEGER::NumCom(NL)
    REAL*8::fare(NL)
    REAL*8::DEMAND(NOD)
    INTEGER::LineStops(NLINE)
    INTEGER::LINE(NLINE,MaxLineStops)   ! bus lines


    INTEGER PA(NN)
    !Flow Related
    !	REAL*8::XFA(NL,NDEST) ! Variable:: v_sd
    REAL*8::SLFLOW(NL,NLINE) ! SECTIONLINE FLOW
    REAL*8::X(NL,NDEST) ! PERCENTAGE
    REAL*8::NF(NN,NDEST)  ! NODE FLOW OF OD PAIR
    REAL*8::CLF(NL)  ! CURRENT LINK FLOW
    !cost related
    REAL*8::FRE(NLINE)
    real*8::SF(NL)       ! section frequency sum
    REAL*8::SLF(NL,NLINE)   ! SECTION FREQUENCY SUM: SECTION LINE FREQUENCY
    REAL*8::FX(NL,NDEST)	! NDEST IS THE NUMBER OF DESTINATIONS
    REAL*8::SCOST(NL),SVAR(NL) ! section cost and section variance
    INTEGER::ANODE(NL),BNODE(NL)
    REAL*8::STT(NL)  ! SECTION TRAVEL TIME
    REAL*8 DIST(NN) ! NODE LABER FOR SHORTEST COST
    REAL*8::NDIST(NN,NDEST)		! the shortest distance from node all
    REAL*8::LNDIST(NN,NDEST)		! the LONGEST distance from node all
    !		PREDECESSOR ARC
    INTEGER SINDEX(NL)! relate section and link index
    INTEGER::SL(NL,MaxSecLine)
    INTEGER::SLC(NL)
    INTEGER::CASEINDEX



    ! new part added in 2018 for the caspt paper
    type(LineClass),dimension(NLINE):: MyLines
    type(LinkClass),dimension(NL)::MyLinks
    real*8:: tsl(8,4) ! cost, var, fare, fre





    END MODULE
    ! this file is to define the variables relate to graph algorithms
















    MODULE GRAPH
    USE DEFINE
    IMPLICIT NONE
    INTEGER::ROOTS(NDEST)
    INTEGER::TORDER(NN,NDEST) ! TOPOLOY OREDER
    INTEGER::PITER
    LOGICAL::SubLink(NL,NDEST),SubNode(NN,NDEST)
    INTEGER::FIRSTIN(NN),LASTIN(NN)
    INTEGER::BACKANODE(NL),BACKBNODE(NL)
    INTEGER::BACKTOFORWARD(NL)






    END MODULE

