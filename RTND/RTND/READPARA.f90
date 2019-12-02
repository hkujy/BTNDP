    !TODO: the function read and write default parameters for the program
    subroutine readpara
    use constpara
    implicit none
    integer::i, i_val 
    real*8::value

    select case(networktype)
    case(-2)
        OPEN(1,file='c:/GitCodes/OpenTransportData/STOCH/networkpara.txt')
        open(2,file='c:/GitCodes/OpenTransportData/STOCH/Para.txt')
        open(3,file='c:/GitCodes/OpenTransportData/STOCH/BsValue.txt')
        write(*,*) "Network:           STOCH"
    case(-1)
        open(1,file='C:\GitCodes\OpenTransportData\cycle\networkpara.txt')
        open(2,file='C:\GitCodes\OpenTransportData\cycle\para.txt')
        open(3,file='C:\GitCodes\OpenTransportData\cycle\bsvalue.txt')
        write(*,*) "Network:           cycle"
    case(0)
        open(1,file='c:\gitcodes\BTNDP\input\testnetwork\networkpara.txt')
        open(2,file='c:\gitcodes\BTNDP\input\testnetwork\para.txt')
        open(3,file='c:\gitcodes\BTNDP\input\testnetwork\bsvalue.txt')
        write(*,*) "Network:           Four nodes Net in TestNet"
    case(1)
        open(1,file='C:\GitCodes\OpenTransportData\SiouxFallNet\Transit_Toy\networkpara.txt')
        open(2,file='c:\gitcodes\OpenTransportData\SiouxFallNet\Transit_Toy\para.txt')
        open(3,file='c:\gitcodes\OpenTransportData\SiouxFallNet\Transit_Toy\bsvalue.txt')
        write(*,*) "Network:           SiouxFall Transit Toy"
    case(2)
        open(1,file='C:\GitCodes\OpenTransportData\SiouxFallNet\Transit_AllOD\networkpara.txt')
        open(2,file='c:\gitcodes\OpenTransportData\SiouxFallNet\Transit_AllOD\para.txt')
        open(3,file='c:\gitcodes\OpenTransportData\SiouxFallNet\Transit_AllOD\bsvalue.txt')
        write(*,*) "Network:           SiouxFall Transit All OD"
 
    case(3)
        open(1,file='C:\GitCodes\OpenTransportData\SiouxFallNet\Transport_Toy\networkpara.txt')
        open(2,file='c:\gitcodes\OpenTransportData\SiouxFallNet\Transport_Toy\para.txt')
        open(3,file='c:\gitcodes\OpenTransportData\SiouxFallNet\Transport_Toy\bsvalue.txt')
        write(*,*) "Network:           SiouxFall Transport Toy"
    case(4)
        open(1,file='C:\GitCodes\OpenTransportData\SiouxFallNet\Transport_AllOD\networkpara.txt')
        open(2,file='c:\gitcodes\OpenTransportData\SiouxFallNet\Transport_AllOD\para.txt')
        open(3,file='c:\gitcodes\OpenTransportData\SiouxFallNet\Transport_AllOD\bsvalue.txt')
        write(*,*) "Network:           SiouxFall Transport All OD"
    end select

    do i=1, 5
        read(1,*) i_val
        select case (i)
        case (1)
            ndest = i_val
        case (2)
            nod= i_val
        case (3)
            nn = i_val
        case (4)
            nl = i_val
        case (5)
            nline = i_val
        end select
    end do 
   
    
    ! I think these are global parameters
    allocate(dist(nn)) ! node laber for shortest cost
    allocate(lndist(nn,ndest))	! the longest distance from node all
    allocate(fleet_lb(nline), fleet_ub(nline)) ! lower and upper bound of fleet
    allocate(fre_lb(nline), fre_ub(nline)) ! lower and upper bound of frequency 
    allocate(bs(nl))

    do i = 1,4
        read(2,*) value 
        select case(i)
        case(1)
            congestion_n = IDINT(value)
        case(2)
            write(logfileno,*) "read_pare: use the same bs value for all"
        case(3)
            capk = value
        case(4)
           rio = value
        end select
    enddo
    do i=1, nl
        read (3,*) bs(i)
    enddo
  
    close(1)
    close(2)
    close(3)
    call writepara
    end subroutine
