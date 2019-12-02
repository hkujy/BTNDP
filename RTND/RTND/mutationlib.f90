!mutsation lib for the fleet 
    
    module mutelib
    use constpara
    contains 
   
    !TODO : write other type of mutation operators

    subroutine mutation_main(now,nei)
        implicit none
        integer,intent(in)::now(nline)
        integer,intent(out)::nei(nline)
        integer::index
        real*8::ran
        call random_number(ran)
        index = int(4*ran + 1)
        write(*,*) "mutation index = ", index 
        select case (index)
        case (1)
            call mute_increa_by1(now,nei)
        case (2)
            call mute_swap(now,nei)
        case (3)
            call mute_incre_decre(now,nei,2)
        case (4)
            call mute_incre_decre(now,nei,3)
        end select
    end subroutine

    subroutine select_two_dif_line(L1,L2)
        implicit none
        integer,INTENT(OUT)::L1,L2
        real*8::ran
        integer::count
        count = 0 
    10  call random_number(ran)
        L1 = int(nline*ran+1)

    15  call random_number(ran)
        L2 = int(nline*ran + 1)
        if (L1.eq.L2) then 
            count = count + 1
            if (count.ge.100) then 
                write(*,*) "can not find a line to increase"
            end if 
            goto 15
        end if 
    end subroutine

    subroutine mute_swap(now,nei)
        implicit none
        integer,intent(in)::now(nline)
        integer,intent(out)::nei(nline)
        integer::L1,L2
        !todo: swap the two lines 

        nei =  now
        call select_two_dif_line(L1,L2)
        nei(L1) = now(L2) 
        nei(L2) = now(L1)

    end subroutine

    subroutine mute_incre_decre(now,nei,num)
        implicit none 
        integer,intent(in)::now(nline)
        integer,intent(out)::nei(nline)
        integer,intent(in)::num
        integer::L1,L2
        nei =  now
        call select_two_dif_line(L1,L2)
        nei(L1) = max(nei(L1) - num,fleet_lb(L1))
        nei(L2) = min(nei(L2) + num,fleet_ub(L2))
    end subroutine

    subroutine mute_increa_by1(now,nei)
        ! increase one line and reduce other line
    implicit none 
    integer,intent(in)::now(nline)
    integer,intent(out)::nei(nline)
    integer::count
    real*8::ran
    integer::rl,il    !reduce and increase line

    nei =  now
    count = 0 
10  call random_number(ran)
    rl = int(nline*ran+1)
    if (nei(rl)-1.lt.fleet_lb(rl)) then 
        count = count + 1
        if (count.ge.100) then 
            write(*,*) " can not find a line to remove"
        end if 
        goto 10
    end if 
    count = 0
15  call random_number(ran)
    il = int(nline*ran + 1)
    if ((il.eq.rl).or.(nei(il)+1.gt.fleet_ub(il))) then 
        count = count + 1
        if (count.ge.100) then 
            write(*,*) "can not find a line to increase"
        end if 
        goto 15
    end if 

    nei(rl) = nei(rl) - 1
    nei(il) = nei(il) + 1

    end subroutine

   
    subroutine roulette(fits,fitsize,list,listsize) 
    implicit none 
    
    integer,intent(in)::fitsize
    real*8,intent(in)::fits(fitsize)
    integer::id
    real*8::ts
    real*8::prob(fitsize)
    real*8::ran
    integer::p,j
    integer::listsize
    integer::list(listsize)
    real*8::cum_sum
    ts = sum(fits)
    prob(:)=fits(:)/ts
    cum_sum  = 0
    do p=1, fitsize
        prob(p) = cum_sum + prob(p)
        cum_sum = prob(p)
    end do 

    list(:) = -1
    do j = 1, listsize
        id = -1
        call random_number(ran)
        do p = 1, fitsize
            if (ran.le.prob(p)) then 
                id =  p
            exit
            endif 
        enddo
        if(id.lt.0) then
            write(*,*)  "roulette err: cannot find valid id"
            pause
        else
            list(j) = id
        end if
    enddo
    end subroutine
    
    subroutine remedy(now)
    implicit none 
    integer,intent(inout)::now(nline)
    integer::l
    integer::add_sum, reduce_sum
    logical::isRemedy
    isRemedy = .false.
    
    do l=1, nline
        if ((now(l).lt.fleet_lb(l)).or.(now(l).gt.fleet_ub(l))) then 
            isRemedy = .true. 
            exit
        endif
    enddo 
    if (.not.isRemedy) then
        return 
    endif
    add_sum = 0
    reduce_sum = 0
    do l = 1, nline
        do while(now(l).lt.fleet_lb(l))
            now(l)= now(l) + 1
            add_sum = add_sum + 1 
        end do
        do while(now(l).gt.fleet_ub(l))
            now(l) = now(l) - 1
            reduce_sum = reduce_sum + 1
        end do
    end do 
    end subroutine   
    


    end module