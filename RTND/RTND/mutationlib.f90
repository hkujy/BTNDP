!mutsation lib for the fleet 
    
    module mutelib
    use constpara
    contains 
   
    !TODO : write other type of mutation operators

    subroutine mutation_main(now,nei)
        implicit none
        integer,intent(in)::now(nline)
        integer,intent(out)::nei(nline)
        

        call mute_increa_by1(now,nei)


    end subroutine

    subroutine mute_swap(now,nei)
        implicit none
        integer,intent(in)::now(nline)
        integer,intent(out)::nei(nline)
        !todo: swap the two lines 

        
    end subroutine

    subroutine mute_incre_decre(now,nei,num)
        implicit none 
        integer,intent(in)::now(nline)
        integer,intent(out)::nei(nline)
        integer,intent(in)::num
        ! incrase and decrease by a fixed number
        !TODO: To write the mutation main 

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
    
    
    end module