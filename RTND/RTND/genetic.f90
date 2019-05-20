    ! contains the genetic functions
          
    subroutine projection(x1,x0,fx0,alph,nwk)
    use constpara
    use GraphLib
    implicit none
    real*8,intent(out)::x1(nl,ndest)
    real*8,intent(in)::x0(nl,ndest)
    real*8,intent(in)::fx0(nl,ndest)
    type(graphclass),intent(in)::nwk
    integer,parameter::max_out_links=10
    integer::i,n,j,nr,node
    real*8::alph
    real*8::proj_vector(max_out_links),proj_result(max_out_links)
    real*8::const=1.0
    ! find max out links
    piter=piter+1
    x1=0.0
    if (isusebcm) then 
        goto 10
    end if 
    do nr=1,ndest
        do i=1,nn
            if (nwk%torder(i,nr)/=0.and.nwk%torder(i,nr)/=nwk%roots(nr)) then
                node=nwk%torder(i,nr)
                proj_vector=0.0
                proj_result=0.0
                n=0
                do j=nwk%firstout(node),nwk%lastout(node)
                    if (nwk%sublink(j,nr)) then
                        n = n + 1
                        proj_vector(n)=x0(j,nr)-alph*fx0(j,nr)
                    end if
                enddo
                call simplex_projection(proj_result,proj_vector,const,n)
                n = 0
                do j=nwk%firstout(node),nwk%lastout(node)
                    if (nwk%sublink(j,nr)) then
                        n=n+1
                        x1(j,nr)=proj_result(n)
                    end if
                end do
            end if
        enddo
    end do
10  do nr=1,ndest
        do i=1,nn
            if (nwk%torder(i,nr)/=0.and.nwk%torder(i,nr)/=nwk%roots(nr)) then
                node=nwk%torder(i,nr)
                proj_vector=0.0
                proj_result=0.0
            n=0
            do j=nwk%firstout(node),nwk%lastout(node)
                if (fx0(j,nr).lt.large) then
                    if (nwk%sublink(j,nr)) then
                        n = n + 1
                        proj_vector(n)=x0(j,nr)-alph*fx0(j,nr)
                    end if
                endif 
            enddo
            call simplex_projection(proj_result,proj_vector,const,n)
            n = 0
            do j=nwk%firstout(node),nwk%lastout(node)
                if (fx0(j,nr).lt.large) then 
                    if (nwk%sublink(j,nr)) then
                        n=n+1
                        x1(j,nr)=proj_result(n)
                    end if
                endif
            end do
        end if
    enddo
end do

    return
    end subroutine



    subroutine msa(x0,y0,x1,stepsize,n1,n2) ! two dimension msa
    implicit none
! x0 is input and also output
    integer,intent(in)::n1,n2! two dimension
    real*8,intent(in)::y0(n1,n2)
    real*8,intent(in)::x0(n1,n2)
    real*8,intent(out)::x1(n1,n2)
    real*8,intent(in)::stepsize
    integer:: i,j

    x1=0.0
    do i=1,n2
      do j=1,n1
        x1(j,i)=x0(j,i)+stepsize*(y0(j,i)-x0(j,i))
      enddo 
    enddo 
    return 
    end subroutine