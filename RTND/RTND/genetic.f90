    ! contains the genetic functions
          
    subroutine projection(x1,x0,fx0,alph,nwk,d1,d2)
    use constpara
    use GraphLib
    implicit none
    integer,intent(in)::d1,d2
    real*8,dimension(d1,d2),INTENT(OUT)::x1
    ! real*8,intent(out)::x1(nl,ndest)
    ! real*8,intent(in)::x0(nl,ndest)
    real*8,DIMENSION(d1,d2),INTENT(IN)::x0
    ! real*8,intent(in)::fx0(nl,ndest)
    real*8,DIMENSION(d1,d2),INTENT(IN)::fx0
    type(graphclass),intent(in)::nwk
    integer,parameter::max_out_links=20
    integer::i,n,j,nr,node
    real*8::alph
    real*8::proj_vector(max_out_links),proj_result(max_out_links)
    real*8::const=1.0

    ! find max out links
    piter=piter+1
    x1=0.0
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


   function max_dist_err_2(x,y,flow,flow_eps,d1,d2) result(max_abs_gap)
   implicit none 
    
    integer::d1,d2
    integer::i,j
    real*8::max_abs_gap
    real*8,dimension(d1,d2),intent(in)::x
    real*8,dimension(d1,d2),intent(in)::y
    real*8,dimension(d1,d2),intent(in)::flow  ! as a reference
    real*8::flow_eps


    max_abs_gap = 0
    
    do i=1, d1
        do j = 1, d2
            if (flow(i,j).gt.flow_eps) then
                if (isnan(x(i,j)).or.isnan(y(i,j))) then 
                      write(*,*) " wtf,nan"
                      write(*,*) x(i,j),y(i,j)
                endif
                
                max_abs_gap = max(max_abs_gap,abs(x(i,j)-y(i,j)))
            end if
        end do 
    end do 


    return

    end function



    subroutine sort(labels,maporder,dim)
    use constpara
    implicit none 
    integer,intent(in):: dim  ! dimension 
    real*8,intent(in)::labels(dim)
    integer,intent(out)::maporder(dim)
    real*8::vec(dim)
    real*8::temp
	integer::i,j,ti
    do i = 1, dim
        maporder(i) = i
    enddo 

	vec(:)=labels(:)
    do i = 1, dim
        do j = dim, i+1, -1
            if (vec(j-1).le.vec(j)) then 
				temp = vec(j-1)
				vec(j-1) = vec(j)
                vec(j) = temp
                ti = maporder(j-1)
                ! ti =  this%torder(j-1,nr)
                maporder(j-1) = maporder(j)
                ! this%torder(j-1,nr) = this%torder(j,nr)
                maporder(j) = ti
				! this%torder(j,nr) = ti
            end if 
        end do 
    end do 
    do i = 1,dim
        if (vec(i).ge.large) then
            ! this%torder(i,nr) = 0
            maporder(i) = 0
        end if
    end do  
    return

    end subroutine

