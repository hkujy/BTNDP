! STOCH algorithm following sheffi book 
! input a network strcture 
! output loading and cost

module STOCHLoading
use GraphLib
use constpara
implicit none

type,public::DialLoad
    type(graphclass)::nwk
    real*8,ALLOCATABLE::lt(:),lf(:)  ! link flow and link time
    real*8,ALLOCATABLE::prob(:,:)   ! prob
    real*8,ALLOCATABLE::linklike(:,:)
    real*8,ALLOCATABLE::rlabel(:,:),slabel(:,:)
    integer,ALLOCATABLE::rorder(:,:),sorder(:,:) ! ascend order
    real*8,ALLOCATABLE::Wsd(:,:)
    real*8::theta
contains 
    procedure,pass::ini=>ini
    procedure,pass::del=>del
    procedure,pass::loadmain=>loadmain
    procedure,pass::get_sub_graph=>get_sub_graph
    procedure,pass::get_link_like=>get_link_like
    procedure,pass::forward=>forward
    procedure,pass::backward=>backward
end type DialLoad

contains

subroutine ini(this)
  use constpara
  implicit none 
  class(dialload)::this
  if (.not.allocated(this%lt) then
    allocate(this%lt(nl),this%lf(nl))
    allocate(this%linklike(nl,ndest))
    allocate(this%rlabel(nn,ndest),this%slabel(nn,ndest))
    allocate(this%rorder(nn,ndest),this%sorder(nn,ndest))
    allocate(this%Wsd(nl,ndest),this%prob(nl,ndest))
  endif
  this%wsd = 0
  this%rorder = 0
  this%sorder = 0
  this%linklike = 0
  this%rlabel = -1
  this%slabel = -1
  this%lt = 0
  this%lf = 0
  this%prob = 0
  call this%nwk%inigraph
end subroutine

subroutine del(this)
implicit none 
class(dialload)::this

call this%nwk%delgraph
deallocate(this%lt,this%lf,this%prob,this%linklike)
deallocate(this%slabel,this%rlabel,this%rorder,this%sorder)

end subroutine



subroutine loadmain(this)
implicit none 
CLASS(DialLoad)::this

this%theta = 1.0

call this%nwk%readnwt
this%lt= this%nwk%scost
call this%get_sub_graph
call this%get_link_like
call this%backward
call this%forward


end subroutine


subroutine get_sub_graph(this)
use constpara
implicit none 

integer::r,s,w,i,nr,l,j
class(dialload)::this
this%nwk%sublink =.false.
this%nwk%subnode =.false.
do w = 1,nod
    r = this%nwk%origin(w)
    s = this%nwk%dest(w)
    do i = 1, ndest
        if (s.eq.this%nwk%roots(i)) then 
            nr = i
            exit  
        end if 
    enddo 
    ! from r to all nodes 
    call sp(r,this%lt,this%nwk%firstout,this%nwk%lastout,this%nwk%pa(:,nr),this%nwk%bnode)
    this%rlabel(:,nr) = dist(:)
    ! from s to all nodes
    call rsp(s,this%nwk%scost,this%nwk%firstin,this%nwk%lastin,this%nwk%pa(:,nr),this%nwk%backbnode,this%nwk%backtoforward)
    this%slabel(:,nr) = dist(:)

    do l = 1,nl
        i = this%nwk%anode(l)
        j = this%nwk%bnode(l)
        if ((this%rlabel(i,r).lt.this%rlabel(j,nr)).and.(this%slabel(i,nr).gt.this%slabel(j,nr))) then 
            this%nwk%sublink(l,nr) = .true.  
            this%nwk%subnode(i,nr) = .true.
            this%nwk%subnode(j,nr) = .true.
        end if
    end do 
enddo 

end subroutine 


! compute the link likelyhood
subroutine get_link_like(this)
implicit none 

integer::l,nr,i,j
CLASS(DialLoad)::this

do nr = 1, ndest
    do l = 1, nl
        i = this%nwk%anode(l)
        j = this%nwk%bnode(l)
        if (this%nwk%sublink(l,nr)) then 
            this%linklike(l,nr) = exp(this%theta*(this%rlabel(j,nr)-this%rlabel(i,nr)-this%lt(l)))
            write(*,*) "i = ",i," j = ",j," val = ",this%rlabel(j,nr)-this%rlabel(i,nr)-this%lt(l),&
                        " L = ", this%linklike(l,nr)
        else
            this%linklike(l,nr) = 0
        endif
    end do 
end do 

end subroutine

subroutine backward(this)
implicit none 
integer::nr,i,now,l,ol,link
class(DialLoad)::this
real*8::sumwsd

do nr = 1, ndest
    this%Wsd(:,nr) = 0
    call sort(-1*this%slabel(:,nr),this%sorder(:,nr),nn)
    do i = 1, nn
        now = this%sorder(i,nr)
        do link = this%nwk%firstin(now),this%nwk%lastin(now)
            l=this%nwk%backtoforward(link)
           if (now.eq.this%nwk%roots(nr)) then 
                this%Wsd(l,nr) = 1 
           else
                sumwsd = 0
                do ol = this%nwk%firstout(now), this%nwk%lastout(now)
                    sumwsd = sumwsd + this%Wsd(ol,nr)
                enddo 
                this%Wsd(l,nr) = this%linklike(l,nr)*sumwsd
           endif 
           write(*,*) "i = ",this%nwk%anode(l), " j =",this%nwk%bnode(l), " val=",this%Wsd(l,nr)
        enddo
    enddo 
end do

end subroutine


subroutine forward(this)
implicit none

class(dialload)::this
integer::nr,i,now,l,a,b
real*8::sumwsd


do nr = 1, ndest
    call sort(-1*this%rlabel(:,nr),this%rorder(:,nr),nn)
    do i = 1, nn
        now = this%rorder(i,nr)
        sumwsd = 0
        do l = this%nwk%firstout(now), this%nwk%lastout(now)
            sumwsd = this%Wsd(l,nr) + sumwsd
        enddo
        do l = this%nwk%firstout(now), this%nwk%lastout(now)
            if (sumwsd .eq.0) then
                this%prob(l,nr) = 0 
            else
                this%prob(l,nr) = this%Wsd(l,nr)/sumwsd 
            end if
        enddo 
    enddo
    do l = 1, nl
       a = this%nwk%anode(l) 
       b = this%nwk%bnode(l)
    enddo
end do

end subroutine

end module
