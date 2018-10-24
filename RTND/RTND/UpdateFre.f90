
subroutine Update_LineFre(NewFleet)
use ConstPara
use DEFINE
use MyLineClass
implicit none 


integer,dimension(NLINE),INTENT(IN):: NewFleet ! updated fleet size
integer l 

do l = 1, Nline
   call get_line_fre(mylines(l))
   fre(l) = mylines(l)%fre
enddo 


! subroutine UpdateSectionCost(fre,tsl,scost,svar,fare)
 !   subroutine get_line_fre(this)




end subroutine 