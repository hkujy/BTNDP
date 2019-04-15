! Main code the ABC algorithm 
! Solution representation 
! each gen represents the number of fleet allocated to for a ABC

include "MyClass.F90"
use MyClassModule
use GRAPH
module ABC
implicit none 

contains 


subroutine gen_sol(chrom,lb,ub)
implicit none
type(SolClass), intent(out)::chrom
integer,dimension(NLINE),intent(in):: lb, ub !lower and upper bound, min/max fleetsize computed by via travel cost
real*8::ran   ! generate a random number
integer::remain
integer:: i
write(*,*) "print random_number =", ran

! TotalFleet is the upper bound of all the fleet 

chrom(:) = lb(:)
remain = int(TotalFleet - sum(chrom))
if (sum(chrom) .gt. TotalFleet) then 
    write(*,*) "The lower bound is greater than the total fleet"
    pause
endif 
do i = 1, int(TotalFleet-sum(chrom
call random_number(ran)




end subroutine

subroutine employ_bee()
end subroutine 


subroutine on_looker()

end subroutine 



end module 


