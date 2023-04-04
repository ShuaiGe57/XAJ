module evaporation
  use parameter
  implicit none
  

  public :: e_fun
contains
  subroutine e_sub(n, pars, p, e)
    implicit none
    integer, intent(in):: n
    integer:: i, j, k
    type(par), intent(in):: pars
    real, intent(in):: p(n)
    real, intent(out):: e(n)
    do i = 1, n
      if ( p(i) + pars )
    end do

  end subroutine e_sub

!---------------------------------------------------

end module evaporation
