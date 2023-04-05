module pet
  use math_iday, only: iday
    implicit none
    
contains
    
  function ep_har(n, year, mon, day) result (ep)
    integer:: i, j, dn(n)
    integer, intent(in):: n
    integer, intent(in):: year(n), mon(n), day(n)
    real:: ep(n)
    ep = iday()


  end function


end module pet