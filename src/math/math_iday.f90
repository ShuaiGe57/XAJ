module math_iday
    implicit none
    private
    public:: iday
contains
function iday(n, year, mon, day)
    integer:: i
    integer, parameter:: mondays_run(12) = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31], &
                      & mondays_ping(12) = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    integer, intent(in):: n
    integer, intent(in):: year(n), mon(n), day(n)
    real:: iday(n)

    do i = 1, n
      if ( mod(year(i), 400) == 0 .or. ((mod(year(i), 100) /= 0 .and. mod(year(i), 4) == 0)) ) then
        iday(i) = acc_mon_days(mon(i), mondays_run) + day(i)
      else
        iday(i) = acc_mon_days(mon(i), mondays_ping) + day(i)
      end if
    end do

  end function

!------------------月天数累计-----------------------
  function acc_mon_days(mon, mondays) result(days)
    implicit none 
    integer, intent(in):: mon, mondays(12)
    integer:: days, i
    do i = 1, mon
      if ( i == 1 ) then
        days = 0
      else
        days = mondays(i-1) + days
      end if
    end do
  end function

end module math_iday