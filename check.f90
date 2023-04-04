
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

function ep_har(n, year, mon, day) result (dn)

    

    integer:: i, j
    integer, parameter:: mondays_run(12) = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31], &
                      & mondays_ping(12) = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    integer, intent(in):: n
    integer, intent(in):: year(n), mon(n), day(n)
    integer:: dn(n)

    do i = 1, n
      if ( mod(year(i), 400) == 0 .or. ((mod(year(i), 100) /= 0 .and. mod(year(i), 4) == 0)) ) then
        dn(i) = acc_mon_days(mon(i), mondays_run) + day(i)
      else
        dn(i) = acc_mon_days(mon(i), mondays_ping) + day(i)
      end if
    end do

  end function

program check
    interface def
    function ep_har(n, year, mon, day) result (dn)
        integer:: i, j
        integer, parameter:: mondays_run(12) = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31], &
                        & mondays_ping(12) = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        integer, intent(in):: n
        integer, intent(in):: year(n), mon(n), day(n)
        integer:: dn(n)
      end function
    end interface


    integer:: year(12), mon(12), day(12), dn(12), i
    year = 2100
    mon = 12
    day = [(i, i =20, 31)]

    dn = ep_har(12, year, mon, day)
    print *, dn
end program check
