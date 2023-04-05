!====================计算年内日序=======================
! year, mon, day 是长度为n的数组
! iday 是一年内的第i天
!======================================================

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
  integer:: iday(n)

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



module pet
  use math_iday, only: iday
    implicit none
    
contains
  function ep_har(n, lat ,year, mon, day, tmax, tmin) result (ep)
    implicit none
    real, parameter:: pi = 3.1415926, w = 0.2618
    integer:: dn(n)
    integer, intent(in):: n, year(n), mon(n), day(n)
    real, intent(in):: lat, tmax(n), tmin(n) ! 纬度°
    real:: ep(n), delta(n), Tsr(n), E0(n), Ra(n), tav(n)
    dn = iday(n, year, mon, day)
    delta = asin( 0.4*sin( 2*pi/365*(dn-82) ) )
    Tsr = acos( -tan(delta)*tan(lat/180*pi) )
    E0 = 1 + 0.033*cos(2*pi*dn/365)
    Ra = 37.59*E0*( w*Tsr*sin(delta)*sin(lat/180*pi) &
        & + cos(delta)*cos(lat/180*pi)*sin(w*Tsr) )
    tav = (tmin + tmax)/2
    ep = 0.408*0.0023*Ra*(tav + 17.8)*sqrt(tmax - tmin)

  end function


end module pet



program main
  use pet, only: ep_har
  implicit none
  integer, parameter:: n = 12
  real, parameter:: lat = 30.
  integer:: year(n), mon(n), day(n), i
  real:: tmin(n), tmax(n)
  real:: ep(n)
  year = 2100
  mon = 12
  day = [(i, i =1, n)]
  tmin = [(15+i*0.2, i = 1, n)]
  tmax = tmin * 1.1

  ep = ep_har(n, lat, year, mon, day, tmax, tmin)
  print *, ep
end program main
