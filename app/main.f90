program main
  use math_doy, only: doy
  implicit none
  integer, parameter:: n = 12
  real, parameter:: lat = 30.
  integer:: year(n), mon(n), day(n), dn(n), i
  real:: tmin(n), tmax(n)
  real:: ep(n)
  year = 2100
  mon = 12
  day = [(i, i =1, n)]
  tmin = [(15+i*0.2, i = 1, n)]
  tmax = tmin * 1.1

  dn = doy(year, mon, day)
  print *, dn
end program main
