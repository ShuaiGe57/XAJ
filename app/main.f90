program main
  use pet, only: ep_har
  implicit none
  integer, parameter:: n = 12
  real, parameter:: lat = 120.
  integer:: year(n), mon(n), day(n), dn(n), i
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
