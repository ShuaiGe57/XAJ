program main
  use math_iday, only: iday
  integer:: year(12), mon(12), day(12), dn(12), i
  year = 2100
  mon = 12
  day = [(i, i =20, 31)]

  dn = iday(12, year, mon, day)
  print *, dn
end program main
