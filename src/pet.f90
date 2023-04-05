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