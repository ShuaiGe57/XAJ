module math_doy
  implicit none
  
contains

  function doy(year, month, day) result(res)
      integer, intent(in) :: year(:), month(:), day(:)
      integer :: i, res(size(year))
      integer :: month_length(12) = [31,28,31,30,31,30,31,31,30,31,30,31]

      do i = 1, size(year)
          if (mod(year(i), 4) == 0 .and. (mod(year(i), 100) /= 0 .or. mod(year(i), 400) == 0)) then
              month_length(2) = 29
          else
              month_length(2) = 28
          end if
          res(i) = sum(month_length(1:month(i)-1)) + day(i)
      end do
  end function doy
  
end module math_doy
