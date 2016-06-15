program test_ranq1

  implicit none
  ! only to have integer random number in a given range
  integer,parameter ::liml=0, limr=2
  integer i,cou,cou2
  real ::num
  cou=0;cou2=0
  do i = 1, 1000000
    ! for random number between 0 to 1
    num= ranq1()
    ! for integer in given range
    !      num= mod(int(ranq1()*10),(limr-liml))+liml
    if (num<=.5)cou=cou+1
    if (num>.5)cou2=cou2+1
  end do
  write(*,*) cou,cou2, cou+cou2
contains

  !This code is based on Ranq1
  !pp 351 NR 3rd edition
  !       &
  !Steven G. Kargl clf
  function ranq1()
    implicit none
    integer, parameter ::long=selected_int_kind(18), dp = kind(1.d0)
    integer(long), save ::  x= 1234567890987654321_long,  &
    &                       y= 362436362436362436_long,   &
    &                       z= 1066149217761810_long,     &
    &                       w=123456123456123456_long
    integer:: t
    real(dp):: ranq1
    t=ieor(x, ishft(x,11))
    x = y
    y = z
    z = w
    w = ieor(ieor(w, ishft(w,-19)), ieor(t,ishft(t,-8)))
    !      write(*,*) "W=",w
    ranq1 = transfer(iand(w,z'7fffffffffff000'), ranq1)
    ranq1 = 2 * fraction(ranq1) - 1
  end function ranq1
end program test_ranq1


  function kiss64()
    integer(i8b), save :: x, y, z, c
    integer(i8b) :: t, k, m, s, kiss64
    data x, y, z, c &
         / 1234567890987654321_i8b, &
           362436362436362436_i8b, &
           1066149217761810_i8b, &
           123456123456123456_i8b /
    m(x,k) = ieor(x, ishft(x,k))  ! statement function
    s(x) = ishft(x, -63)          ! statement function
    t = ishft(x, 58) + c
    if (s(x) .eq. s(t)) then
       c = ishft(x, -6) + s(x)
    else
       c = ishft(x, -6) + 1 - s(x + t)
    endif
    x = t + x
    y = m(m(m(y,13_i8b),-17_i8b), 43_i8b)
    z = 6906969069_i8b * z + 1234567
    kiss64 = x + y + z
  end function kiss64

