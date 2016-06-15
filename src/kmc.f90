! Kinetic Monte Carlo module
! Includes `init_random_seed()` from gfortran manual
! https://gcc.gnu.org/onlinedocs/gfortran/RANDOM_005fSEED.html 
! Created: 26/10/2015
! Last Modified: 12/11/2015

Module  mkmc
  use mgenlat
  Implicit None
  real(real32), parameter::kb=8.6173e-05
  real(real32):: rktot

contains

  Subroutine kmc(tmp, mcstp, lc, de)
    implicit none
    integer:: i,j,k, iatm
    real(real32):: rho,inkbt
    real(real32), dimension(2):: de!(2)
    real(real32), dimension(0:indx):: r!,de
    real(real32), dimension(0:indx):: cumr
    integer::tmp,mcstp,lc

    cumr = 0.0_real32
    inkbt = 1/(kb*tmp)
    !define rate constants
    !For the time being, taking DE random
    r(0:indx/2) = 10e13*exp(-de(1)*inkbt)
    r(indx/2+1:indx) = 10e13*exp(-de(2)*inkbt)

    ! Cumulative sum
    do i = 1, indx
      cumr(i) = cumr(i-1) + r(i)
    end do

    !write(*,*)rho, "rktot=",rktot, r, kb
    !if (any(cumr < rktot)) write(*,*) cumr
    call random_seed()
                  
    do k = 1, mcstp
      !Generate uniform random number
      call random_number(rho)
      rktot = rho*cumr(indx)
      chk: do i = 1, indx
        if (cumr(i)>rktot) then
          iatm = i
          if (i <= indx/2) lc = 0!then
            !lc = 0
          !else
            !lc = 1
          !endif
          exit chk
        End if
      end do chk
      if (pos(iatm)%atn /= atnm(0)) then
        slct:do j = 1+lc*4,4+lc*4
          if (pos(nn(iatm,j))%atn == atnm(0)) then
            !write(*,*)iatm, pos(iatm)%atn,pos(nn(iatm,j))%atn
            pos(nn(iatm,j))%atn = pos(iatm)%atn
            pos(iatm)%atn = atnm(0)
            !write(*,*)iatm, pos(iatm)%atn,pos(nn(iatm,j))%atn
          endif
        end do slct
      end if

      if (mod(k,100) == 0) then
        open(11, file="lattice.dat", access="append")
        write(11,*)indx
        write(11,*)
        do i = 1,indx
          write(11,*) pos(i)%atn, pos(i)%vect
        end do
        close(11)
      endif

    end do

  end subroutine kmc


!  subroutine init_random_seed()
!    use iso_fortran_env, only: int64
!    implicit none
!    integer, allocatable :: seed(:)
!    integer :: i, n, un, istat, dt(8), pid
!    integer(int64) :: t

!    call random_seed(size = n)
!    allocate(seed(n))
!    ! First try if the OS provides a random number generator
!    open(newunit=un, file="/dev/urandom", access="stream", &
!      form="unformatted", action="read", status="old", iostat=istat)
!    if (istat == 0) then
!      read(un) seed
!      close(un)
!    else
!      ! Fallback to XOR:ing the current time and pid. The PID is
!      ! useful in case one launches multiple instances of the same
!      ! program in parallel.
!      call system_clock(t)
!      if (t == 0) then
!        call date_and_time(values=dt)
!        t = (dt(1) - 1970) * 365_int64 * 24 * 60 * 60 * 1000 &
!          + dt(2) * 31_int64 * 24 * 60 * 60 * 1000 &
!          + dt(3) * 24_int64 * 60 * 60 * 1000 &
!          + dt(5) * 60 * 60 * 1000 &
!          + dt(6) * 60 * 1000 + dt(7) * 1000 &
!          + dt(8)
!      end if
!      pid = getpid()
!      t = ieor(t, int(pid, kind(t)))
!      do i = 1, n
!        seed(i) = lcg(t)
!      end do
!    end if
!    call random_seed(put=seed)
!  contains
!    ! This simple PRNG might not be good enough for real work, but is
!    ! sufficient for seeding a better PRNG.
!    function lcg(s)
!      integer :: lcg
!      integer(int64) :: s
!      if (s == 0) then
!        s = 104729
!      else
!        s = mod(s, 4294967296_int64)
!      end if
!      s = mod(s * 279470273_int64, 4294967291_int64)
!      lcg = int(mod(s, int(huge(0), int64)), kind(0))
!    end function lcg

!  end subroutine init_random_seed

end module  mkmc
