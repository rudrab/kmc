!This is file : src/util
! Author= Rudra Banerjee
! Started at: 25.07.13
!> Utility
Module mutil
  use constants
  use iso_fortran_env
  !use ieee_arithmetic
  implicit none
  public ::error
  private :: error_real, error_int


  interface error
    !interface for generic subroutine error
    module procedure error_real, error_int, error_long
    end interface error

  contains

    subroutine error_long(rval,rmatch,e_string,prognm)
      !Routine error for long_real
      Implicit None
      real(real128),intent(in):: rval,rmatch
      character(120),intent(in) ::e_string, prognm
      if (abs(rval-rmatch)>= .005) then
        write(*,"(f10.5,'is not equal to',f10.5)")rval,rmatch
        write(*,"('Error:', a,' in',a)")trim(e_string),trim(prognm)
        call fstop(prognm)
      end if
    end subroutine error_long

    subroutine error_real(rval,rmatch,e_string,prognm)
      !Routine error for real
      Implicit None
      real(real32),intent(in):: rval,rmatch
      character(len=*),intent(in) ::e_string, prognm
      if (abs(rval-rmatch)>= .005) then
        write(*,"(f10.5,'is not equal to',f10.5)")rval,rmatch
        write(*,"('Error:', a,' in',a)")trim(e_string),trim(prognm)
        call fstop(prognm)
      end if
    end subroutine error_real

    subroutine error_int(ival,imatch,e_string,prognm)
      !Routine error for integer
      Implicit None
      integer,intent(in):: ival,imatch
      character(len=*),intent(in) ::e_string, prognm
      if (ival/=imatch) then
        write(*,"(i5,' is not equal to ',i5)")ival,imatch
        write(*,"('Error: ', a,' in ',a)")trim(e_string),trim(prognm)
        call fstop(prognm)
      end if
    end subroutine error_int

    subroutine fstop(sname,errstr)
      !Routine to stop code for error
      implicit none
      character(*),intent(in)::sname
      character(*),optional, intent(in)::errstr
      write(6,'("*  STOP CALLED BY ROUTINE " ,a,"*")') trim(sname)
      if (present(errstr))  write(*,*)errstr
      stop
    end subroutine fstop

    subroutine get_env()
      !get the system environment
      Implicit None
      character(80):: hostname, pwd,sepe(60)="="
      integer::starttime(8)
      write(*,"(60a1)")sepe(1:60)
      call get_environment_variable("HOSTNAME",hostname)
      call get_environment_variable("PWD",pwd)
      write(*,"('Job submitted by',t20,':', t25,a,2x)")trim(hostname)
      write(*,"('Running From',t20,':',t25,a)") trim(pwd)
      call date_and_time(values=starttime)
      write(*,"('Started running on',t20,':',t25,2(i2,'-'),(i4),t37,'at',t40,i2,':',i2, 2x,'hour')") &
        starttime(3),starttime(2),starttime(1),starttime(5 : 6)
      write(*,"(60a1)")sepe(1:60)
    end subroutine

    logical  function is_numeric(string)
      !use ieee_arithmetic
      implicit none
      character(len=*), intent(in) :: string
      !    logical :: is_numeric
      real :: x
      integer :: e
      !x = FOR_S_NAN
      read(string,*,iostat=e) x
      is_numeric = ((e == 0) .and. (x == x))
      !    is_numeric = ((e == 0) .and. (.not. ieee_is_nan(x)))
    end function is_numeric

  End module mutil
