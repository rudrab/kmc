! This is file : constants.f90
!> Defining Universal constants
module constants
  use iso_fortran_env
  implicit None

  !Fundamental Constants (units are in SI)
  real(real64),parameter :: pi=acos(-1.0_real64)             !> pi
  real(real128),parameter :: c =299792458._real128           !> speed of light
  real(real128),parameter :: h =6.62606957E-34               !> Plank's constant
  real(real128),parameter :: h_cut =h/(2*pi)                 !> Reduced Plank's constant
  real(real128),parameter :: R=8.3144621                     !> Gas Constant     |J/(K*Mol)
  real(real128),parameter :: N_a =6.02214129E23              !> Avogadro's Number| /mol
  real(real128),parameter :: k_B =R/N_a                      !> J/K
  real(real128),parameter :: e=1.602176565E-19               !> C
  real(real128),parameter :: eV=1.602176565E-19              !> J
  real(real128),parameter :: mu_0=4*pi*1E-07                 !> magnetic constant
  real(real128),parameter :: eps_0=1/(mu_0*c*c)              !> electric constant
  real(real128),parameter :: m_e=9.10038291E-31              !> electron mass
  real(real128),parameter :: m_p=1.67262177E-27              !> proton mass
  real(real128),parameter :: Ryd=10973731.568539             !> Rydberg Constant
  real(real128),parameter :: amu= 1.660538921E-27            !> Atomic Mass unit
  real(real128),parameter :: alpha=7.2973525698E-3           !> Fine Structure Constant
  real(real128),parameter :: a_0=alpha/(4*pi*Ryd)            !> Bohr Radius


  !Conversion Table
  real(real128),parameter :: deg2rad=pi/180._real128         !> Degree to Radian
  real(real128),parameter :: ang2bohr=1.889725989_real128    !> Angstrom to Bohr
  real(real128),parameter :: ryd2mev=13.605698066E03_real128 !> Rydberg to meV


end module constants
