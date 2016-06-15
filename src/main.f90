!File=main.f90
!Author=
!Created=Fri 02 Oct 2015 14:31:18 CEST
!Last Modified=Fri 02 Oct 2015 14:31:18 CEST

Program main
  use constants
  use mutil
  use mgenlat
  Implicit None
  namelist /input/xlay,ylay,zlay,slay,rlay,tmp,mcstp,latpar,atnm,lat
  open(unit=8,file="kmc.in")
    read(8,nml=input)
  close(8)
  ! Generate Lattice
  call genlat(xlay,ylay,zlay,slay,rlay,tmp,mcstp,latpar,atnm,lat)
  ! Run actual KMC
  !call kmc(tmp,mcstp,lc,de)
End Program main
