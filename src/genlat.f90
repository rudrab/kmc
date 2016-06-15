! Generate lattice
module  mgenlat
  use iso_fortran_env
  Implicit None

  type point
    real(real32), dimension(3):: vect
  end type point

  type,extends(point):: atmspec
    character(len = 2):: atn
  end type

  type(atmspec),dimension(:),allocatable :: pos
  integer,dimension(:,:),allocatable::nn!(2500,0:8)
  integer::indx
  integer::xlay,ylay,zlay,slay,rlay,mcstp,tmp,vac(3),lc,lat
  character(len=2),dimension(4)::atnm 
  type(point)::latpar

contains
  
  subroutine  genlat(xlay,ylay,zlay,slay,rlay,tmp,mcstp,latpar,atnm,lat)
    Implicit None
    integer::xlay,ylay,zlay,slay,rlay,mcstp,tmp,lat   !
    character(len=2),dimension(4)::atnm               ! Dummies
    type(point)::latpar                               !
  
    integer::i,j,k
    integer::tot
    integer::mult
    !real(real32)::rr(40)

    tot =  1+(xlay+1)*(ylay+1)*(zlay+1)
    allocate(pos(0:tot))
    !call random_number(rr)
    !rind = floor(rr*tot)

    pos(0)%vect =  (/0.0,0.0,0.0/)
    pos%atn =  atnm(2)

    select case(lat)
    case (221)
      write(*,*)"Creating  BCC Lattice"
      allocate(nn(tot,0:8))
      indx =  1
      do k = 0, zlay
        mult =  mod(k,2)
        do j = 0, ylay
          do i = 0,xlay
            pos(indx)%vect = (/i*latpar%vect(1)+latpar%vect(1)*(mult)/ 2.0, &
              j*latpar%vect(2)+latpar%vect(2)*(mult)/ 2.0, &
              k*latpar%vect(3)/2.0/)
            if (k >  (zlay-slay-rlay))  pos(indx)%atn = atnm(3)
            if (k >  (zlay-slay)) pos(indx)%atn = atnm(4)
            !if (any(rind == indx)) pos(indx)%atn = atnm(0)
            indx = indx + 1
          end do
        end do
      end do

    case default
      write(*,*)"lattice type not implemented"
      stop
    end select

    write(*,*)atnm(3), lat

    open(10, file="lattice.dat")
    write(10,*)indx
    write(10,*)
    do i =  1,indx!xlay*ylay*zlay
      write(10,*)pos(i)%atn, pos(i)%vect
    end do
    close(10)

  end subroutine  genlat

end module  mgenlat
