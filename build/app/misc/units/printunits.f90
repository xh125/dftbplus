!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!



!> Printing out the conversion factors for the different units
program printunits
  use dftbp_common_unitconversion, only : lengthUnits, inverseLengthUnits, energyUnits, forceUnits,&
      & timeUnits, freqUnits, volumeUnits, chargeUnits, eFieldUnits, bFieldUnits, pressureUnits,&
      & velocityUnits, dipoleUnits, massUnits, angularUnits, massDensityUnits, TUnit
  implicit none

  type(TUnit) :: localUnit
  integer :: ii

  write(*,*)"Convert from unit to a.u. by multiplying with"
    write(*,*)
    write(*,"(a)")"Length:"
    do ii = 1, size(lengthUnits)
      ! Workaround: nag 7.1
      ! Can not print derived type, if part of an array
      ! write(*,"(1x,dt)") lengthUnits(ii)
      localUnit = lengthUnits(ii)
      write(*,"(1x,dt)") localUnit
    end do
    write(*,*)
    write(*,"(a)")"Inverse length:"
    do ii = 1, size(inverseLengthUnits)
      ! Workaround: nag 7.1
      ! Can not print derived type, if part of an array
      ! write(*,"(1x,dt)") inverseLengthUnits(ii)
      localUnit = inverseLengthUnits(ii)
      write(*,"(1x,dt)") localUnit
    end do
    write(*,*)
    write(*,"(a)")"Energy:"
    do ii = 1, size(energyUnits)
      ! Workaround: nag 7.1
      ! Can not print derived type, if part of an array
      ! write(*,"(1x,dt)") energyUnits(ii)
      localUnit = energyUnits(ii)
      write(*,"(1x,dt)") localUnit
    end do
    write(*,*)
    write(*,"(a)")"Force:"
    do ii = 1, size(forceUnits)
      ! Workaround: nag 7.1
      ! Can not print derived type, if part of an array
      ! write(*,"(1x,dt)") forceUnits(ii)
      localUnit = forceUnits(ii)
      write(*,"(1x,dt)") localUnit
    end do
    write(*,*)
    write(*,"(a)")"Time:"
    do ii = 1, size(timeUnits)
      ! Workaround: nag 7.1
      ! Can not print derived type, if part of an array
      ! write(*,"(1x,dt)") timeUnits(ii)
      localUnit = timeUnits(ii)
      write(*,"(1x,dt)") localUnit
    end do
    write(*,*)
    write(*,"(a)")"Frequency:"
    do ii = 1, size(freqUnits)
      ! Workaround: nag 7.1
      ! Can not print derived type, if part of an array
      ! write(*,"(1x,dt)") freqUnits(ii)
      localUnit = freqUnits(ii)
      write(*,"(1x,dt)") localUnit
    end do
    write(*,*)
    write(*,"(a)")"Volume:"
    do ii = 1, size(volumeUnits)
      ! Workaround: nag 7.1
      ! Can not print derived type, if part of an array
      ! write(*,"(1x,dt)") volumeUnits(ii)
      localUnit = volumeUnits(ii)
      write(*,"(1x,dt)") localUnit
    end do
    write(*,*)
    write(*,"(a)")"Charge:"
    do ii = 1, size(chargeUnits)
      ! Workaround: nag 7.1
      ! Can not print derived type, if part of an array
      ! write(*,"(1x,dt)") chargeUnits(ii)
      localUnit = chargeUnits(ii)
      write(*,"(1x,dt)") localUnit
    end do
    write(*,*)
    write(*,"(a)")"Elec. Field:"
    do ii = 1, size(EFieldUnits)
      ! Workaround: nag 7.1
      ! Can not print derived type, if part of an array
      ! write(*,"(1x,dt)") EFieldUnits(ii)
      localUnit = EFieldUnits(ii)
      write(*,"(1x,dt)") localUnit
    end do
    write(*,*)
    write(*,"(a)")"Mag. Field:"
    do ii = 1, size(BFieldUnits)
      ! Workaround: nag 7.1
      ! Can not print derived type, if part of an array
      ! write(*,"(1x,dt)") BFieldUnits(ii)
      localUnit = BFieldUnits(ii)
      write(*,"(1x,dt)") localUnit
    end do
    write(*,*)
    write(*,"(a)")"Pressure:"
    do ii = 1, size(pressureUnits)
      ! Workaround: nag 7.1
      ! Can not print derived type, if part of an array
      ! write(*,"(1x,dt)") pressureUnits(ii)
      localUnit = pressureUnits(ii)
      write(*,"(1x,dt)") localUnit
    end do
    write(*,*)
    write(*,"(a)")"Velocity:"
    do ii = 1, size(velocityUnits)
      ! Workaround: nag 7.1
      ! Can not print derived type, if part of an array
      ! write(*,"(1x,dt)") velocityUnits(ii)
      localUnit = velocityUnits(ii)
      write(*,"(1x,dt)") localUnit
    end do
    write(*,*)
    write(*,"(a)")"Elec. dipole:"
    do ii = 1, size(dipoleUnits)
      ! Workaround: nag 7.1
      ! Can not print derived type, if part of an array
      ! write(*,"(1x,dt)") dipoleUnits(ii)
      localUnit = dipoleUnits(ii)
      write(*,"(1x,dt)") localUnit
    end do
    write(*,*)
    write(*,"(a)")"Mass:"
    do ii = 1, size(massUnits)
      ! Workaround: nag 7.1
      ! Can not print derived type, if part of an array
      ! write(*,"(1x,dt)") massUnits(ii)
      localUnit = massUnits(ii)
      write(*,"(1x,dt)") localUnit
    end do
    write(*,*)
    write(*,"(a)")"Angular:"
    do ii = 1, size(angularUnits)
      ! Workaround: nag 7.1
      ! Can not print derived type, if part of an array
      ! write(*,"(1x,dt)") angularUnits(ii)
      localUnit = angularUnits(ii)
      write(*,"(1x,dt)") localUnit
    end do
    write(*,*)
    write(*,"(a)")"Mass Density:"
    do ii = 1, size(massDensityUnits)
      ! Workaround: nag 7.1
      ! Can not print derived type, if part of an array
      ! write(*,"(1x,dt)") massDensityUnits(ii)
      localUnit = massDensityUnits(ii)
      write(*,"(1x,dt)") localUnit
    end do

end program printunits
