module pwscf_nac 
  use pwscf_kinds,  only : dp 
  use pwscf_wfc
  use pwscf_eig

  implicit none

  contains

    subroutine coupij(rundirA, rundirB)
      character(len=*), intent(in) :: rundirA, rundirB

      type(wave1k) :: waveA     ! wavefunction at time t
      type(wave1k) :: waveB     ! wavefunction at time t + dt

      integer     :: i, j
      integer, allocatable :: phase_correct(:)
      complex(dp) :: pij, pji, c0
      complex(dp), allocatable :: Cij(:,:)

      waveA%wfcFile = trim(rundirA) // '/evc.dat'
      waveB%wfcFile = trim(rundirB) // '/evc.dat'
      waveA%eigFile = trim(rundirB) // '/eigenval.xml'

      ! read the plane wave coefficients of wavefunction at t and t + dt
      call read_wfc1k(waveA)
      call read_wfc1k(waveB)
      ! read the eigenvalues at time t
      call read_eig1k(waveA)

      if (waveA%nspin  /= waveB%nspin  .or. &
          waveA%nkpts  /= waveB%nkpts  .or. &
          waveA%nband  /= waveB%nband  .or. &
          waveA%lgamma /= waveB%lgamma .or. &
          waveA%npw    /= waveB%npw )       &
          write(*,*) "Inconsistency between ", trim(waveA%wfcFile), " and ", trim(waveB%wfcFile)

      ! ! phase correction according to "The Journal of Chemical Physics, 122, 034105"
      ! allocate(phase_correct(waveA%nband))
      ! do i=1, waveA%nband
      !   c0 = SUM(CONJG(waveA%evc(:,i)) * waveB%evc(:,i))
      !   if ( real(c0) > 1) then
      !     phase_correct(i) = 1
      !   else
      !     phase_correct(i) = -1
      !   end if
      ! end do

      if (waveA%lgamma) then
        ! For gamma-only case, only half of the plane wave coefficients are
        ! stored.  In the following, plane wave coefficients except that of G = 0
        ! are multiplied by a factor of SQRT(2.0). By doing so,  the
        ! norm of the wavefunction can be easily obtained by calculating
        ! sqrt(sum(conjg(wfc%evc(:,ib)) * wfc%evc(:,ib)))
        ! as is the case for non gamma-only version. Note that this trick is
        ! also used in the gamma version WAVECAR of VASP.

        ! waveA%evc(2:waveA%npw,:) = waveA%evc(2:waveA%npw,:) * sqrt(2.d0) 
        ! waveB%evc(2:waveB%npw,:) = waveB%evc(2:waveB%npw,:) * sqrt(2.d0) 

        ! A little faster than the method above?
        waveA%evc = waveA%evc * sqrt(2.d0) 
        waveB%evc = waveB%evc * sqrt(2.d0) 
        waveA%evc(1,:) = waveA%evc(1,:) / sqrt(2.d0) 
        waveB%evc(1,:) = waveB%evc(1,:) / sqrt(2.d0) 

      ! else
      !   ! For non-gamma version, the wavefunction at G=0 can contain an arbitrary
      !   ! total phase, as a result the phase of the resulting NAC is not determined. 
      !   ! The total phase is determined by the first plane wave coefficients?
      !   do i=1, waveA%nband
      !     c0 = waveA%evc(1,i)
      !     waveA%evc(:,i) = waveA%evc(:,i) * CONJG(c0) / ABS(c0)
      !
      !     c0 = waveB%evc(1,i)
      !     waveB%evc(:,i) = waveB%evc(:,i) * CONJG(c0) / ABS(c0)
      !   end do

      end if

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Nonadiabatic trajectory calculations with ab initio and semiempirical
      ! methods: volume 17. : World Scienti c Publishing Company, 2011: 463–496.
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! <psi_i(t)| d/dt |(psi_j(t))> ~=~
      !                             (<psi_i(t)|psi_j(t+dt)> - <psi_i(t+dt)|psi_j(t)>) / (2dt)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      allocate(Cij(waveA%nband, waveA%nband))
      Cij = (0_dp, 0_dp)

      do i=1, waveA%nband
        do j=i+1, waveA%nband
          ! <psi_i(t)|psi_j(t+dt)>
          pij = SUM(CONJG(waveA%evc(:,i)) * waveB%evc(:,j)) ! * phase_correct(j)
          ! <psi_i(t+dt)|psi_j(t)>
          pji = SUM(CONJG(waveB%evc(:,i)) * waveA%evc(:,j)) ! * phase_correct(i)

          ! Not devided by 2 * dt
          Cij(i, j) = pij - pji
          if ( i /= j ) then
            Cij(j, i) = -CONJG(Cij(i, j))
          end if
        end do
      end do

      call write_nac(Cij, waveA%eig, waveA%nband, rundirA)

      deallocate(Cij, waveA%evc, waveA%eig, waveA%occ,  &
                      waveB%evc                      )
      ! deallocate(phase_correct)

      return
    end subroutine coupij

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    subroutine write_nac(Cij, Eig, nband, rundir)
      implicit none

      complex(dp), intent(in) :: Cij(:,:)
      real(dp), intent(in)    :: Eig(:)
      integer, intent(in)     :: nband
      character(len=*), intent(in) :: rundir

      integer :: i,j, iu1, iu2
      character(len=256) :: formatter

      open(unit=newunit(iu1), file=trim(rundir) // '/eig.dat', &
          status='unknown', action='write')
      open(unit=newunit(iu2), file=trim(rundir) // '/nac.dat', &
          status='unknown', action='write')

      write(formatter,*) '(', nband, '(1X, E17.10))'
      write(unit=iu1, fmt=formatter) (Eig(j), j=1, nband)

      write(formatter,*) '(', nband * nband, '(1X, E17.10))'
      write(unit=iu2, fmt=formatter) ((REAL(Cij(i,j)), j=1, nband), i=1, nband)

      ! write(unit=22, fmt='(1X, F18.10)', advance='no') (Eig(j), j=1, nband)
      ! write(unit=23, fmt='(1X, F18.10)', advance='no') ((REAL(Cij(i,j)), j=1, nband), i=1, nband)
    end subroutine write_nac
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! http://fortranwiki.org/fortran/show/newunit
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! This is a simple function to search for an available unit.
    ! LUN_MIN and LUN_MAX define the range of possible LUNs to check.
    ! The UNIT value is returned by the function, and also by the optional
    ! argument. This allows the function to be used directly in an OPEN
    ! statement, and optionally save the result in a local variable.
    ! If no units are available, -1 is returned.

    integer function newunit(unit)
      integer, intent(out), optional :: unit
      ! local
      integer, parameter :: LUN_MIN=20, LUN_MAX=2000
      logical :: opened
      integer :: lun
      ! begin
      newunit=-1
      do lun=LUN_MIN,LUN_MAX
        inquire(unit=lun,opened=opened)
        if (.not. opened) then
          newunit=lun
          exit
        end if
      end do
      if (present(unit)) unit=newunit
    end function newunit

end module pwscf_nac 
