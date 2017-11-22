module pwscf_eig

  use pwscf_kinds,  only : dp 
  use pwscf_wfc
  use iotk_module

  implicit none

  contains

    subroutine read_eig1k(wfc)
      implicit none

      type(wave1k), intent(inout) :: wfc

      ! local variables
      integer  :: iuni, ierr, nband, i
      character(iotk_attlenx)  :: attr
      character(len=128) :: energy_unit
      real(dp), parameter :: har = 27.211386024367243_dp

      call iotk_free_unit( iuni )
      call iotk_open_read( iuni, file = wfc%eigFile, &
                           binary = .false., ierr = ierr )
      if ( ierr /= 0 ) then
        write(*,*)  'read_eig1k ', &
                     'cannot open restart file for reading', ierr 
      end if

        call iotk_scan_empty( iuni, "INFO", attr )
        call iotk_scan_attr( attr, "nbnd", nband )

        call iotk_scan_empty( iuni, "UNITS_FOR_ENERGIES", attr )
        call iotk_scan_attr( attr, "UNITS", energy_unit )

        if (wfc%nband /= nband) then
          write(*,*) 'Band number in eigenvalue file and wfc file does not match!', &
                     wfc%nband, nband
        end if

        allocate(wfc%eig(nband), wfc%occ(nband), stat=ierr)

        call iotk_scan_dat  ( iuni, "EIGENVALUES", wfc%eig(:)  )
        call iotk_scan_dat  ( iuni, "OCCUPATIONS", wfc%occ(:)  )

        if ( trim(energy_unit) == 'Hartree' ) then
          ! write(*,*) "kaka", energy_unit
          wfc%eig = wfc%eig * har
        end if

      call iotk_close_read( iuni )

      return
    end subroutine read_eig1k

end module pwscf_eig
