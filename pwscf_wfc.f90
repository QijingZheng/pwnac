module pwscf_wfc

  use pwscf_kinds,  only : dp 
  use iotk_module

  implicit none

  type wave1k
    character(len=1024) :: wfcFile, eigFile
    integer  :: nspin
    integer  :: nkpts
    integer  :: nband
    integer  :: npw
    logical  :: lgamma
    real(dp), allocatable :: eig(:), occ(:)
    complex(dp), allocatable :: evc(:,:)
  end type wave1k

  contains

    subroutine read_wfc1k(wfc)
      implicit none

      type(wave1k), intent(inout) :: wfc

      ! local variables
      character(iotk_attlenx)  :: attr
      integer  :: iuni
      integer  :: ispin, nspin, nkpts, nband, npw, ngw
      integer  :: ik, j, ierr
      real(dp) :: scalef
      logical  :: lgamma

      call iotk_free_unit( iuni )
      call iotk_open_read( iuni, file = wfc%wfcFile, &
                           binary = .true., ierr = ierr )
      if ( ierr /= 0 ) then
        write(*,*)  'read_wfc1k ', &
                     'cannot open restart file for reading', ierr 
      end if

        call iotk_scan_empty( iuni, "INFO", attr )
        !
        call iotk_scan_attr( attr, "ngw",          ngw   )
        call iotk_scan_attr( attr, "igwx",         npw   )
        call iotk_scan_attr( attr, "gamma_only",   lgamma)
        call iotk_scan_attr( attr, "nbnd",         nband )
        call iotk_scan_attr( attr, "ik",           ik    )
        call iotk_scan_attr( attr, "nk",           nkpts )
        call iotk_scan_attr( attr, "ispin",        ispin )
        call iotk_scan_attr( attr, "nspin",        nspin )
        call iotk_scan_attr( attr, "scale_factor", scalef)

        wfc%lgamma = lgamma
        wfc%nspin  = nspin
        wfc%nkpts  = nkpts
        wfc%nband  = nband
        wfc%npw    = npw
        allocate(wfc%evc(npw, nband), stat=ierr)

        do j = 1, nband
          call iotk_scan_dat( iuni, &
                                     "evc" // iotk_index( j ), wfc%evc(1:npw, j) )
        end do

      call iotk_close_read( iuni )

      return
    end subroutine read_wfc1k

end module pwscf_wfc
