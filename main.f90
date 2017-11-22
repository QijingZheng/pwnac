program main
  use pwscf_wfc
  use pwscf_nac

  implicit none

  character(len=1024) :: rundirA, rundirB
  integer :: i

  i = command_argument_count()
  if ( i /= 2 ) then
    write(*,*) "Usage: pwnac rundir_t rundir_t+dt"
    stop
  else
      call get_command_argument(1, rundirA)
      call get_command_argument(2, rundirB)
  end if

  call coupij(trim(rundirA), trim(rundirB))

end program main
