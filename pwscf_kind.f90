! Fortran kind used in PWSCF.

module pwscf_kinds

  implicit none
  save
  integer, parameter :: dp = selected_real_kind(14,200)
  integer, parameter :: sgl = selected_real_kind(6,30)
  integer, parameter :: i4b = selected_int_kind(9)
  private
  public :: i4b, sgl, dp

end module
