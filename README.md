# pwnac
Calculate non-adiabatic couplings (NACs) from PWSCF wavefunctions.

# Compilation
I assume that you have successfully compiled QuantusmEspresso on your system,
because this program utilizes the QE iotk library to read the output files of
PWSCF. For compilation simply change `LDFLAGS` and `AUTO_LIBS` in the Makefile
accordingly and then type `make`. The resulting binary is called `pwnac`.

# Usage
1. `pwnac` is used to calculate the NACs between two wavefunctions.

``` pwnac dir1 dir2 ```

where `dir1` and `dir2` are the directories that contain the pwscf wavefunction,
e.g. "./pwscf.save/K0001".

2. `calc_nac.sh` is used to calculate the NACs between a sequence of
wavefunctiosn and subsequently extract sections of NACs from the full NACs
according to the given `BMIN` and `BMAX`.
