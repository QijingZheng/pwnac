# pwnac
Calculate NACs from PWSCF wavefunctions.

# Compilation
I assume that you have successfully compiled QuantusmEspresso on your system,
because this program utilizes the QE iotk library to read the output files of
PWSCF. For compilation simply change `LDFLAGS` and `AUTO_LIBS` in the Makefile
accordingly and then type `make`.

# Usage

