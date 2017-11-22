################################################################################
# the FORTRAN compiler used to compile QuantumEspresso
LD             = mpiifort
# The directory of QE iotk.
LDFLAGS        = -I/public/home/zqj/src/Quantum_Espresso/6.1/qe-6.1/S3DE/iotk/src/
# The location of libiotk.a
AUTO_LIBS      = /public/home/zqj/src/Quantum_Espresso/6.1/qe-6.1/S3DE/iotk/src/libiotk.a
LD_LIBS        = 
MPI_LIBS       = 

############################################################
OBJ = pwscf_kind.o pwscf_wfc.o pwscf_eig.o pwscf_nac.o main.o
pwnac: $(OBJ)
	$(LD) $(LDFLAGS) -o pwnac $(OBJ) $(AUTO_LIBS) $(LD_LIBS) $(MPI_LIBS)

############################################################
kaka: test.o
	$(LD) $(LDFLAGS) -o kaka test.o $(AUTO_LIBS) $(LD_LIBS) $(MPI_LIBS)

############################################################
clean:
	touch *.f90
	rm *.o *.mod

############################################################
.SUFFIXES: .o .f90
.f90.o:
	$(LD) $(LDFLAGS) -c $<
################################################################################
