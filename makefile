.SUFFIXES : .o .f90
FC = ifort
#FFLAGS = -O3 -fpe0 -fp-model precise -xHost

#FFLAGS = -g
FFLAGS = -check all -traceback -g -fpe0 -ftrapuv

#FFLAGS = -O0 -ftrapuv -heap-arrays -auto -g -traceback -fpe0
#FFLAGS = -O0
#FFLAGS = -O1 -fpe0
#FFLAGS = -O3 -ftrapuv -heap-arrays -auto -g -traceback -fpe0 -fp-model precise
#FFLAGS = -O3 -traceback -fpe0 -fp-model precise
#FFLAGS = -O3 -fpe0
#FFLAGS = -check bounds -traceback -g

.f90.o :
	$(FC) $(FFLAGS) -c $<

OBJS = mod_clam.o

clam.out : $(OBJS)
	$(FC) -o $@ $(OBJS)

run :
	./clam.out

gdb :
	gdb-ia ./clam.out

clean :
	rm -f $(OBJS) clam.out *.mod
