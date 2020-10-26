.SUFFIXES : .o .f90
FC = ifort
EXE = shellfish.out

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

OBJS = mod_shellfish.o

shellfish.out : $(OBJS)
	$(FC) -o $@ $(OBJS)

run :
	./$(EXE)

gdb :
	gdb-ia ./$(EXE)

clean :
	rm -f $(OBJS) $(EXE) *.mod
