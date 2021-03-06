F90=ifort
FCFLAGS=-O3 -qopenmp

TARGET= Particle.exe
OBJECT= Particle_module.o Particle_main.o Particle_setup.o Particle_solver.o    \
				Particle_read.o Particle_setting.o Particle_Boundary_Condition.o				\
				Particle_write.o 

all : $(TARGET)
$(TARGET) : $(OBJECT)
	$(F90) $(FCFLAGS) -o $@ $^

.SUFFIXES. : .o .f90

%.o : %.f90
	$(F90) $(FCFLAGS) -c $<

clean :
	rm -f *.o
	rm -f *.mod
	rm Particle.exe
