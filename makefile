PFUNIT = /home/sk/pff
F90_VENDOR = Intel

include $(PFUNIT)/include/base.mk

FFLAGS += -std08 -fpp -warn all -pedantic
LIBS = $(PFUNIT)/lib/libpfunit$(LIB_EXT)

PFS = $(wildcard *.pf)
OBJS = $(PFS:.pf=.o)

%.F90: %.pf
	$(PFUNIT)/bin/pFUnitParser.py $< $@

%.o: %.F90
	$(F90) $(FFLAGS) -c $<

test: testSuites.inc mult.o $(OBJS)
	$(F90) -o $@ -I$(PFUNIT)/mod -I$(PFUNIT)/include \
		$(PFUNIT)/include/driver.F90 \
        ./*$(OBJ_EXT) $(LIBS) $(FFLAGS)
main:
	$(F90) mult.F90 main.F90 -o main $(FFLAGS)
main2:
	$(F90) mult.F90 main.F90 -o main -std08 -fpp -O2
clean:
	rm -f main *.o *genmod.f90 test *.mod