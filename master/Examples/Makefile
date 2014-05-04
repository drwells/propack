#
#     (C) Rasmus Munk Larsen, Stanford University, 2004
#     (C) David Wells, Virginia Tech, 2014
#
include ../../make.inc
IPATH = -I..
LIBS = ../libXXpropack.a $(BLAS)

all: example example_irl compare

.F.o:
	$(FC) $(FFLAGS) $(IPATH) -c -o $*.o $*.F

matvec.o example.o example_irl.o: matvec.h

example: example.o matvec.o
	$(LINKER) $(LINKFLAGS) -o example example.o matvec.o $(LIBS)

example_irl: example_irl.o matvec.o
	$(LINKER) $(LINKFLAGS) -o example_irl example_irl.o matvec.o $(LIBS)

compare: compare.o
	$(LINKER) $(LINKFLAGS) -o compare compare.o $(LIBS)

test:
	@( echo "==Testing XXXCAP version without implicit restart.=="; \
	./example < example.in; \
	echo "==Testing XXXCAP version with implicit restart.=="; \
	./example_irl < example_irl.in )

verify:
	@( echo "==Comparing XXXCAP results to reference.=="; \
	./compare < compare.in; \
	echo "==Comparing XXXCAP IRL results to reference.=="; \
	./compare < compare_irl.in )

clean:
	rm -f *.o *.il
	rm -rf rii_files

cleanall:
	rm -f *.o *.a example example_irl compare *~ work* *.il *.ascii
	rm -rf rii_files
