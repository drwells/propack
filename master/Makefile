#
#     (C) Rasmus Munk Larsen, Stanford University, 2004
#     (C) David Wells, Virginia Tech, 2014
#
include ../make.inc

all: libXXpropack.a exampleprogs

DOBJ = XXlanbpro.o XXreorth.o XXgetu0.o XXsafescal.o XXblasext.o \
       XXlansvd.o printstat.o XXgemm_ovwr.o \
       XXlansvd_irl.o XXbsvd.o XXritzvec.o XX$(MGS).o psecond.o

.F.o:
	$(FC) $(FFLAGS) -c -o $*.o $*.F

libXXpropack.a: $(DOBJ)
	$(MAKELIB) libXXpropack.a $(DOBJ)
	$(LINKER) -shared libXXpropack.a -o libXXpropack.so $(LIBS)

exampleprogs:
	@( cd Examples; \
	$(MAKE) all; \
	cd .. )

test:
	@( cd Examples; \
	$(MAKE) test; \
	cd .. )

verify:
	@( cd Examples; \
	$(MAKE) verify; \
	cd .. )

clean:
	rm -f  *.o *.il *.a
	rm -rf rii_files
	@( cd Examples; \
	$(MAKE) clean; \
	cd .. )
