#This file is generated by vimf90 plugin
#This is a bare bone Makefile.am file
bin_PROGRAMS =	kmc
kmc_SOURCES =	src/main.f90 src/constants.f90 src/genlat.f90 src/util.f90
if FOUND_MAKEDEPF90
depend depend.mk:
	makedepf90 $(kmc_SOURCES) >depend.mk
@am__include@ @am__quote@depend.mk@am__quote@
else
$(warning Create the dependencies Manually or try installing makedepf90)
$(error  like ./src/main.o:./src/main.f90)
endif
kmc_LDADD = 
EXTRA_DIST=depend.mk autogen.sh Makefile kmc.in 
CLEANFILES =*.mod  *.log
