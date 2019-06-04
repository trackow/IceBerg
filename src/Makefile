executable = ../bin/amip-forcing.exe

VPATH = cplng

cplng_srcs = \
    cplng_data_mod.F90 \
    cplng_init_mod.F90 \
    cplng_config_mod.F90 \
    cplng_exchange_mod.F90 \
    cplng_finalize_mod.F90 \
    cplng_mod.F90

amipforc_srcs = amip_forcing_mod.F90 amip_forcing.F90

$(executable): config_mod.o $(cplng_srcs:.F90=.o) $(amipforc_srcs:.F90=.o)
	$(OIFS_FC) $(OIFS_CFLAGS) -o $@ $^ $(OIFS_OASIS_LIB) $(OIFS_NETCDFF_LIB) $(OIFS_MPI_LIB)

%.o: %.F90
	$(OIFS_FC) $(OIFS_FFLAGS) -c $(OIFS_OASIS_INCLUDE) -I$(NETCDF_Fortran_INCLUDE_DIRECTORIES) $<

.PHONY: clean
clean:
	@rm -f *.o *.mod
