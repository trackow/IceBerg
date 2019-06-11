executable = ../bin/iceberg.exe

VPATH = cplng

cplng_srcs = \
    cplng_data_mod.F90 \
    cplng_init_mod.F90 \
    cplng_config_mod.F90 \
    cplng_exchange_mod.F90 \
    cplng_finalize_mod.F90 \
    cplng_mod.F90

iceberg_srcs = iceberg_modules.F90 iceberg_main.F90

$(executable): iceberg_config.o $(cplng_srcs:.F90=.o) $(iceberg_srcs:.F90=.o)
	$(FC) -o $@ $^ $(ICEBERG_OASIS_LIB) -L${NETCDF_DIR}/lib -lnetcdff $(ICEBERG_MPI_LIB)

%.o: %.F90
	$(FC) $(ICEBERG_FFLAGS) -c $(ICEBERG_OASIS_INCLUDE) -I${NETCDF_DIR}/include $<

.PHONY: clean
clean:
	@rm -f *.o *.mod
