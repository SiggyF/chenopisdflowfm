ACLOCAL_AMFLAGS=-I m4
bin_PROGRAMS=swanfm

# coupler, module and application
swanfm_SOURCES=swanfmcoupler.F90 swanfmmod.F90 swanfmapp.F90

ESMF_DIR=../../esmf-git/DEFAULTINSTALLDIR

# ESMF_MODS=${ESMF_DIR}/mod
# ESMF_LIB=${ESMF_DIR}/lib
# needed for debugging
ESMF_MODS=${ESMF_DIR}/mod/modg/Darwin.gfortran.64.mpiuni.default
ESMF_LIB=${ESMF_DIR}/lib/libg/Darwin.gfortran.64.mpiuni.default
SWAN_LIB=${HOME}/.local/lib
DFLOW_LIB=${HOME}/.local/lib



# Set flags off by default
swanfm_FCFLAGS=
swanfm_FCFLAGS+=-I${ESMF_MODS}
swanfm_FCFLAGS=$(OPENMP_FCFLAGS)

swanfm_FCFLAGS+=${NETCDF_CXX_CFLAGS} ${NETCDF_FORTRAN_CFLAGS}
swanfm_LDADD=-L${ESMF_LIB} -lmpi_cxx -lnetcdff -lnetcdf_c++ -lnetcdf  ${NETCDF_FORTRAN_LIBS} ${NETCDF_CXX_LIBS}
# ${LIBDFLOWFM} ${FM}
# Do static, problems with ESMF debugging
swanfm_LDADD+=${ESMF_LIB}/libesmf.a ${SWAN_LIB}/libswanesmf.a ${DFLOW_LIB}/libdflowfm.a
FC=${MPIFC}
CLEANFILES=*.mod *.MOD
