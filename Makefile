FC=gfortran
EXE=sample-reader.exe
MOD=module_netcdf.f90
OBJ=${MOD:.f90=.o}

FLAGS=-I/Users/xtian/opt/miniconda3/envs/xtian/include -I/Users/xtian/opt/miniconda3/envs/xtian/include
LIBS=-L/Users/xtian/opt/miniconda3/envs/xtian/lib -lnetcdff -lnetcdf

MAIN=nc-caller.f90

${EXE}:		${MAIN} ${OBJ}
		${FC} ${FLAGS} ${LIBS} ${MAIN} ${OBJ} -o $@
${OBJ}:		${MOD}
		${FC} -c ${FLAGS} ${LIBS} ${MOD}

clean:
		rm ./*.o ./*.exe *.mod ./*~
