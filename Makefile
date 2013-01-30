X10_NPLACES=1
X10_NTHREADS=8
OPT=-cxx-prearg -g -O -NO_CHECKS -define NO_BOUNDS_CHECKS
#OPT=-cxx-prearg -g -x10rt mpi -O -NO_CHECKS -define NO_BOUNDS_CHECK
CC=x10c++
JCC=x10c
SRC=NoC.x10
TARGET=$(SRC:.x10=)
EXECPATH=noc/
all: compile

j_compile:
	$(JCC) $(OPT) $(SRC) 

compile:
	$(CC) $(OPT) $(SRC) 

j_run:
	X10_NPLACES=$(X10_NPLACES) X10_NTHREADS=$(X10_NTHREADS) x10 $(EXECPATH)$(TARGET)

run:
	X10_NPLACES=$(X10_NPLACES) X10_NTHREADS=$(X10_NTHREADS) ./a.out

clean:
	rm -rf *.class *.java $(EXECPATH) a.out xxx_main_xxx.cc
