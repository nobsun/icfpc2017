TARGET=main
CXX = g++
OPTIMIZE = -O3
CPP11 = -std=c++11 -std=c++1y
WARNING = -Wall
DEBUG = -g
CFLAGS = ${OPTIMIZE} ${WARNING} ${CPP11} ${DEBUG}
LIBS = -lm
RM = rm

SRCS=${shell ls src/*.cpp}
HEDS=${shell ls src/*.h}
OBJS=${SRCS:.cpp=.o}

.SUFFIXES:.cpp .o .h

.PHONY: all
all : ${TARGET}

${TARGET} : ${OBJS}
	${CXX} ${CFLAGS} -o $@ ${OBJS} ${LIBS}

.cpp.o:
	${CXX} ${CFLAGS} -c $< -o $@

.PHONY: clean

clean:
	${RM} -f ${TARGET} src/*~ src/*.o *~
# DO NOT DELETE
