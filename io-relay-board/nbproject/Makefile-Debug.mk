#
# Generated Makefile - do not edit!
#
# Edit the Makefile in the project folder instead (../Makefile). Each target
# has a -pre and a -post target defined where you can add customized code.
#
# This makefile implements configuration specific macros and targets.


# Environment
MKDIR=mkdir
CP=cp
GREP=grep
NM=nm
CCADMIN=CCadmin
RANLIB=ranlib
CC=gcc
CCC=g++
CXX=g++
FC=gfortran
AS=as

# Macros
CND_PLATFORM=GNU-Linux-x86
CND_DLIB_EXT=so
CND_CONF=Debug
CND_DISTDIR=dist
CND_BUILDDIR=build

# Include project Makefile
include Makefile

# Object Directory
OBJECTDIR=${CND_BUILDDIR}/${CND_CONF}/${CND_PLATFORM}

# Object Files
OBJECTFILES= \
	${OBJECTDIR}/src/io-relay-board.o \
	${OBJECTDIR}/src/iorb-service.o \
	${OBJECTDIR}/src/rbport.o


# C Compiler Flags
CFLAGS=

# CC Compiler Flags
CCFLAGS=
CXXFLAGS=

# Fortran Compiler Flags
FFLAGS=

# Assembler Flags
ASFLAGS=

# Link Libraries and Options
LDLIBSOPTIONS=-L../common/yami4/lib -lboost_program_options -lboost_thread -lboost_system ../common/common/../bin/Debug/libcommon.a -lyamicpp -lyamicore

# Build Targets
.build-conf: ${BUILD_SUBPROJECTS}
	"${MAKE}"  -f nbproject/Makefile-${CND_CONF}.mk ../bin/${CND_CONF}/io-relay-board

../bin/${CND_CONF}/io-relay-board: ../common/common/../bin/Debug/libcommon.a

../bin/${CND_CONF}/io-relay-board: ${OBJECTFILES}
	${MKDIR} -p ../bin/${CND_CONF}
	${LINK.cc} -o ../bin/${CND_CONF}/io-relay-board ${OBJECTFILES} ${LDLIBSOPTIONS}

${OBJECTDIR}/src/io-relay-board.o: src/io-relay-board.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/common/src -I../common/yami4/include -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/io-relay-board.o src/io-relay-board.cpp

${OBJECTDIR}/src/iorb-service.o: src/iorb-service.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/common/src -I../common/yami4/include -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/iorb-service.o src/iorb-service.cpp

${OBJECTDIR}/src/rbport.o: src/rbport.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/common/src -I../common/yami4/include -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/rbport.o src/rbport.cpp

# Subprojects
.build-subprojects:
	cd ../common/common && ${MAKE}  -f Makefile CONF=Debug

# Clean Targets
.clean-conf: ${CLEAN_SUBPROJECTS}
	${RM} -r ${CND_BUILDDIR}/${CND_CONF}
	${RM} ../bin/${CND_CONF}/io-relay-board

# Subprojects
.clean-subprojects:
	cd ../common/common && ${MAKE}  -f Makefile CONF=Debug clean

# Enable dependency checking
.dep.inc: .depcheck-impl

include .dep.inc
