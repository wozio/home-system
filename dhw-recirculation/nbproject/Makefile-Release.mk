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
CND_PLATFORM=GNU_4.7.3-Linux-x86
CND_DLIB_EXT=so
CND_CONF=Release
CND_DISTDIR=dist
CND_BUILDDIR=build

# Include project Makefile
include Makefile

# Object Directory
OBJECTDIR=${CND_BUILDDIR}/${CND_CONF}/${CND_PLATFORM}

# Object Files
OBJECTFILES= \
	${OBJECTDIR}/src/dhw-recirculation.o \
	${OBJECTDIR}/src/dhwrec-service.o


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
LDLIBSOPTIONS=-L../common/yami4/lib -lPocoFoundation -lPocoNet -lboost_date_time -lboost_program_options -lboost_system -lboost_thread ../common/common/../bin/Release/libcommon.a -lyamicpp -lyamicore

# Build Targets
.build-conf: ${BUILD_SUBPROJECTS}
	"${MAKE}"  -f nbproject/Makefile-${CND_CONF}.mk ../bin/${CND_CONF}/dhw-recirculation

../bin/${CND_CONF}/dhw-recirculation: ../common/common/../bin/Release/libcommon.a

../bin/${CND_CONF}/dhw-recirculation: ${OBJECTFILES}
	${MKDIR} -p ../bin/${CND_CONF}
	${LINK.cc} -o ../bin/${CND_CONF}/dhw-recirculation ${OBJECTFILES} ${LDLIBSOPTIONS} -s

${OBJECTDIR}/src/dhw-recirculation.o: src/dhw-recirculation.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -O3 -Wall -s -I../common/common/src -I../common/yami4/include -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/dhw-recirculation.o src/dhw-recirculation.cpp

${OBJECTDIR}/src/dhwrec-service.o: src/dhwrec-service.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -O3 -Wall -s -I../common/common/src -I../common/yami4/include -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/dhwrec-service.o src/dhwrec-service.cpp

# Subprojects
.build-subprojects:
	cd ../common/common && ${MAKE}  -f Makefile CONF=Release

# Clean Targets
.clean-conf: ${CLEAN_SUBPROJECTS}
	${RM} -r ${CND_BUILDDIR}/${CND_CONF}
	${RM} ../bin/${CND_CONF}/dhw-recirculation

# Subprojects
.clean-subprojects:
	cd ../common/common && ${MAKE}  -f Makefile CONF=Release clean

# Enable dependency checking
.dep.inc: .depcheck-impl

include .dep.inc
