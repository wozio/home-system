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
CND_CONF=Debug
CND_DISTDIR=dist
CND_BUILDDIR=build

# Include project Makefile
include Makefile

# Object Directory
OBJECTDIR=${CND_BUILDDIR}/${CND_CONF}/${CND_PLATFORM}

# Object Files
OBJECTFILES= \
	${OBJECTDIR}/src/control-server.o \
	${OBJECTDIR}/src/control-service.o \
	${OBJECTDIR}/src/http.o


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
LDLIBSOPTIONS=-L../common/yami4/lib -lPocoFoundation -lPocoNet -lPocoXML -lboost_filesystem -lboost_program_options -lboost_system -lboost_thread ../common/common/../Debug/libcommon.a -lyamicpp -lyamicore

# Build Targets
.build-conf: ${BUILD_SUBPROJECTS}
	"${MAKE}"  -f nbproject/Makefile-${CND_CONF}.mk ../${CND_CONF}/control-server

../${CND_CONF}/control-server: ../common/common/../Debug/libcommon.a

../${CND_CONF}/control-server: ${OBJECTFILES}
	${MKDIR} -p ../${CND_CONF}
	${LINK.cc} -o ../${CND_CONF}/control-server ${OBJECTFILES} ${LDLIBSOPTIONS}

${OBJECTDIR}/src/control-server.o: src/control-server.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/common/src -I../common/yami4/include -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/control-server.o src/control-server.cpp

${OBJECTDIR}/src/control-service.o: src/control-service.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/common/src -I../common/yami4/include -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/control-service.o src/control-service.cpp

${OBJECTDIR}/src/http.o: src/http.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/common/src -I../common/yami4/include -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/http.o src/http.cpp

# Subprojects
.build-subprojects:
	cd ../common/common && ${MAKE}  -f Makefile CONF=Debug

# Clean Targets
.clean-conf: ${CLEAN_SUBPROJECTS}
	${RM} -r ${CND_BUILDDIR}/${CND_CONF}
	${RM} ../${CND_CONF}/control-server

# Subprojects
.clean-subprojects:
	cd ../common/common && ${MAKE}  -f Makefile CONF=Debug clean

# Enable dependency checking
.dep.inc: .depcheck-impl

include .dep.inc
