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
CND_CONF=Debug_laptop
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
LDLIBSOPTIONS=-L../yami4/lib -lPocoFoundation -lPocoNet -lPocoXML -lboost_filesystem -lboost_program_options -lboost_system -lboost_thread ../common/../Debug_laptop/libcommon.a ../yami4/yami4-cpp/../../Debug_laptop/libyami4-cpp.a ../yami4/yami4-core/../../Debug_laptop/libyami4-core.a

# Build Targets
.build-conf: ${BUILD_SUBPROJECTS}
	"${MAKE}"  -f nbproject/Makefile-${CND_CONF}.mk ../${CND_CONF}/control-server

../${CND_CONF}/control-server: ../common/../Debug_laptop/libcommon.a

../${CND_CONF}/control-server: ../yami4/yami4-cpp/../../Debug_laptop/libyami4-cpp.a

../${CND_CONF}/control-server: ../yami4/yami4-core/../../Debug_laptop/libyami4-core.a

../${CND_CONF}/control-server: ${OBJECTFILES}
	${MKDIR} -p ../${CND_CONF}
	${LINK.cc} -o ../${CND_CONF}/control-server ${OBJECTFILES} ${LDLIBSOPTIONS}

${OBJECTDIR}/src/control-server.o: src/control-server.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} $@.d
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/src -I../yami4 -std=c++11 -MMD -MP -MF $@.d -o ${OBJECTDIR}/src/control-server.o src/control-server.cpp

${OBJECTDIR}/src/control-service.o: src/control-service.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} $@.d
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/src -I../yami4 -std=c++11 -MMD -MP -MF $@.d -o ${OBJECTDIR}/src/control-service.o src/control-service.cpp

${OBJECTDIR}/src/http.o: src/http.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} $@.d
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/src -I../yami4 -std=c++11 -MMD -MP -MF $@.d -o ${OBJECTDIR}/src/http.o src/http.cpp

# Subprojects
.build-subprojects:
	cd ../common && ${MAKE}  -f Makefile CONF=Debug_laptop
	cd ../yami4/yami4-cpp && ${MAKE}  -f Makefile CONF=Debug_laptop
	cd ../yami4/yami4-core && ${MAKE}  -f Makefile CONF=Debug_laptop
	cd ../common && ${MAKE}  -f Makefile CONF=Debug_laptop

# Clean Targets
.clean-conf: ${CLEAN_SUBPROJECTS}
	${RM} -r ${CND_BUILDDIR}/${CND_CONF}
	${RM} ../${CND_CONF}/control-server

# Subprojects
.clean-subprojects:
	cd ../common && ${MAKE}  -f Makefile CONF=Debug_laptop clean
	cd ../yami4/yami4-cpp && ${MAKE}  -f Makefile CONF=Debug_laptop clean
	cd ../yami4/yami4-core && ${MAKE}  -f Makefile CONF=Debug_laptop clean
	cd ../common && ${MAKE}  -f Makefile CONF=Debug_laptop clean

# Enable dependency checking
.dep.inc: .depcheck-impl

include .dep.inc
