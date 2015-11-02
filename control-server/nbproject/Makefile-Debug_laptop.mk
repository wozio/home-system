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
CND_PLATFORM=MinGW-Windows
CND_DLIB_EXT=dll
CND_CONF=Debug_laptop
CND_DISTDIR=dist
CND_BUILDDIR=build

# Include project Makefile
include Makefile

# Object Directory
OBJECTDIR=${CND_BUILDDIR}/${CND_CONF}/${CND_PLATFORM}

# Object Files
OBJECTFILES= \
	${OBJECTDIR}/_ext/278941292/json_converter.o \
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
LDLIBSOPTIONS=-L../common/yami4/lib -lPocoFoundation -lPocoNet -lPocoXML -lboost_filesystem -lboost_program_options -lboost_system -lboost_thread ../common/common/../bin/Debug_laptop/libcommon.a -lyamicpp -lyamicore

# Build Targets
.build-conf: ${BUILD_SUBPROJECTS}
	"${MAKE}"  -f nbproject/Makefile-${CND_CONF}.mk ../bin/${CND_CONF}/control-server.exe

../bin/${CND_CONF}/control-server.exe: ../common/common/../bin/Debug_laptop/libcommon.a

../bin/${CND_CONF}/control-server.exe: ${OBJECTFILES}
	${MKDIR} -p ../bin/${CND_CONF}
	${LINK.cc} -o ../bin/${CND_CONF}/control-server ${OBJECTFILES} ${LDLIBSOPTIONS}

${OBJECTDIR}/_ext/278941292/json_converter.o: //SERVER/storage/develop/home-system/control-server/src/json_converter.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/278941292
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/common/src -I../common/yami4/include -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/_ext/278941292/json_converter.o //SERVER/storage/develop/home-system/control-server/src/json_converter.cpp

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
	cd ../common/common && ${MAKE}  -f Makefile CONF=Debug_laptop

# Clean Targets
.clean-conf: ${CLEAN_SUBPROJECTS}
	${RM} -r ${CND_BUILDDIR}/${CND_CONF}
	${RM} ../bin/${CND_CONF}/control-server.exe

# Subprojects
.clean-subprojects:
	cd ../common/common && ${MAKE}  -f Makefile CONF=Debug_laptop clean

# Enable dependency checking
.dep.inc: .depcheck-impl

include .dep.inc
