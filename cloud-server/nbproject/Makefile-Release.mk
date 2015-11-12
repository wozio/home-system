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
CND_CONF=Release
CND_DISTDIR=dist
CND_BUILDDIR=build

# Include project Makefile
include Makefile

# Object Directory
OBJECTDIR=${CND_BUILDDIR}/${CND_CONF}/${CND_PLATFORM}

# Object Files
OBJECTFILES= \
	${OBJECTDIR}/_ext/888364097/system_handler.o \
	${OBJECTDIR}/client_request_handler.o \
	${OBJECTDIR}/cloud-server.o \
	${OBJECTDIR}/request_handler_factories.o \
	${OBJECTDIR}/system_request_handler.o


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
LDLIBSOPTIONS=-lPocoFoundation -lPocoNet -lpthread -lboost_program_options -lboost_system ../common/common/../bin/Release/libcommon.a

# Build Targets
.build-conf: ${BUILD_SUBPROJECTS}
	"${MAKE}"  -f nbproject/Makefile-${CND_CONF}.mk ../bin/${CND_CONF}/cloud-server

../bin/${CND_CONF}/cloud-server: ../common/common/../bin/Release/libcommon.a

../bin/${CND_CONF}/cloud-server: ${OBJECTFILES}
	${MKDIR} -p ../bin/${CND_CONF}
	${LINK.cc} -o ../bin/${CND_CONF}/cloud-server ${OBJECTFILES} ${LDLIBSOPTIONS} -s

${OBJECTDIR}/_ext/888364097/system_handler.o: //SERVER/storage/develop/home-system/cloud-server/system_handler.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/888364097
	${RM} "$@.d"
	$(COMPILE.cc) -O3 -s -I../common/common/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/_ext/888364097/system_handler.o //SERVER/storage/develop/home-system/cloud-server/system_handler.cpp

${OBJECTDIR}/client_request_handler.o: client_request_handler.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} "$@.d"
	$(COMPILE.cc) -O3 -s -I../common/common/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/client_request_handler.o client_request_handler.cpp

${OBJECTDIR}/cloud-server.o: cloud-server.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} "$@.d"
	$(COMPILE.cc) -O3 -s -I../common/common/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/cloud-server.o cloud-server.cpp

${OBJECTDIR}/request_handler_factories.o: request_handler_factories.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} "$@.d"
	$(COMPILE.cc) -O3 -s -I../common/common/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/request_handler_factories.o request_handler_factories.cpp

${OBJECTDIR}/system_request_handler.o: system_request_handler.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} "$@.d"
	$(COMPILE.cc) -O3 -s -I../common/common/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/system_request_handler.o system_request_handler.cpp

# Subprojects
.build-subprojects:
	cd ../common/common && ${MAKE}  -f Makefile CONF=Release

# Clean Targets
.clean-conf: ${CLEAN_SUBPROJECTS}
	${RM} -r ${CND_BUILDDIR}/${CND_CONF}
	${RM} ../bin/${CND_CONF}/cloud-server

# Subprojects
.clean-subprojects:
	cd ../common/common && ${MAKE}  -f Makefile CONF=Release clean

# Enable dependency checking
.dep.inc: .depcheck-impl

include .dep.inc