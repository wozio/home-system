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
CND_PLATFORM=None-Windows
CND_DLIB_EXT=dll
CND_CONF=Release
CND_DISTDIR=dist
CND_BUILDDIR=build

# Include project Makefile
include Makefile

# Object Directory
OBJECTDIR=${CND_BUILDDIR}/${CND_CONF}/${CND_PLATFORM}

# Object Files
OBJECTFILES= \
	${OBJECTDIR}/client.o \
	${OBJECTDIR}/client_request_handler.o \
	${OBJECTDIR}/clients.o \
	${OBJECTDIR}/cloud-server.o \
	${OBJECTDIR}/request_handler_factories.o \
	${OBJECTDIR}/system.o \
	${OBJECTDIR}/system_request_handler.o \
	${OBJECTDIR}/systems.o


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
LDLIBSOPTIONS=-lPocoFoundation -lPocoNet -lpthread -lboost_system ../common/wshandling/../bin/Debug/libwshandling.a ../common/common/../bin/Debug/libcommon.a -lboost_program_options -lboost_filesystem

# Build Targets
.build-conf: ${BUILD_SUBPROJECTS}
	"${MAKE}"  -f nbproject/Makefile-${CND_CONF}.mk ../bin/${CND_CONF}/cloud-server.exe

../bin/${CND_CONF}/cloud-server.exe: ../common/wshandling/../bin/${CND_CONF}/libwshandling.a

../bin/${CND_CONF}/cloud-server.exe: ../common/common/../bin/${CND_CONF}/libcommon.a

../bin/${CND_CONF}/cloud-server.exe: ${OBJECTFILES}
	${MKDIR} -p ../bin/${CND_CONF}
	${LINK.cc} -o ../bin/${CND_CONF}/cloud-server ${OBJECTFILES} ${LDLIBSOPTIONS} -s

${OBJECTDIR}/client.o: client.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} "$@.d"
	$(COMPILE.cc) -O3 -s -I../common/common/src -I../rapidjson/include -I../common/wshandling -I../common/easyloggingpp/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/client.o client.cpp

${OBJECTDIR}/client_request_handler.o: client_request_handler.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} "$@.d"
	$(COMPILE.cc) -O3 -s -I../common/common/src -I../rapidjson/include -I../common/wshandling -I../common/easyloggingpp/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/client_request_handler.o client_request_handler.cpp

${OBJECTDIR}/clients.o: clients.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} "$@.d"
	$(COMPILE.cc) -O3 -s -I../common/common/src -I../rapidjson/include -I../common/wshandling -I../common/easyloggingpp/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/clients.o clients.cpp

${OBJECTDIR}/cloud-server.o: cloud-server.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} "$@.d"
	$(COMPILE.cc) -O3 -s -I../common/common/src -I../rapidjson/include -I../common/wshandling -I../common/easyloggingpp/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/cloud-server.o cloud-server.cpp

${OBJECTDIR}/request_handler_factories.o: request_handler_factories.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} "$@.d"
	$(COMPILE.cc) -O3 -s -I../common/common/src -I../rapidjson/include -I../common/wshandling -I../common/easyloggingpp/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/request_handler_factories.o request_handler_factories.cpp

${OBJECTDIR}/system.o: system.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} "$@.d"
	$(COMPILE.cc) -O3 -s -I../common/common/src -I../rapidjson/include -I../common/wshandling -I../common/easyloggingpp/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/system.o system.cpp

${OBJECTDIR}/system_request_handler.o: system_request_handler.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} "$@.d"
	$(COMPILE.cc) -O3 -s -I../common/common/src -I../rapidjson/include -I../common/wshandling -I../common/easyloggingpp/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/system_request_handler.o system_request_handler.cpp

${OBJECTDIR}/systems.o: systems.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} "$@.d"
	$(COMPILE.cc) -O3 -s -I../common/common/src -I../rapidjson/include -I../common/wshandling -I../common/easyloggingpp/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/systems.o systems.cpp

# Subprojects
.build-subprojects:
	cd ../common/wshandling && ${MAKE}  -f Makefile CONF=${CND_CONF}
	cd ../common/common && ${MAKE}  -f Makefile CONF=${CND_CONF}

# Clean Targets
.clean-conf: ${CLEAN_SUBPROJECTS}
	${RM} -r ${CND_BUILDDIR}/${CND_CONF}
	${RM} ../bin/${CND_CONF}/cloud-server.exe

# Subprojects
.clean-subprojects:
	cd ../common/wshandling && ${MAKE}  -f Makefile CONF=${CND_CONF} clean
	cd ../common/common && ${MAKE}  -f Makefile CONF=${CND_CONF} clean

# Enable dependency checking
.dep.inc: .depcheck-impl

include .dep.inc
