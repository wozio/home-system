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
CND_PLATFORM=GNU-Linux
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
	${OBJECTDIR}/src/clients.o \
	${OBJECTDIR}/src/client.o \
	${OBJECTDIR}/src/client_service.o \
	${OBJECTDIR}/src/cloud_client.o \
	${OBJECTDIR}/src/cloud_ws.o \
	${OBJECTDIR}/src/control-server.o \
	${OBJECTDIR}/src/control-service.o \
	${OBJECTDIR}/src/file_request_handler.o \
	${OBJECTDIR}/src/http.o \
	${OBJECTDIR}/src/json_converter.o \
	${OBJECTDIR}/src/ws_request_handler.o


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
LDLIBSOPTIONS=-L../common/yami4/lib -lboost_program_options -lboost_system -lboost_thread ../common/wshandling/../bin/Debug/libwshandling.a ../common/common/../bin/Debug/libcommon.a -lyamicpp -lyamicore -lpthread -lPocoFoundation -lPocoNet -lPocoNetSSL -lboost_filesystem

# Build Targets
.build-conf: ${BUILD_SUBPROJECTS}
	"${MAKE}"  -f nbproject/Makefile-${CND_CONF}.mk ../bin/${CND_CONF}/control-server

../bin/${CND_CONF}/control-server: ../common/wshandling/../bin/Debug/libwshandling.a

../bin/${CND_CONF}/control-server: ../common/common/../bin/Debug/libcommon.a

../bin/${CND_CONF}/control-server: ${OBJECTFILES}
	${MKDIR} -p ../bin/${CND_CONF}
	${LINK.cc} -o ../bin/${CND_CONF}/control-server ${OBJECTFILES} ${LDLIBSOPTIONS}

${OBJECTDIR}/src/clients.o: src/clients.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/common/src -I../common/yami4/include -I../rapidjson/include -I../common/wshandling -I../common/easyloggingpp/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/clients.o src/clients.cpp
	
${OBJECTDIR}/src/client.o: src/client.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/common/src -I../common/yami4/include -I../rapidjson/include -I../common/wshandling -I../common/easyloggingpp/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/client.o src/client.cpp

${OBJECTDIR}/src/client_service.o: src/client_service.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/common/src -I../common/yami4/include -I../rapidjson/include -I../common/wshandling -I../common/easyloggingpp/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/client_service.o src/client_service.cpp

${OBJECTDIR}/src/cloud_client.o: src/cloud_client.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/common/src -I../common/yami4/include -I../rapidjson/include -I../common/wshandling -I../common/easyloggingpp/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/cloud_client.o src/cloud_client.cpp

${OBJECTDIR}/src/cloud_ws.o: src/cloud_ws.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/common/src -I../common/yami4/include -I../rapidjson/include -I../common/wshandling -I../common/easyloggingpp/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/cloud_ws.o src/cloud_ws.cpp

${OBJECTDIR}/src/control-server.o: src/control-server.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/common/src -I../common/yami4/include -I../rapidjson/include -I../common/wshandling -I../common/easyloggingpp/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/control-server.o src/control-server.cpp

${OBJECTDIR}/src/control-service.o: src/control-service.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/common/src -I../common/yami4/include -I../rapidjson/include -I../common/wshandling -I../common/easyloggingpp/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/control-service.o src/control-service.cpp

${OBJECTDIR}/src/file_request_handler.o: src/file_request_handler.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/common/src -I../common/yami4/include -I../rapidjson/include -I../common/wshandling -I../common/easyloggingpp/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/file_request_handler.o src/file_request_handler.cpp

${OBJECTDIR}/src/http.o: src/http.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/common/src -I../common/yami4/include -I../rapidjson/include -I../common/wshandling -I../common/easyloggingpp/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/http.o src/http.cpp

${OBJECTDIR}/src/json_converter.o: src/json_converter.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/common/src -I../common/yami4/include -I../rapidjson/include -I../common/wshandling -I../common/easyloggingpp/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/json_converter.o src/json_converter.cpp

${OBJECTDIR}/src/ws_request_handler.o: src/ws_request_handler.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/common/src -I../common/yami4/include -I../rapidjson/include -I../common/wshandling -I../common/easyloggingpp/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/ws_request_handler.o src/ws_request_handler.cpp

# Subprojects
.build-subprojects:
	cd ../common/wshandling && ${MAKE}  -f Makefile CONF=Debug
	cd ../common/common && ${MAKE}  -f Makefile CONF=Debug

# Clean Targets
.clean-conf: ${CLEAN_SUBPROJECTS}
	${RM} -r ${CND_BUILDDIR}/${CND_CONF}
	${RM} ../bin/${CND_CONF}/control-server

# Subprojects
.clean-subprojects:
	cd ../common/wshandling && ${MAKE}  -f Makefile CONF=Debug clean
	cd ../common/common && ${MAKE}  -f Makefile CONF=Debug clean

# Enable dependency checking
.dep.inc: .depcheck-impl

include .dep.inc
