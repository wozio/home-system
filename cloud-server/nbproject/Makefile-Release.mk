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

# Test Directory
TESTDIR=${CND_BUILDDIR}/${CND_CONF}/${CND_PLATFORM}/tests

# Test Files
TESTFILES= \
	${TESTDIR}/TestFiles/f1

# Test Object Files
TESTOBJECTFILES= \
	${TESTDIR}/tests/systemconnection.o

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
LDLIBSOPTIONS=-lPocoFoundation -lPocoNet -lpthread -lboost_program_options -lboost_filesystem -lboost_system ../common/wshandling/../bin/Debug/libwshandling.a ../common/common/../bin/Debug/libcommon.a -lboost_date_time

# Build Targets
.build-conf: ${BUILD_SUBPROJECTS}
	"${MAKE}"  -f nbproject/Makefile-${CND_CONF}.mk ../bin/${CND_CONF}/cloud-server

../bin/${CND_CONF}/cloud-server: ../common/wshandling/../bin/Debug/libwshandling.a

../bin/${CND_CONF}/cloud-server: ../common/common/../bin/Debug/libcommon.a

../bin/${CND_CONF}/cloud-server: ${OBJECTFILES}
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
	cd ../common/wshandling && ${MAKE}  -f Makefile CONF=Debug
	cd ../common/common && ${MAKE}  -f Makefile CONF=Debug

# Build Test Targets
.build-tests-conf: .build-tests-subprojects .build-conf ${TESTFILES}
.build-tests-subprojects:

${TESTDIR}/TestFiles/f1: ${TESTDIR}/tests/systemconnection.o ${OBJECTFILES:%.o=%_nomain.o}
	${MKDIR} -p ${TESTDIR}/TestFiles
	${LINK.cc}   -o ${TESTDIR}/TestFiles/f1 $^ ${LDLIBSOPTIONS} 


${TESTDIR}/tests/systemconnection.o: tests/systemconnection.cpp 
	${MKDIR} -p ${TESTDIR}/tests
	${RM} "$@.d"
	$(COMPILE.cc) -O3 -s -I../common/common/src -I../rapidjson/include -I../common/wshandling -I../common/easyloggingpp/src -I. -std=c++11 -MMD -MP -MF "$@.d" -o ${TESTDIR}/tests/systemconnection.o tests/systemconnection.cpp


${OBJECTDIR}/client_nomain.o: ${OBJECTDIR}/client.o client.cpp 
	${MKDIR} -p ${OBJECTDIR}
	@NMOUTPUT=`${NM} ${OBJECTDIR}/client.o`; \
	if (echo "$$NMOUTPUT" | ${GREP} '|main$$') || \
	   (echo "$$NMOUTPUT" | ${GREP} 'T main$$') || \
	   (echo "$$NMOUTPUT" | ${GREP} 'T _main$$'); \
	then  \
	    ${RM} "$@.d";\
	    $(COMPILE.cc) -O3 -s -I../common/common/src -I../rapidjson/include -I../common/wshandling -I../common/easyloggingpp/src -std=c++11 -Dmain=__nomain -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/client_nomain.o client.cpp;\
	else  \
	    ${CP} ${OBJECTDIR}/client.o ${OBJECTDIR}/client_nomain.o;\
	fi

${OBJECTDIR}/client_request_handler_nomain.o: ${OBJECTDIR}/client_request_handler.o client_request_handler.cpp 
	${MKDIR} -p ${OBJECTDIR}
	@NMOUTPUT=`${NM} ${OBJECTDIR}/client_request_handler.o`; \
	if (echo "$$NMOUTPUT" | ${GREP} '|main$$') || \
	   (echo "$$NMOUTPUT" | ${GREP} 'T main$$') || \
	   (echo "$$NMOUTPUT" | ${GREP} 'T _main$$'); \
	then  \
	    ${RM} "$@.d";\
	    $(COMPILE.cc) -O3 -s -I../common/common/src -I../rapidjson/include -I../common/wshandling -I../common/easyloggingpp/src -std=c++11 -Dmain=__nomain -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/client_request_handler_nomain.o client_request_handler.cpp;\
	else  \
	    ${CP} ${OBJECTDIR}/client_request_handler.o ${OBJECTDIR}/client_request_handler_nomain.o;\
	fi

${OBJECTDIR}/clients_nomain.o: ${OBJECTDIR}/clients.o clients.cpp 
	${MKDIR} -p ${OBJECTDIR}
	@NMOUTPUT=`${NM} ${OBJECTDIR}/clients.o`; \
	if (echo "$$NMOUTPUT" | ${GREP} '|main$$') || \
	   (echo "$$NMOUTPUT" | ${GREP} 'T main$$') || \
	   (echo "$$NMOUTPUT" | ${GREP} 'T _main$$'); \
	then  \
	    ${RM} "$@.d";\
	    $(COMPILE.cc) -O3 -s -I../common/common/src -I../rapidjson/include -I../common/wshandling -I../common/easyloggingpp/src -std=c++11 -Dmain=__nomain -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/clients_nomain.o clients.cpp;\
	else  \
	    ${CP} ${OBJECTDIR}/clients.o ${OBJECTDIR}/clients_nomain.o;\
	fi

${OBJECTDIR}/cloud-server_nomain.o: ${OBJECTDIR}/cloud-server.o cloud-server.cpp 
	${MKDIR} -p ${OBJECTDIR}
	@NMOUTPUT=`${NM} ${OBJECTDIR}/cloud-server.o`; \
	if (echo "$$NMOUTPUT" | ${GREP} '|main$$') || \
	   (echo "$$NMOUTPUT" | ${GREP} 'T main$$') || \
	   (echo "$$NMOUTPUT" | ${GREP} 'T _main$$'); \
	then  \
	    ${RM} "$@.d";\
	    $(COMPILE.cc) -O3 -s -I../common/common/src -I../rapidjson/include -I../common/wshandling -I../common/easyloggingpp/src -std=c++11 -Dmain=__nomain -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/cloud-server_nomain.o cloud-server.cpp;\
	else  \
	    ${CP} ${OBJECTDIR}/cloud-server.o ${OBJECTDIR}/cloud-server_nomain.o;\
	fi

${OBJECTDIR}/request_handler_factories_nomain.o: ${OBJECTDIR}/request_handler_factories.o request_handler_factories.cpp 
	${MKDIR} -p ${OBJECTDIR}
	@NMOUTPUT=`${NM} ${OBJECTDIR}/request_handler_factories.o`; \
	if (echo "$$NMOUTPUT" | ${GREP} '|main$$') || \
	   (echo "$$NMOUTPUT" | ${GREP} 'T main$$') || \
	   (echo "$$NMOUTPUT" | ${GREP} 'T _main$$'); \
	then  \
	    ${RM} "$@.d";\
	    $(COMPILE.cc) -O3 -s -I../common/common/src -I../rapidjson/include -I../common/wshandling -I../common/easyloggingpp/src -std=c++11 -Dmain=__nomain -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/request_handler_factories_nomain.o request_handler_factories.cpp;\
	else  \
	    ${CP} ${OBJECTDIR}/request_handler_factories.o ${OBJECTDIR}/request_handler_factories_nomain.o;\
	fi

${OBJECTDIR}/system_nomain.o: ${OBJECTDIR}/system.o system.cpp 
	${MKDIR} -p ${OBJECTDIR}
	@NMOUTPUT=`${NM} ${OBJECTDIR}/system.o`; \
	if (echo "$$NMOUTPUT" | ${GREP} '|main$$') || \
	   (echo "$$NMOUTPUT" | ${GREP} 'T main$$') || \
	   (echo "$$NMOUTPUT" | ${GREP} 'T _main$$'); \
	then  \
	    ${RM} "$@.d";\
	    $(COMPILE.cc) -O3 -s -I../common/common/src -I../rapidjson/include -I../common/wshandling -I../common/easyloggingpp/src -std=c++11 -Dmain=__nomain -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/system_nomain.o system.cpp;\
	else  \
	    ${CP} ${OBJECTDIR}/system.o ${OBJECTDIR}/system_nomain.o;\
	fi

${OBJECTDIR}/system_request_handler_nomain.o: ${OBJECTDIR}/system_request_handler.o system_request_handler.cpp 
	${MKDIR} -p ${OBJECTDIR}
	@NMOUTPUT=`${NM} ${OBJECTDIR}/system_request_handler.o`; \
	if (echo "$$NMOUTPUT" | ${GREP} '|main$$') || \
	   (echo "$$NMOUTPUT" | ${GREP} 'T main$$') || \
	   (echo "$$NMOUTPUT" | ${GREP} 'T _main$$'); \
	then  \
	    ${RM} "$@.d";\
	    $(COMPILE.cc) -O3 -s -I../common/common/src -I../rapidjson/include -I../common/wshandling -I../common/easyloggingpp/src -std=c++11 -Dmain=__nomain -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/system_request_handler_nomain.o system_request_handler.cpp;\
	else  \
	    ${CP} ${OBJECTDIR}/system_request_handler.o ${OBJECTDIR}/system_request_handler_nomain.o;\
	fi

${OBJECTDIR}/systems_nomain.o: ${OBJECTDIR}/systems.o systems.cpp 
	${MKDIR} -p ${OBJECTDIR}
	@NMOUTPUT=`${NM} ${OBJECTDIR}/systems.o`; \
	if (echo "$$NMOUTPUT" | ${GREP} '|main$$') || \
	   (echo "$$NMOUTPUT" | ${GREP} 'T main$$') || \
	   (echo "$$NMOUTPUT" | ${GREP} 'T _main$$'); \
	then  \
	    ${RM} "$@.d";\
	    $(COMPILE.cc) -O3 -s -I../common/common/src -I../rapidjson/include -I../common/wshandling -I../common/easyloggingpp/src -std=c++11 -Dmain=__nomain -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/systems_nomain.o systems.cpp;\
	else  \
	    ${CP} ${OBJECTDIR}/systems.o ${OBJECTDIR}/systems_nomain.o;\
	fi

# Run Test Targets
.test-conf:
	@if [ "${TEST}" = "" ]; \
	then  \
	    ${TESTDIR}/TestFiles/f1 || true; \
	else  \
	    ./${TEST} || true; \
	fi

# Clean Targets
.clean-conf: ${CLEAN_SUBPROJECTS}
	${RM} -r ${CND_BUILDDIR}/${CND_CONF}
	${RM} ../bin/${CND_CONF}/cloud-server

# Subprojects
.clean-subprojects:
	cd ../common/wshandling && ${MAKE}  -f Makefile CONF=Debug clean
	cd ../common/common && ${MAKE}  -f Makefile CONF=Debug clean

# Enable dependency checking
.dep.inc: .depcheck-impl

include .dep.inc
