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
	${OBJECTDIR}/src/db.o \
	${OBJECTDIR}/src/epg.o \
	${OBJECTDIR}/src/recordings.o \
	${OBJECTDIR}/src/session.o \
	${OBJECTDIR}/src/source.o \
	${OBJECTDIR}/src/sources.o \
	${OBJECTDIR}/src/tv-service.o \
	${OBJECTDIR}/src/tv.o


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
LDLIBSOPTIONS=-L../common/yami4/lib -lPocoData -lPocoFoundation -lPocoNet -lPocoDataSQLite -lboost_program_options -lboost_system -lboost_filesystem -lboost_thread -lpthread ../common/common/../bin/Debug/libcommon.a -lyamicpp -lyamicore

# Build Targets
.build-conf: ${BUILD_SUBPROJECTS}
	"${MAKE}"  -f nbproject/Makefile-${CND_CONF}.mk ../bin/${CND_CONF}/tv

../bin/${CND_CONF}/tv: ../common/common/../bin/Debug/libcommon.a

../bin/${CND_CONF}/tv: ${OBJECTFILES}
	${MKDIR} -p ../bin/${CND_CONF}
	${LINK.cc} -o ../bin/${CND_CONF}/tv ${OBJECTFILES} ${LDLIBSOPTIONS}

${OBJECTDIR}/src/clients.o: src/clients.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/common/src -I../common/yami4/include -I../common/easyloggingpp/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/clients.o src/clients.cpp

${OBJECTDIR}/src/db.o: src/db.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/common/src -I../common/yami4/include -I../common/easyloggingpp/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/db.o src/db.cpp

${OBJECTDIR}/src/epg.o: src/epg.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/common/src -I../common/yami4/include -I../common/easyloggingpp/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/epg.o src/epg.cpp

${OBJECTDIR}/src/recordings.o: src/recordings.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/common/src -I../common/yami4/include -I../common/easyloggingpp/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/recordings.o src/recordings.cpp

${OBJECTDIR}/src/session.o: src/session.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/common/src -I../common/yami4/include -I../common/easyloggingpp/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/session.o src/session.cpp

${OBJECTDIR}/src/source.o: src/source.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/common/src -I../common/yami4/include -I../common/easyloggingpp/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/source.o src/source.cpp

${OBJECTDIR}/src/sources.o: src/sources.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/common/src -I../common/yami4/include -I../common/easyloggingpp/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/sources.o src/sources.cpp

${OBJECTDIR}/src/tv-service.o: src/tv-service.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/common/src -I../common/yami4/include -I../common/easyloggingpp/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/tv-service.o src/tv-service.cpp

${OBJECTDIR}/src/tv.o: src/tv.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/common/src -I../common/yami4/include -I../common/easyloggingpp/src -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/tv.o src/tv.cpp

# Subprojects
.build-subprojects:
	cd ../common/common && ${MAKE}  -f Makefile CONF=Debug

# Clean Targets
.clean-conf: ${CLEAN_SUBPROJECTS}
	${RM} -r ${CND_BUILDDIR}/${CND_CONF}
	${RM} ../bin/${CND_CONF}/tv

# Subprojects
.clean-subprojects:
	cd ../common/common && ${MAKE}  -f Makefile CONF=Debug clean

# Enable dependency checking
.dep.inc: .depcheck-impl

include .dep.inc
