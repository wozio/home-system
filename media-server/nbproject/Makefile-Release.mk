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
	${OBJECTDIR}/src/db.o \
	${OBJECTDIR}/src/media-server.o \
	${OBJECTDIR}/src/media-service.o \
	${OBJECTDIR}/src/media/directory.o \
	${OBJECTDIR}/src/media/file.o \
	${OBJECTDIR}/src/media/files.o \
	${OBJECTDIR}/src/media/media.o \
	${OBJECTDIR}/src/media/root.o


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
LDLIBSOPTIONS=-lboost_system -lboost_filesystem -lboost_thread -lPocoFoundation -lPocoData -lPocoSQLite ../yami4/yami4-cpp/../../Release/libyami4-cpp.a ../yami4/yami4-core/../../Release/libyami4-core.a ../common/../Release/libcommon.a

# Build Targets
.build-conf: ${BUILD_SUBPROJECTS}
	"${MAKE}"  -f nbproject/Makefile-${CND_CONF}.mk ../${CND_CONF}/media-server

../${CND_CONF}/media-server: ../yami4/yami4-cpp/../../Release/libyami4-cpp.a

../${CND_CONF}/media-server: ../yami4/yami4-core/../../Release/libyami4-core.a

../${CND_CONF}/media-server: ../common/../Release/libcommon.a

../${CND_CONF}/media-server: ${OBJECTFILES}
	${MKDIR} -p ../${CND_CONF}
	${LINK.cc} -o ../${CND_CONF}/media-server ${OBJECTFILES} ${LDLIBSOPTIONS} -s

${OBJECTDIR}/src/db.o: src/db.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -O3 -Wall -s -I../common/src -I../yami4 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/db.o src/db.cpp

${OBJECTDIR}/src/media-server.o: src/media-server.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -O3 -Wall -s -I../common/src -I../yami4 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/media-server.o src/media-server.cpp

${OBJECTDIR}/src/media-service.o: src/media-service.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -O3 -Wall -s -I../common/src -I../yami4 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/media-service.o src/media-service.cpp

${OBJECTDIR}/src/media/directory.o: src/media/directory.cpp 
	${MKDIR} -p ${OBJECTDIR}/src/media
	${RM} "$@.d"
	$(COMPILE.cc) -O3 -Wall -s -I../common/src -I../yami4 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/media/directory.o src/media/directory.cpp

${OBJECTDIR}/src/media/file.o: src/media/file.cpp 
	${MKDIR} -p ${OBJECTDIR}/src/media
	${RM} "$@.d"
	$(COMPILE.cc) -O3 -Wall -s -I../common/src -I../yami4 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/media/file.o src/media/file.cpp

${OBJECTDIR}/src/media/files.o: src/media/files.cpp 
	${MKDIR} -p ${OBJECTDIR}/src/media
	${RM} "$@.d"
	$(COMPILE.cc) -O3 -Wall -s -I../common/src -I../yami4 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/media/files.o src/media/files.cpp

${OBJECTDIR}/src/media/media.o: src/media/media.cpp 
	${MKDIR} -p ${OBJECTDIR}/src/media
	${RM} "$@.d"
	$(COMPILE.cc) -O3 -Wall -s -I../common/src -I../yami4 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/media/media.o src/media/media.cpp

${OBJECTDIR}/src/media/root.o: src/media/root.cpp 
	${MKDIR} -p ${OBJECTDIR}/src/media
	${RM} "$@.d"
	$(COMPILE.cc) -O3 -Wall -s -I../common/src -I../yami4 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/media/root.o src/media/root.cpp

# Subprojects
.build-subprojects:
	cd ../yami4/yami4-cpp && ${MAKE}  -f Makefile CONF=Release
	cd ../yami4/yami4-core && ${MAKE}  -f Makefile CONF=Release
	cd ../common && ${MAKE}  -f Makefile CONF=Release

# Clean Targets
.clean-conf: ${CLEAN_SUBPROJECTS}
	${RM} -r ${CND_BUILDDIR}/${CND_CONF}
	${RM} ../${CND_CONF}/media-server

# Subprojects
.clean-subprojects:
	cd ../yami4/yami4-cpp && ${MAKE}  -f Makefile CONF=Release clean
	cd ../yami4/yami4-core && ${MAKE}  -f Makefile CONF=Release clean
	cd ../common && ${MAKE}  -f Makefile CONF=Release clean

# Enable dependency checking
.dep.inc: .depcheck-impl

include .dep.inc
