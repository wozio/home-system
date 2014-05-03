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
CND_CONF=Debug_tv
CND_DISTDIR=dist
CND_BUILDDIR=build

# Include project Makefile
include Makefile

# Object Directory
OBJECTDIR=${CND_BUILDDIR}/${CND_CONF}/${CND_PLATFORM}

# Object Files
OBJECTFILES= \
	${OBJECTDIR}/test.o


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
LDLIBSOPTIONS=-lboost_system -lboost_thread ../common/../Debug_tv/libcommon.a ../yami4/yami4-cpp/../../Debug_tv/libyami4-cpp.a ../yami4/yami4-core/../../Debug_tv/libyami4-core.a -lPocoNet -lPocoFoundation -lpthread

# Build Targets
.build-conf: ${BUILD_SUBPROJECTS}
	"${MAKE}"  -f nbproject/Makefile-${CND_CONF}.mk ../${CND_CONF}/test

../${CND_CONF}/test: ../common/../Debug_tv/libcommon.a

../${CND_CONF}/test: ../yami4/yami4-cpp/../../Debug_tv/libyami4-cpp.a

../${CND_CONF}/test: ../yami4/yami4-core/../../Debug_tv/libyami4-core.a

../${CND_CONF}/test: ${OBJECTFILES}
	${MKDIR} -p ../${CND_CONF}
	${LINK.cc} -o ../${CND_CONF}/test ${OBJECTFILES} ${LDLIBSOPTIONS}

${OBJECTDIR}/test.o: test.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} "$@.d"
	$(COMPILE.cc) -g -D_DEBUG -I../common/src -I../yami4 -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/test.o test.cpp

# Subprojects
.build-subprojects:
	cd ../common && ${MAKE}  -f Makefile CONF=Debug_tv
	cd ../yami4/yami4-cpp && ${MAKE}  -f Makefile CONF=Debug_tv
	cd ../yami4/yami4-core && ${MAKE}  -f Makefile CONF=Debug_tv

# Clean Targets
.clean-conf: ${CLEAN_SUBPROJECTS}
	${RM} -r ${CND_BUILDDIR}/${CND_CONF}
	${RM} ../${CND_CONF}/test

# Subprojects
.clean-subprojects:
	cd ../common && ${MAKE}  -f Makefile CONF=Debug_tv clean
	cd ../yami4/yami4-cpp && ${MAKE}  -f Makefile CONF=Debug_tv clean
	cd ../yami4/yami4-core && ${MAKE}  -f Makefile CONF=Debug_tv clean

# Enable dependency checking
.dep.inc: .depcheck-impl

include .dep.inc
