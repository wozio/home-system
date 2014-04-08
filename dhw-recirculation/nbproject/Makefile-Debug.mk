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
CND_CONF=Debug
CND_DISTDIR=dist
CND_BUILDDIR=build

# Include project Makefile
include Makefile

# Object Directory
OBJECTDIR=${CND_BUILDDIR}/${CND_CONF}/${CND_PLATFORM}

# Object Files
OBJECTFILES= \
	${OBJECTDIR}/src/dhw-recirculation.o \
	${OBJECTDIR}/src/dhwrec-service.o


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
LDLIBSOPTIONS=-L../yami4/lib -lPocoFoundation -lPocoNet -lboost_date_time -lboost_program_options -lboost_system -lboost_thread ../yami4/yami4-cpp/../../Debug/libyami4-cpp.a ../yami4/yami4-core/../../Debug/libyami4-core.a ../common/../Debug/libcommon.a

# Build Targets
.build-conf: ${BUILD_SUBPROJECTS}
	"${MAKE}"  -f nbproject/Makefile-${CND_CONF}.mk ../${CND_CONF}/dhw-recirculation

../${CND_CONF}/dhw-recirculation: ../yami4/yami4-cpp/../../Debug/libyami4-cpp.a

../${CND_CONF}/dhw-recirculation: ../yami4/yami4-core/../../Debug/libyami4-core.a

../${CND_CONF}/dhw-recirculation: ../common/../Debug/libcommon.a

../${CND_CONF}/dhw-recirculation: ${OBJECTFILES}
	${MKDIR} -p ../${CND_CONF}
	${LINK.cc} -o ../${CND_CONF}/dhw-recirculation ${OBJECTFILES} ${LDLIBSOPTIONS}

${OBJECTDIR}/src/dhw-recirculation.o: src/dhw-recirculation.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} $@.d
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/src -I../yami4 -std=c++11 -MMD -MP -MF $@.d -o ${OBJECTDIR}/src/dhw-recirculation.o src/dhw-recirculation.cpp

${OBJECTDIR}/src/dhwrec-service.o: src/dhwrec-service.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} $@.d
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/src -I../yami4 -std=c++11 -MMD -MP -MF $@.d -o ${OBJECTDIR}/src/dhwrec-service.o src/dhwrec-service.cpp

# Subprojects
.build-subprojects:
	cd ../yami4/yami4-cpp && ${MAKE}  -f Makefile CONF=Debug
	cd ../yami4/yami4-core && ${MAKE}  -f Makefile CONF=Debug
	cd ../common && ${MAKE}  -f Makefile CONF=Debug
	cd ../common && ${MAKE}  -f Makefile CONF=Debug

# Clean Targets
.clean-conf: ${CLEAN_SUBPROJECTS}
	${RM} -r ${CND_BUILDDIR}/${CND_CONF}
	${RM} ../${CND_CONF}/dhw-recirculation

# Subprojects
.clean-subprojects:
	cd ../yami4/yami4-cpp && ${MAKE}  -f Makefile CONF=Debug clean
	cd ../yami4/yami4-core && ${MAKE}  -f Makefile CONF=Debug clean
	cd ../common && ${MAKE}  -f Makefile CONF=Debug clean
	cd ../common && ${MAKE}  -f Makefile CONF=Debug clean

# Enable dependency checking
.dep.inc: .depcheck-impl

include .dep.inc
