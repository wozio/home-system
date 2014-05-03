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
CND_PLATFORM=GNU_4.7.3-Linux-x86
CND_DLIB_EXT=so
CND_CONF=Debug_praca
CND_DISTDIR=dist
CND_BUILDDIR=build

# Include project Makefile
include Makefile

# Object Directory
OBJECTDIR=${CND_BUILDDIR}/${CND_CONF}/${CND_PLATFORM}

# Object Files
OBJECTFILES= \
	${OBJECTDIR}/src/channel.o \
	${OBJECTDIR}/src/channels.o \
	${OBJECTDIR}/src/demux.o \
	${OBJECTDIR}/src/dvb-service.o \
	${OBJECTDIR}/src/dvb.o \
	${OBJECTDIR}/src/file-reader.o \
	${OBJECTDIR}/src/frontend.o \
	${OBJECTDIR}/src/main.o \
	${OBJECTDIR}/src/param_convert.o \
	${OBJECTDIR}/src/session.o \
	${OBJECTDIR}/src/transponder.o \
	${OBJECTDIR}/src/transponder_dvbs.o \
	${OBJECTDIR}/src/transponder_dvbs2.o \
	${OBJECTDIR}/src/transponder_dvbt.o \
	${OBJECTDIR}/src/transponders.o


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
LDLIBSOPTIONS=-ldvbapi -ldvbcfg -ldvbsec -lucsi -lPocoFoundation -lboost_date_time -lboost_program_options -lboost_system -lboost_thread ../yami4/yami4-cpp/../../Debug_praca/libyami4-cpp.a ../yami4/yami4-core/../../Debug_praca/libyami4-core.a ../common/../Debug_praca/libcommon.a -lPocoNet

# Build Targets
.build-conf: ${BUILD_SUBPROJECTS}
	"${MAKE}"  -f nbproject/Makefile-${CND_CONF}.mk ../${CND_CONF}/dvb-source

../${CND_CONF}/dvb-source: ../yami4/yami4-cpp/../../Debug_praca/libyami4-cpp.a

../${CND_CONF}/dvb-source: ../yami4/yami4-core/../../Debug_praca/libyami4-core.a

../${CND_CONF}/dvb-source: ../common/../Debug_praca/libcommon.a

../${CND_CONF}/dvb-source: ${OBJECTFILES}
	${MKDIR} -p ../${CND_CONF}
	${LINK.cc} -o ../${CND_CONF}/dvb-source ${OBJECTFILES} ${LDLIBSOPTIONS}

${OBJECTDIR}/src/channel.o: src/channel.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/src -I../yami4 -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/channel.o src/channel.cpp

${OBJECTDIR}/src/channels.o: src/channels.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/src -I../yami4 -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/channels.o src/channels.cpp

${OBJECTDIR}/src/demux.o: src/demux.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/src -I../yami4 -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/demux.o src/demux.cpp

${OBJECTDIR}/src/dvb-service.o: src/dvb-service.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/src -I../yami4 -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/dvb-service.o src/dvb-service.cpp

${OBJECTDIR}/src/dvb.o: src/dvb.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/src -I../yami4 -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/dvb.o src/dvb.cpp

${OBJECTDIR}/src/file-reader.o: src/file-reader.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/src -I../yami4 -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/file-reader.o src/file-reader.cpp

${OBJECTDIR}/src/frontend.o: src/frontend.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/src -I../yami4 -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/frontend.o src/frontend.cpp

${OBJECTDIR}/src/main.o: src/main.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/src -I../yami4 -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/main.o src/main.cpp

${OBJECTDIR}/src/param_convert.o: src/param_convert.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/src -I../yami4 -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/param_convert.o src/param_convert.cpp

${OBJECTDIR}/src/session.o: src/session.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/src -I../yami4 -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/session.o src/session.cpp

${OBJECTDIR}/src/transponder.o: src/transponder.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/src -I../yami4 -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/transponder.o src/transponder.cpp

${OBJECTDIR}/src/transponder_dvbs.o: src/transponder_dvbs.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -D_GLIBCXX_USE_NANOSLEEP -I../common/src -I../yami4 -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/transponder_dvbs.o src/transponder_dvbs.cpp

${OBJECTDIR}/src/transponder_dvbs2.o: src/transponder_dvbs2.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/src -I../yami4 -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/transponder_dvbs2.o src/transponder_dvbs2.cpp

${OBJECTDIR}/src/transponder_dvbt.o: src/transponder_dvbt.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/src -I../yami4 -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/transponder_dvbt.o src/transponder_dvbt.cpp

${OBJECTDIR}/src/transponders.o: src/transponders.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -I../common/src -I../yami4 -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/transponders.o src/transponders.cpp

# Subprojects
.build-subprojects:
	cd ../common && ${MAKE}  -f Makefile CONF=Debug_praca
	cd ../common && ${MAKE}  -f Makefile CONF=Debug_praca

# Clean Targets
.clean-conf: ${CLEAN_SUBPROJECTS}
	${RM} -r ${CND_BUILDDIR}/${CND_CONF}
	${RM} ../${CND_CONF}/dvb-source

# Subprojects
.clean-subprojects:
	cd ../common && ${MAKE}  -f Makefile CONF=Debug_praca clean
	cd ../common && ${MAKE}  -f Makefile CONF=Debug_praca clean

# Enable dependency checking
.dep.inc: .depcheck-impl

include .dep.inc
