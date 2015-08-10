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
CND_CONF=Debug
CND_DISTDIR=dist
CND_BUILDDIR=build

# Include project Makefile
include Makefile

# Object Directory
OBJECTDIR=${CND_BUILDDIR}/${CND_CONF}/${CND_PLATFORM}

# Object Files
OBJECTFILES= \
	${OBJECTDIR}/src/1wire/common/crcutil.o \
	${OBJECTDIR}/src/1wire/common/findtype.o \
	${OBJECTDIR}/src/1wire/common/ioutil.o \
	${OBJECTDIR}/src/1wire/common/owerr.o \
	${OBJECTDIR}/src/1wire/common/temp10.o \
	${OBJECTDIR}/src/1wire/lib/other/libUSB/libusbds2490.o \
	${OBJECTDIR}/src/1wire/lib/other/libUSB/libusblnk.o \
	${OBJECTDIR}/src/1wire/lib/other/libUSB/libusbnet.o \
	${OBJECTDIR}/src/1wire/lib/other/libUSB/libusbses.o \
	${OBJECTDIR}/src/1wire/lib/other/libUSB/libusbtran.o \
	${OBJECTDIR}/src/input-output.o \
	${OBJECTDIR}/src/io-service.o \
	${OBJECTDIR}/src/ownetwork.o \
	${OBJECTDIR}/src/owtemp.o \
	${OBJECTDIR}/src/utils.o


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
LDLIBSOPTIONS=-L../common/yami4/lib -lusb -lboost_filesystem -lboost_program_options -lboost_system -lboost_thread -lpthread ../common/common/../bin/Debug/libcommon.a -lyamicpp -lyamicore -lPocoNet

# Build Targets
.build-conf: ${BUILD_SUBPROJECTS}
	"${MAKE}"  -f nbproject/Makefile-${CND_CONF}.mk ../bin/${CND_CONF}/input-output

../bin/${CND_CONF}/input-output: ../common/common/../bin/Debug/libcommon.a

../bin/${CND_CONF}/input-output: ${OBJECTFILES}
	${MKDIR} -p ../bin/${CND_CONF}
	${LINK.cc} -o ../bin/${CND_CONF}/input-output ${OBJECTFILES} ${LDLIBSOPTIONS}

${OBJECTDIR}/src/1wire/common/crcutil.o: src/1wire/common/crcutil.c 
	${MKDIR} -p ${OBJECTDIR}/src/1wire/common
	${RM} "$@.d"
	$(COMPILE.c) -g -Wall -DDEBUG -Isrc/1wire/common -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/1wire/common/crcutil.o src/1wire/common/crcutil.c

${OBJECTDIR}/src/1wire/common/findtype.o: src/1wire/common/findtype.c 
	${MKDIR} -p ${OBJECTDIR}/src/1wire/common
	${RM} "$@.d"
	$(COMPILE.c) -g -Wall -DDEBUG -Isrc/1wire/common -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/1wire/common/findtype.o src/1wire/common/findtype.c

${OBJECTDIR}/src/1wire/common/ioutil.o: src/1wire/common/ioutil.c 
	${MKDIR} -p ${OBJECTDIR}/src/1wire/common
	${RM} "$@.d"
	$(COMPILE.c) -g -Wall -DDEBUG -Isrc/1wire/common -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/1wire/common/ioutil.o src/1wire/common/ioutil.c

${OBJECTDIR}/src/1wire/common/owerr.o: src/1wire/common/owerr.c 
	${MKDIR} -p ${OBJECTDIR}/src/1wire/common
	${RM} "$@.d"
	$(COMPILE.c) -g -Wall -DDEBUG -Isrc/1wire/common -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/1wire/common/owerr.o src/1wire/common/owerr.c

${OBJECTDIR}/src/1wire/common/temp10.o: src/1wire/common/temp10.c 
	${MKDIR} -p ${OBJECTDIR}/src/1wire/common
	${RM} "$@.d"
	$(COMPILE.c) -g -Wall -DDEBUG -Isrc/1wire/common -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/1wire/common/temp10.o src/1wire/common/temp10.c

${OBJECTDIR}/src/1wire/lib/other/libUSB/libusbds2490.o: src/1wire/lib/other/libUSB/libusbds2490.c 
	${MKDIR} -p ${OBJECTDIR}/src/1wire/lib/other/libUSB
	${RM} "$@.d"
	$(COMPILE.c) -g -Wall -DDEBUG -Isrc/1wire/common -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/1wire/lib/other/libUSB/libusbds2490.o src/1wire/lib/other/libUSB/libusbds2490.c

${OBJECTDIR}/src/1wire/lib/other/libUSB/libusblnk.o: src/1wire/lib/other/libUSB/libusblnk.c 
	${MKDIR} -p ${OBJECTDIR}/src/1wire/lib/other/libUSB
	${RM} "$@.d"
	$(COMPILE.c) -g -Wall -DDEBUG -Isrc/1wire/common -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/1wire/lib/other/libUSB/libusblnk.o src/1wire/lib/other/libUSB/libusblnk.c

${OBJECTDIR}/src/1wire/lib/other/libUSB/libusbnet.o: src/1wire/lib/other/libUSB/libusbnet.c 
	${MKDIR} -p ${OBJECTDIR}/src/1wire/lib/other/libUSB
	${RM} "$@.d"
	$(COMPILE.c) -g -Wall -DDEBUG -Isrc/1wire/common -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/1wire/lib/other/libUSB/libusbnet.o src/1wire/lib/other/libUSB/libusbnet.c

${OBJECTDIR}/src/1wire/lib/other/libUSB/libusbses.o: src/1wire/lib/other/libUSB/libusbses.c 
	${MKDIR} -p ${OBJECTDIR}/src/1wire/lib/other/libUSB
	${RM} "$@.d"
	$(COMPILE.c) -g -Wall -DDEBUG -Isrc/1wire/common -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/1wire/lib/other/libUSB/libusbses.o src/1wire/lib/other/libUSB/libusbses.c

${OBJECTDIR}/src/1wire/lib/other/libUSB/libusbtran.o: src/1wire/lib/other/libUSB/libusbtran.c 
	${MKDIR} -p ${OBJECTDIR}/src/1wire/lib/other/libUSB
	${RM} "$@.d"
	$(COMPILE.c) -g -Wall -DDEBUG -Isrc/1wire/common -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/1wire/lib/other/libUSB/libusbtran.o src/1wire/lib/other/libUSB/libusbtran.c

${OBJECTDIR}/src/input-output.o: src/input-output.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -Isrc/1wire/common -I../common/common/src -I../common/yami4/include -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/input-output.o src/input-output.cpp

${OBJECTDIR}/src/io-service.o: src/io-service.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -Isrc/1wire/common -I../common/common/src -I../common/yami4/include -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/io-service.o src/io-service.cpp

${OBJECTDIR}/src/ownetwork.o: src/ownetwork.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -Isrc/1wire/common -I../common/common/src -I../common/yami4/include -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/ownetwork.o src/ownetwork.cpp

${OBJECTDIR}/src/owtemp.o: src/owtemp.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -Isrc/1wire/common -I../common/common/src -I../common/yami4/include -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/owtemp.o src/owtemp.cpp

${OBJECTDIR}/src/utils.o: src/utils.cpp 
	${MKDIR} -p ${OBJECTDIR}/src
	${RM} "$@.d"
	$(COMPILE.cc) -g -Wall -D_DEBUG -Isrc/1wire/common -I../common/common/src -I../common/yami4/include -std=c++11 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/src/utils.o src/utils.cpp

# Subprojects
.build-subprojects:
	cd ../common/common && ${MAKE}  -f Makefile CONF=Debug

# Clean Targets
.clean-conf: ${CLEAN_SUBPROJECTS}
	${RM} -r ${CND_BUILDDIR}/${CND_CONF}
	${RM} ../bin/${CND_CONF}/input-output

# Subprojects
.clean-subprojects:
	cd ../common/common && ${MAKE}  -f Makefile CONF=Debug clean

# Enable dependency checking
.dep.inc: .depcheck-impl

include .dep.inc
