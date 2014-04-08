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
	${OBJECTDIR}/agent.o \
	${OBJECTDIR}/allocator.o \
	${OBJECTDIR}/c_interface.o \
	${OBJECTDIR}/channel.o \
	${OBJECTDIR}/channel_group.o \
	${OBJECTDIR}/channel_holder.o \
	${OBJECTDIR}/fatal_errors.o \
	${OBJECTDIR}/listener.o \
	${OBJECTDIR}/network_utils.o \
	${OBJECTDIR}/options.o \
	${OBJECTDIR}/parameter_entry.o \
	${OBJECTDIR}/parameter_iterator.o \
	${OBJECTDIR}/parameters-details.o \
	${OBJECTDIR}/parameters.o \
	${OBJECTDIR}/posix/channel.o \
	${OBJECTDIR}/posix/io_error_handler.o \
	${OBJECTDIR}/posix/listener.o \
	${OBJECTDIR}/posix/mutex.o \
	${OBJECTDIR}/posix/network_utils.o \
	${OBJECTDIR}/posix/selector.o \
	${OBJECTDIR}/raw_buffer_data_source.o \
	${OBJECTDIR}/serialization.o


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
LDLIBSOPTIONS=

# Build Targets
.build-conf: ${BUILD_SUBPROJECTS}
	"${MAKE}"  -f nbproject/Makefile-${CND_CONF}.mk ../../${CND_CONF}/libyami4-core.a

../../${CND_CONF}/libyami4-core.a: ${OBJECTFILES}
	${MKDIR} -p ../../${CND_CONF}
	${RM} ../../${CND_CONF}/libyami4-core.a
	${AR} -rv ../../${CND_CONF}/libyami4-core.a ${OBJECTFILES} 
	$(RANLIB) ../../${CND_CONF}/libyami4-core.a

${OBJECTDIR}/agent.o: agent.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -Iposix -MMD -MP -MF $@.d -o ${OBJECTDIR}/agent.o agent.cpp

${OBJECTDIR}/allocator.o: allocator.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -Iposix -MMD -MP -MF $@.d -o ${OBJECTDIR}/allocator.o allocator.cpp

${OBJECTDIR}/c_interface.o: c_interface.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -Iposix -MMD -MP -MF $@.d -o ${OBJECTDIR}/c_interface.o c_interface.cpp

${OBJECTDIR}/channel.o: channel.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -Iposix -MMD -MP -MF $@.d -o ${OBJECTDIR}/channel.o channel.cpp

${OBJECTDIR}/channel_group.o: channel_group.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -Iposix -MMD -MP -MF $@.d -o ${OBJECTDIR}/channel_group.o channel_group.cpp

${OBJECTDIR}/channel_holder.o: channel_holder.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -Iposix -MMD -MP -MF $@.d -o ${OBJECTDIR}/channel_holder.o channel_holder.cpp

${OBJECTDIR}/fatal_errors.o: fatal_errors.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -Iposix -MMD -MP -MF $@.d -o ${OBJECTDIR}/fatal_errors.o fatal_errors.cpp

${OBJECTDIR}/listener.o: listener.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -Iposix -MMD -MP -MF $@.d -o ${OBJECTDIR}/listener.o listener.cpp

${OBJECTDIR}/network_utils.o: network_utils.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -Iposix -MMD -MP -MF $@.d -o ${OBJECTDIR}/network_utils.o network_utils.cpp

${OBJECTDIR}/options.o: options.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -Iposix -MMD -MP -MF $@.d -o ${OBJECTDIR}/options.o options.cpp

${OBJECTDIR}/parameter_entry.o: parameter_entry.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -Iposix -MMD -MP -MF $@.d -o ${OBJECTDIR}/parameter_entry.o parameter_entry.cpp

${OBJECTDIR}/parameter_iterator.o: parameter_iterator.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -Iposix -MMD -MP -MF $@.d -o ${OBJECTDIR}/parameter_iterator.o parameter_iterator.cpp

${OBJECTDIR}/parameters-details.o: parameters-details.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -Iposix -MMD -MP -MF $@.d -o ${OBJECTDIR}/parameters-details.o parameters-details.cpp

${OBJECTDIR}/parameters.o: parameters.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -Iposix -MMD -MP -MF $@.d -o ${OBJECTDIR}/parameters.o parameters.cpp

${OBJECTDIR}/posix/channel.o: posix/channel.cpp 
	${MKDIR} -p ${OBJECTDIR}/posix
	${RM} $@.d
	$(COMPILE.cc) -g -Iposix -MMD -MP -MF $@.d -o ${OBJECTDIR}/posix/channel.o posix/channel.cpp

${OBJECTDIR}/posix/io_error_handler.o: posix/io_error_handler.cpp 
	${MKDIR} -p ${OBJECTDIR}/posix
	${RM} $@.d
	$(COMPILE.cc) -g -Iposix -MMD -MP -MF $@.d -o ${OBJECTDIR}/posix/io_error_handler.o posix/io_error_handler.cpp

${OBJECTDIR}/posix/listener.o: posix/listener.cpp 
	${MKDIR} -p ${OBJECTDIR}/posix
	${RM} $@.d
	$(COMPILE.cc) -g -Iposix -MMD -MP -MF $@.d -o ${OBJECTDIR}/posix/listener.o posix/listener.cpp

${OBJECTDIR}/posix/mutex.o: posix/mutex.cpp 
	${MKDIR} -p ${OBJECTDIR}/posix
	${RM} $@.d
	$(COMPILE.cc) -g -Iposix -MMD -MP -MF $@.d -o ${OBJECTDIR}/posix/mutex.o posix/mutex.cpp

${OBJECTDIR}/posix/network_utils.o: posix/network_utils.cpp 
	${MKDIR} -p ${OBJECTDIR}/posix
	${RM} $@.d
	$(COMPILE.cc) -g -Iposix -MMD -MP -MF $@.d -o ${OBJECTDIR}/posix/network_utils.o posix/network_utils.cpp

${OBJECTDIR}/posix/selector.o: posix/selector.cpp 
	${MKDIR} -p ${OBJECTDIR}/posix
	${RM} $@.d
	$(COMPILE.cc) -g -Iposix -MMD -MP -MF $@.d -o ${OBJECTDIR}/posix/selector.o posix/selector.cpp

${OBJECTDIR}/raw_buffer_data_source.o: raw_buffer_data_source.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -Iposix -MMD -MP -MF $@.d -o ${OBJECTDIR}/raw_buffer_data_source.o raw_buffer_data_source.cpp

${OBJECTDIR}/serialization.o: serialization.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -Iposix -MMD -MP -MF $@.d -o ${OBJECTDIR}/serialization.o serialization.cpp

# Subprojects
.build-subprojects:

# Clean Targets
.clean-conf: ${CLEAN_SUBPROJECTS}
	${RM} -r ${CND_BUILDDIR}/${CND_CONF}
	${RM} ../../${CND_CONF}/libyami4-core.a

# Subprojects
.clean-subprojects:

# Enable dependency checking
.dep.inc: .depcheck-impl

include .dep.inc
