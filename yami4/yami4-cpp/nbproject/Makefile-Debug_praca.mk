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
CND_CONF=Debug_praca
CND_DISTDIR=dist
CND_BUILDDIR=build

# Include project Makefile
include Makefile

# Object Directory
OBJECTDIR=${CND_BUILDDIR}/${CND_CONF}/${CND_PLATFORM}

# Object Files
OBJECTFILES= \
	${OBJECTDIR}/activity_statistics_monitor.o \
	${OBJECTDIR}/agent.o \
	${OBJECTDIR}/agent_impl.o \
	${OBJECTDIR}/details.o \
	${OBJECTDIR}/id_generator.o \
	${OBJECTDIR}/incoming_message.o \
	${OBJECTDIR}/incoming_message_queue.o \
	${OBJECTDIR}/name_resolver.o \
	${OBJECTDIR}/options.o \
	${OBJECTDIR}/outgoing_message.o \
	${OBJECTDIR}/outgoing_message_info.o \
	${OBJECTDIR}/outgoing_message_manager.o \
	${OBJECTDIR}/parameter_entry.o \
	${OBJECTDIR}/parameters.o \
	${OBJECTDIR}/posix/delay.o \
	${OBJECTDIR}/posix/flag.o \
	${OBJECTDIR}/posix/semaphore.o \
	${OBJECTDIR}/posix/start_thread.o \
	${OBJECTDIR}/raw_buffer_data_source.o \
	${OBJECTDIR}/value_publisher.o \
	${OBJECTDIR}/value_publisher_impl.o \
	${OBJECTDIR}/version.o \
	${OBJECTDIR}/water_flow_manager.o


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
	"${MAKE}"  -f nbproject/Makefile-${CND_CONF}.mk ../../${CND_CONF}/libyami4-cpp.a

../../${CND_CONF}/libyami4-cpp.a: ${OBJECTFILES}
	${MKDIR} -p ../../${CND_CONF}
	${RM} ../../${CND_CONF}/libyami4-cpp.a
	${AR} -rv ../../${CND_CONF}/libyami4-cpp.a ${OBJECTFILES} 
	$(RANLIB) ../../${CND_CONF}/libyami4-cpp.a

${OBJECTDIR}/activity_statistics_monitor.o: activity_statistics_monitor.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -I.. -Iposix -I../yami4-core/posix -MMD -MP -MF $@.d -o ${OBJECTDIR}/activity_statistics_monitor.o activity_statistics_monitor.cpp

${OBJECTDIR}/agent.o: agent.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -I.. -Iposix -I../yami4-core/posix -MMD -MP -MF $@.d -o ${OBJECTDIR}/agent.o agent.cpp

${OBJECTDIR}/agent_impl.o: agent_impl.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -I.. -Iposix -I../yami4-core/posix -MMD -MP -MF $@.d -o ${OBJECTDIR}/agent_impl.o agent_impl.cpp

${OBJECTDIR}/details.o: details.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -I.. -Iposix -I../yami4-core/posix -MMD -MP -MF $@.d -o ${OBJECTDIR}/details.o details.cpp

${OBJECTDIR}/id_generator.o: id_generator.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -I.. -Iposix -I../yami4-core/posix -MMD -MP -MF $@.d -o ${OBJECTDIR}/id_generator.o id_generator.cpp

${OBJECTDIR}/incoming_message.o: incoming_message.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -I.. -Iposix -I../yami4-core/posix -MMD -MP -MF $@.d -o ${OBJECTDIR}/incoming_message.o incoming_message.cpp

${OBJECTDIR}/incoming_message_queue.o: incoming_message_queue.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -I.. -Iposix -I../yami4-core/posix -MMD -MP -MF $@.d -o ${OBJECTDIR}/incoming_message_queue.o incoming_message_queue.cpp

${OBJECTDIR}/name_resolver.o: name_resolver.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -I.. -Iposix -I../yami4-core/posix -MMD -MP -MF $@.d -o ${OBJECTDIR}/name_resolver.o name_resolver.cpp

${OBJECTDIR}/options.o: options.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -I.. -Iposix -I../yami4-core/posix -MMD -MP -MF $@.d -o ${OBJECTDIR}/options.o options.cpp

${OBJECTDIR}/outgoing_message.o: outgoing_message.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -I.. -Iposix -I../yami4-core/posix -MMD -MP -MF $@.d -o ${OBJECTDIR}/outgoing_message.o outgoing_message.cpp

${OBJECTDIR}/outgoing_message_info.o: outgoing_message_info.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -I.. -Iposix -I../yami4-core/posix -MMD -MP -MF $@.d -o ${OBJECTDIR}/outgoing_message_info.o outgoing_message_info.cpp

${OBJECTDIR}/outgoing_message_manager.o: outgoing_message_manager.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -I.. -Iposix -I../yami4-core/posix -MMD -MP -MF $@.d -o ${OBJECTDIR}/outgoing_message_manager.o outgoing_message_manager.cpp

${OBJECTDIR}/parameter_entry.o: parameter_entry.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -I.. -Iposix -I../yami4-core/posix -MMD -MP -MF $@.d -o ${OBJECTDIR}/parameter_entry.o parameter_entry.cpp

${OBJECTDIR}/parameters.o: parameters.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -I.. -Iposix -I../yami4-core/posix -MMD -MP -MF $@.d -o ${OBJECTDIR}/parameters.o parameters.cpp

${OBJECTDIR}/posix/delay.o: posix/delay.cpp 
	${MKDIR} -p ${OBJECTDIR}/posix
	${RM} $@.d
	$(COMPILE.cc) -g -I.. -Iposix -I../yami4-core/posix -MMD -MP -MF $@.d -o ${OBJECTDIR}/posix/delay.o posix/delay.cpp

${OBJECTDIR}/posix/flag.o: posix/flag.cpp 
	${MKDIR} -p ${OBJECTDIR}/posix
	${RM} $@.d
	$(COMPILE.cc) -g -I.. -Iposix -I../yami4-core/posix -MMD -MP -MF $@.d -o ${OBJECTDIR}/posix/flag.o posix/flag.cpp

${OBJECTDIR}/posix/semaphore.o: posix/semaphore.cpp 
	${MKDIR} -p ${OBJECTDIR}/posix
	${RM} $@.d
	$(COMPILE.cc) -g -I.. -Iposix -I../yami4-core/posix -MMD -MP -MF $@.d -o ${OBJECTDIR}/posix/semaphore.o posix/semaphore.cpp

${OBJECTDIR}/posix/start_thread.o: posix/start_thread.cpp 
	${MKDIR} -p ${OBJECTDIR}/posix
	${RM} $@.d
	$(COMPILE.cc) -g -I.. -Iposix -I../yami4-core/posix -MMD -MP -MF $@.d -o ${OBJECTDIR}/posix/start_thread.o posix/start_thread.cpp

${OBJECTDIR}/raw_buffer_data_source.o: raw_buffer_data_source.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -I.. -Iposix -I../yami4-core/posix -MMD -MP -MF $@.d -o ${OBJECTDIR}/raw_buffer_data_source.o raw_buffer_data_source.cpp

${OBJECTDIR}/value_publisher.o: value_publisher.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -I.. -Iposix -I../yami4-core/posix -MMD -MP -MF $@.d -o ${OBJECTDIR}/value_publisher.o value_publisher.cpp

${OBJECTDIR}/value_publisher_impl.o: value_publisher_impl.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -I.. -Iposix -I../yami4-core/posix -MMD -MP -MF $@.d -o ${OBJECTDIR}/value_publisher_impl.o value_publisher_impl.cpp

${OBJECTDIR}/version.o: version.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -I.. -Iposix -I../yami4-core/posix -MMD -MP -MF $@.d -o ${OBJECTDIR}/version.o version.cpp

${OBJECTDIR}/water_flow_manager.o: water_flow_manager.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -I.. -Iposix -I../yami4-core/posix -MMD -MP -MF $@.d -o ${OBJECTDIR}/water_flow_manager.o water_flow_manager.cpp

# Subprojects
.build-subprojects:

# Clean Targets
.clean-conf: ${CLEAN_SUBPROJECTS}
	${RM} -r ${CND_BUILDDIR}/${CND_CONF}
	${RM} ../../${CND_CONF}/libyami4-cpp.a

# Subprojects
.clean-subprojects:

# Enable dependency checking
.dep.inc: .depcheck-impl

include .dep.inc
