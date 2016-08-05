#ifndef JSON_CONVERTER_H
#define	JSON_CONVERTER_H

#include "handler.h"
#include "msg_type_t.h"
#include <yami4-cpp/parameters.h>
#include <string>
#include <exception>

namespace home_system
{

/**
 * Extracts from json service, message and yami::parameters
 * @param data Input null terminated JSON string
 * @param source Source
 * @param target Target
 * @param message Message
 * @param sequence_number Sequence number of the message
 * @param params Output yami parameters
 */
  msg_type_t from_json(data_t data, std::string& source, std::string& target, std::string& message,
  long long& sequence_number, yami::parameters& params);

/**
 * Convert YAMI reply into JSON string
 * @param target Target
 * @param result Result
 * @param reason Reason of failed result
 * @param sequence_number Sequence number of the message
 * @param params YAMI parameters
 * @param buffer Output buffer with JSON string
 */
void reply_to_json(const std::string& target, const std::string& result, const std::string& reason,
        long long sequence_number, const yami::parameters& params,
        buffer_t buffer);

/**
 * Convert YAMI reply without parameters into JSON string
 * @param target Target
 * @param result Result
 * @param reason Reason of failed result
 * @param sequence_number Sequence number of the message
 * @param buffer Output buffer with JSON string
 */
void reply_to_json(const std::string& target, const std::string& result, const std::string& reason,
        long long sequence_number,
        buffer_t buffer);

/**
 * Convert YAMI message into JSON string
 * @param target Target
 * @param message Message
 * @param sequence_number Sequence number of the message
 * @param params YAMI parameters
 * @param buffer Output buffer with JSON string
 */
void msg_to_json(const std::string& target, const std::string& message,
        long long sequence_number, const yami::parameters& params,
        buffer_t buffer);

/**
 * Convert YAMI message into JSON string
 * @param target Target
 * @param message Message
 * @param params YAMI parameters
 * @param buffer Output buffer with JSON string
 */
void msg_to_json(const std::string& target, const std::string& message,
        const yami::parameters& params,
        buffer_t buffer);

}

#endif	/* JSON_CONVERTER_H */

