// Copyright Maciej Sobczak 2008-2015.
// This file is part of YAMI4.
//
// YAMI4 is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// YAMI4 is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with YAMI4.  If not, see <http://www.gnu.org/licenses/>.

#ifndef YAMI4IDL_NAME_UTILS_H
#define YAMI4IDL_NAME_UTILS_H

#include <string>

namespace name_utils
{

std::string package_to_file_name(const std::string & package_name);

void verify_name_is_simple(const std::string & name);

bool name_is_qualified(const std::string & name);

std::string first_component(const std::string & s, bool leave_if_single = false);

std::string trim_first_component(const std::string & s, bool leave_if_single = false);

std::string last_component(const std::string & s, bool leave_if_single = false);

std::string trim_last_component(const std::string & s, bool leave_if_single = false);

} // namespace name_utils

#endif // YAMI4IDL_NAME_UTILS_H