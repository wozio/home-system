#include "pch.h"
#include "binary_connection.h"

namespace home_system
{

binary_connection::binary_connection(ws_t ws)
  : handler(ws, true)
{

}

binary_connection::~binary_connection()
{

}

void binary_connection::on_read(data_t data, size_t data_size)
{

}

}