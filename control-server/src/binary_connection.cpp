#include "pch.h"
#include "binary_connection.h"

namespace home_system
{

binary_connection::binary_connection(ws_t ws, const std::string& name)
: handler(ws, true),
  agent_([]()->yami::parameters {
    yami::parameters p;
    p.set_boolean("deliver_as_raw_binary", true);
    return p;
  }())
{
  Poco::Net::NetworkInterface::NetworkInterfaceList il = Poco::Net::NetworkInterface::list();
  std::string ip;
  for (size_t i = 0; i < il.size(); ++i)
  {
    if (!il[i].address().isLoopback())
      if (il[i].address().family() == Poco::Net::IPAddress::Family::IPv4)
        ip = il[i].address().toString();
  }

  std::string ep("tcp://");
  ep.append(ip).append(":*");
  endpoint_ = agent_.add_listener(ep);
  LOGH(DEBUG) << "Binary YAMI listener added: " << endpoint_;

  agent_.register_object(name, *this);
}

binary_connection::~binary_connection()
{
}

void binary_connection::operator()(yami::incoming_message & im)
{
  auto indata = im.get_raw_content();
  //LOGH(DEBUG) << "received " << indata.size() << " bytes";
  auto outdata = create_data();

  memcpy(outdata->data(), &indata[0], indata.size());

  on_send(outdata, indata.size(), BINARY);
}

void binary_connection::on_read(data_t data, size_t data_size, type_t data_type)
{
  if (data_type == BINARY)
  {
    LOGH(DEBUG) << "Received " << data_size << " bytes";
  }
  else
  {
    LOGH(DEBUG) << "Text data received from WS...";
  }
}

const std::string& binary_connection::get_endpoint()
{
  return endpoint_;
}

}