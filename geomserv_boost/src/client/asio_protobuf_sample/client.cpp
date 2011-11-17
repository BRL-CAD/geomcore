//
// client.cpp - Boost ASIO + Protobuf C++ test client
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#include <cstdlib>
#include <cstring>
#include <iostream>
#include <boost/asio.hpp>
#include <boost/shared_ptr.hpp>
#include "stringdb.pb.h"
#include "packedmessage.h"

using boost::asio::ip::tcp;
using boost::uint8_t;

typedef boost::shared_ptr<stringdb::Request> RequestPointer;
typedef boost::shared_ptr<stringdb::Response> ResponsePointer;

int main(int argc, char* argv[])
{
  try
  {
    boost::asio::io_service io_service;
    tcp::resolver resolver(io_service);

    tcp::resolver::query query(tcp::v4(), "127.0.0.1", "5309");
    tcp::resolver::iterator iterator = resolver.resolve(query);

    tcp::socket s(io_service);
    boost::asio::connect(s, iterator);


    RequestPointer request(new stringdb::Request);
    request->set_type(stringdb::Request::GET_VALUE);

    std::vector<uint8_t> writebuf;
    PackedMessage<stringdb::Request> request_msg(request);
    request_msg.pack(writebuf);
    
    boost::asio::write(s, boost::asio::buffer(writebuf));

    std::vector<uint8_t> m_readbuf;
    m_readbuf.resize(4);
    boost::asio::read(s, boost::asio::buffer(m_readbuf));
    PackedMessage<stringdb::Response> packed_reply;
    unsigned msg_len = packed_reply.decode_header(m_readbuf);
    m_readbuf.resize(4 + msg_len);
    boost::asio::mutable_buffers_1 buf = boost::asio::buffer(&m_readbuf[HEADER_SIZE], msg_len);
    boost::asio::read(s, buf);
    packed_reply.unpack(m_readbuf);
    ResponsePointer reply = packed_reply.get_msg();
    
  }
  catch (std::exception& e)
  {
    std::cerr << "Exception: " << e.what() << "\n";
  }

  return 0;
}
