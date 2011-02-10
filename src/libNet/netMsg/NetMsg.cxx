/*                      N E T M S G . C X X
 * BRL-CAD
 *
 * Copyright (c) 2011 United States Government as represented by
 * the U.S. Army Research Laboratory.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this file; see the file named COPYING for more
 * information.
 */
/** @file NetMsg.cxx
 *
 * Brief description
 *
 */

#include "NetMsg.h"
#include "Portal.h"
#include "DataStreamUtils.h"
#include <sstream>

/* Normal Constructor */
NetMsg::NetMsg(uint16_t mType) :
  msgType(mType), hasReUUID(false), reUUID(NULL)
{
  msgUUID = QUuid::createUuid();
}

/* Reply Constructor */
NetMsg::NetMsg(uint16_t mType, NetMsg* msg) :
  msgType(mType)
{
  if (msg->getMsgUUID() != NULL)
    {
      QUuid uuid(msg->getMsgUUID().toString());
      this->reUUID = uuid;
      this->hasReUUID = true;
    }
  else
    {
      this->reUUID = NULL;
      this->hasReUUID = false;
    }
  msgUUID = QUuid::createUuid();
}

/* Deserializing Constructor */
NetMsg::NetMsg(QDataStream* ds, Portal* origin)
{
  this->origin = origin;
  *ds >> this->msgType;
  this->msgUUID = *DataStreamUtils::getQUuid(ds);
  *ds >> this->hasReUUID;
  if (this->hasReUUID)
      this->reUUID = *DataStreamUtils::getQUuid(ds);
}

/* Destructor */
NetMsg::~NetMsg()
{}

/* Serializers */
QByteArray*
NetMsg::serialize()
{
  QByteArray* ba = new QByteArray();
  this->serialize(ba);
  return ba;
}

void
NetMsg::serialize(QByteArray* ba)
{
  /* Make a DS for the subclass */
  QDataStream subDS(ba, QIODevice::ReadWrite);

  /* Serialize Header */
  subDS << this->msgType;
  DataStreamUtils::putQUuid(&subDS, this->msgUUID);
  subDS << this->hasReUUID;

  if (this->hasReUUID)
    {
      DataStreamUtils::putQUuid(&subDS, this->reUUID);
    }

  /* Call subclass serialize */
  if (!this->_serialize(&subDS))
    {
      std::cerr << "A serialization Error in NetMsg::serialize() occurred.\n";
      return;
    }
}

/*
 *Getters n Setters
 */
uint16_t
NetMsg::getMsgType() const
{
  return this->msgType;
}
QUuid
NetMsg::getMsgUUID() const
{
  return this->msgUUID;
}
bool
NetMsg::msgHasReUUID() const
{
  return this->hasReUUID;
}
QUuid
NetMsg::getReUUID() const
{
  return this->reUUID;
}

Portal*
NetMsg::getOrigin() const
{
	return this->origin;
}

/*
 * Utilities
 */

bool
NetMsg::operator==(const NetMsg& other)
{
  return this->equals(other);
}

bool NetMsg::equals(const NetMsg& other) {
	if (this->getMsgType() != other.getMsgType())
		return false;

	if (this->getMsgUUID() != other.getMsgUUID())
		return false;

	if (this->msgHasReUUID() != other.msgHasReUUID())
		return false;

	if (this->msgHasReUUID())
		if (this->getReUUID() != other.getReUUID())
			return false;

	return this->_equals(other);
}

std::string NetMsg::toString()
{
	char buf[BUFSIZ];
	std::string out;

	snprintf(buf, BUFSIZ, "msgType: %d'", this->msgType);
	out.assign(buf);

	if (this->msgUUID != NULL) {
		snprintf(buf, BUFSIZ, "'\t msgUUID: %s'", this->msgUUID.toString().toStdString().c_str());
		out.append(buf);
	}

	snprintf(buf, BUFSIZ, "'\t hasReUUID: %d'", this->hasReUUID);
	out.append(buf);

	if (this->reUUID != NULL) {
		snprintf(buf, BUFSIZ, "'\t reUUID: %s'", this->reUUID.toString().toStdString().c_str());
		out.append(buf);
	}

	out.append("'");

	return out;
}

std::string
NetMsg::toStdString()
{
  return this->toString();
}

void
NetMsg::printMe()
{
  std::cout << this->toStdString();
}

/*
 * Local Variables:
 * mode: C
 * tab-width: 8
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
