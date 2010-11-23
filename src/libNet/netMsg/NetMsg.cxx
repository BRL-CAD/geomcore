/*                      N E T M S G . C X X
 * BRL-CAD
 *
 * Copyright (c) 2010 United States Government as represented by
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
#include <sstream>

/* Normal Constructor */
NetMsg::NetMsg(quint16 mType) :
  msgType(mType), hasReUUID(false), reUUID(NULL)
{
  msgUUID = QUuid::createUuid();
}

/* Reply Constructor */
NetMsg::NetMsg(quint16 mType, NetMsg* msg) :
  msgType(mType)
{
  if (msg->getMsgUUID() != NULL)
    {
      QString strUUID = msg->getMsgUUID().toString();
      QUuid uuid(strUUID);
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
  this->msgUUID = *Utils::getQUuid(ds);
  *ds >> this->hasReUUID;
  if (this->hasReUUID)
    {
      this->reUUID = *Utils::getQUuid(ds);
    }
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
  Utils::putQUuid(&subDS, this->msgUUID);
  subDS << this->hasReUUID;

  if (this->hasReUUID)
    {
      Utils::putQUuid(&subDS, this->reUUID);
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
quint16
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

bool
NetMsg::equals(const NetMsg& other)
{
  if (this->getMsgType() != other.getMsgType())
    {
      return false;
    }

  if (this->getMsgUUID() != other.getMsgUUID())
    {
      return false;
    }

  if (this->msgHasReUUID() != other.msgHasReUUID())
    {
      return false;
    }

  if (this->msgHasReUUID())
    {
      if (this->getReUUID() != other.getReUUID())
        {
          return false;
        }
    }

  return this->_equals(other);
}

QString
NetMsg::toString()
{
  QString out;

  out += "msgType: '" + QString::number(this->msgType);

  if (this->msgUUID != NULL)
    {
      out += "'\t msgUUID: '" + this->msgUUID.toString();
    }

  out += "'\t hasReUUID: '" + QString::number(this->hasReUUID);

  if (this->reUUID != NULL)
    {
      out += "'\t reUUID: '" + this->reUUID.toString();
    }

  out += "'";

  return out;
}

std::string
NetMsg::toStdString()
{
  return this->toString().toStdString();
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
