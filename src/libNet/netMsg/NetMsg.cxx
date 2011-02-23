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

#include <arpa/inet.h> /* ntohs */

#include "NetMsg.h"
#include "Portal.h"
#include "DataStreamUtils.h"
#include "GSUuid.h"

#include <sstream>

/* Normal Constructor */
NetMsg::NetMsg(uint16_t mType) :
    msgType(mType), hasReUUID(false), reUUID(NULL)
{
    msgUUID = GSUuid::createUuid();
}

/* Reply Constructor */
NetMsg::NetMsg(uint16_t mType, NetMsg* msg) :
    msgType(mType)
{
    if (msg->getMsgUUID() != NULL) {
	this->reUUID = new GSUuid(msg->getMsgUUID());
	this->hasReUUID = true;
    } else {
	this->reUUID = NULL;
	this->hasReUUID = false;
    }
    msgUUID = GSUuid::createUuid();
}

/* Deserializing Constructor */
NetMsg::NetMsg(DataStream* ds, Portal* origin)
{
    this->origin = origin;
    this->msgType = ntohs(*(uint16_t*)ds->get(2));
    this->msgUUID = DataStreamUtils::getGSUuid(ds);
    this->hasReUUID = *(unsigned char*)ds->get(1);
    if (this->hasReUUID)
	this->reUUID = DataStreamUtils::getGSUuid(ds);
}

/* Destructor */
NetMsg::~NetMsg()
{}

/* Serializers */
    ByteArray*
NetMsg::serialize()
{
    ByteArray* ba = new ByteArray();
    this->serialize(ba);
    return ba;
}

    void
NetMsg::serialize(ByteArray* ba)
{
    uint16_t mt;
    /* Make a DS for the subclass */
    DataStream subDS(ba->data(), ba->size());

    /* Serialize Header */
    mt = htons(this->msgType);
    std::cout << "Erm, serialize? " << this->msgType << ":" << this->msgUUID << std::endl;
    subDS.append((const char *)&mt, 2);
    DataStreamUtils::putGSUuid(&subDS, this->msgUUID);
    subDS.append((const char *)&this->hasReUUID, 1);

    if (this->hasReUUID) {
	DataStreamUtils::putGSUuid(&subDS, this->reUUID);
    }

    /* Call subclass serialize */
    if (!this->_serialize(&subDS)) {
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
GSUuid*
NetMsg::getMsgUUID() const
{
    return this->msgUUID;
}
bool
NetMsg::msgHasReUUID() const
{
    return this->hasReUUID;
}
GSUuid*
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

	if (this->getMsgUUID()->equals(other.getMsgUUID()))
	    return false;

	if (this->msgHasReUUID() == other.msgHasReUUID())
	    return false;

	if (this->msgHasReUUID())
	    if (this->getReUUID()->equals(other.getReUUID()))
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
	snprintf(buf, BUFSIZ, "'\t msgUUID: %s'", this->msgUUID->toString()->c_str());
	out.append(buf);
    }

    snprintf(buf, BUFSIZ, "'\t hasReUUID: %d'", this->hasReUUID);
    out.append(buf);

    if (this->reUUID != NULL) {
	snprintf(buf, BUFSIZ, "'\t reUUID: %s'", this->reUUID->toString()->c_str());
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
