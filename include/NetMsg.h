/*                        N E T M S G . H
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
/** @file NetMsg.h
 *
 * Brief description
 *
 */

#ifndef __NETMSG_H__
#define __NETMSG_H__

#include "IOException.h"
#include "libutility.h"
#include <QDataStream>
#include <QByteArray>
#include <QUuid>
#include <sstream>

class NetMsg
{

public:
	/* Normal Constructor */
	NetMsg(quint32 mType);

	/* Reply Constructor */
	NetMsg(quint32 mType, NetMsg* msg);

	/* Deserializing Constructor */
	NetMsg(QDataStream* ds, QString origin);

	/* Destructor */
	virtual ~NetMsg();

	/* Serializers */
	QByteArray* serialize();
	void serialize(QByteArray* ba);

	/*
	 *Getters n Setters
	 */
	quint32 getMsgLen() const;
	quint32 getMsgType() const;
	QUuid getMsgUUID() const;
	bool msgHasReUUID() const;
	QUuid getReUUID() const;

	/*
	 * Utilities
	 */
	virtual QString toString();
	virtual std::string toStdString();
	virtual bool equals(const NetMsg& other) const;
	void printMe();

	bool operator== (const NetMsg& other) const;

protected:
	quint32 msgLen;
	quint32 msgType;
	QUuid msgUUID;
	bool hasReUUID;
	QUuid reUUID;
	QString origin;

	virtual bool _serialize(QDataStream* ds) = 0;
	virtual bool _equals(const NetMsg& msg) const;

private:
	/* Disable Default Constructor */
	NetMsg(){};
};

#endif

// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 2 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=2 tabstop=8
