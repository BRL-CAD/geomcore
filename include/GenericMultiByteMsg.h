/*             G E N E R I C M U L T I B Y T E M S G . H
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
/** @file GenericMultiByteMsg.h
 *
 * Brief description
 *
 */

#ifndef __GENERICMULTIBYTEMSG_H__
#define __GENERICMULTIBYTEMSG_H__

#include "NetMsg.h"
#include <QtCore/QByteArray>

class GenericMultiByteMsg : public NetMsg
{
public:
	/* Normal Constructor */
	GenericMultiByteMsg(quint32 type, char* dataIn, quint32 dataInLen);

	/* Reply Constructor */
	GenericMultiByteMsg(quint32 type, NetMsg* msg, char* dataIn, quint32 dataInLen);

	/* Deserializing Constructor */
	GenericMultiByteMsg(QDataStream* ds, Portal* origin);

	/* Destructor */
	virtual ~GenericMultiByteMsg();

	/*
	 * Utilities
	 */
	virtual std::string toString();
	QByteArray* getQByteArray();

protected:
	quint32 getDataLen();
	char* getData();

	quint32 dataLen;
	char* data;

	virtual bool _serialize(QDataStream* ds);
	virtual bool _equals(const NetMsg& msg);

private:
	/* Disable copy cstr and =operator */
	GenericMultiByteMsg(GenericMultiByteMsg const&):NetMsg(0){};
	GenericMultiByteMsg& operator=(GenericMultiByteMsg const&){};
};

#endif /* __GENERICMULTIBYTEMSG_H__ */

/*
 * Local Variables: ***
 * mode: C++ ***
 * tab-width: 8 ***
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t ***
 * End: ***
 * ex: shiftwidth=2 tabstop=8
*/
