/*             G E N E R I C F O U R B Y T E S M S G . H
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
/** @file GenericFourBytesMsg.h
 *
 * Brief description
 *
 */

#ifndef __GENERICFOURBYTESMSG_H__
#define __GENERICFOURBYTESMSG_H__

#include "NetMsg.h"

class GenericFourBytesMsg : public NetMsg
{
public:
	/* Normal Constructor */
	GenericFourBytesMsg(quint32 type, quint32 b);

	/* Reply Constructor */
	GenericFourBytesMsg(quint32 type, NetMsg* msg, quint32 b);

	/* Deserializing Constructor */
	GenericFourBytesMsg(QDataStream* ds, Portal* origin);

	/* Destructor */
	virtual ~GenericFourBytesMsg();

	/*
	 * Utilities
	 */
	virtual std::string toString();

protected:
	quint32 getData();
	quint32 data;

	virtual bool _serialize(QDataStream* ds);
	virtual bool _equals(const NetMsg& msg);

private:
	/* Disable copy cstr and =operator */
	GenericFourBytesMsg(GenericFourBytesMsg const&):NetMsg(0){};
	GenericFourBytesMsg& operator=(GenericFourBytesMsg const&){};
};

#endif /* __GENERICFOURBYTESMSG_H__ */

/*
 * Local Variables: ***
 * mode: C++ ***
 * tab-width: 8 ***
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t ***
 * End: ***
 * ex: shiftwidth=2 tabstop=8
*/
