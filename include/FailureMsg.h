/*         F A I L U R E M S G . H
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
/** @file FailureMsg.h
 *
 * Brief description
 *
 */

#ifndef __FAILUREMSG_H__
#define __FAILUREMSG_H__

#include "GenericOneByteMsg.h"

class FailureMsg : public GenericOneByteMsg
{
public:
    /* Normal Constructor */
    FailureMsg(uint8_t failureCode);

    /* Reply Constructor */
    FailureMsg(NetMsg* msg, uint8_t failureCode);

    /* Deserializing Constructor */
    FailureMsg(DataStream* ds, Portal* origin);

    /* Destructor */
    virtual ~FailureMsg();

    /*
     *Getters n Setters
     */
    uint8_t getFailureCode();

private:
	/* Disable copy cstr and =operator */
	FailureMsg(FailureMsg const&):GenericOneByteMsg(0,(uint8_t)0){};
	FailureMsg& operator=(FailureMsg const&){};
};

#endif /* __FAILUREMSG_H__ */

/*
 * Local Variables: ***
 * mode: C
 * tab-width: 8
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t
 * End: ***
 * ex: shiftwidth=4 tabstop=8
*/
