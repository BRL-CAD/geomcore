/*             G E N E R I C O N E S T R I N G M S G . C X X
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
/** @file GenericOneStringMsg.cxx
 *
 * Brief description
 *
 */

#include "libNetwork/GenericOneStringMsg.h"
#include <sstream>

//Normal Constructor
GenericOneStringMsg::GenericOneStringMsg(quint32 type, QString s):
  NetMsg(type), strData(s)
{
}

  //Reply Constructor
GenericOneStringMsg::GenericOneStringMsg(quint32 type, NetMsg* msg, QString s):
  NetMsg(type, msg), strData(s)
{
}

//Deserializing Constructors
GenericOneStringMsg::GenericOneStringMsg(QDataStream* ds):
  NetMsg(ds)
{
  this->strData = *Utils::getString(ds);
}

//Destructor
GenericOneStringMsg::~GenericOneStringMsg()
{
}

bool GenericOneStringMsg::_serialize(QDataStream* ds)
{
  Utils::putString(ds, this->strData);
  return true;
}

QString GenericOneStringMsg::toString() 
{
  QString out;

  out.append(NetMsg::toString());
  out.append("\t  strData: ");
  out.append(this->strData);
  
  return out;
}


bool
GenericOneStringMsg::_equals(NetMsg& msg) 
{
  GenericOneStringMsg& gmsg = (GenericOneStringMsg&) msg;
 
  if (this->getStrData() != gmsg.getStrData()) {
    return false;
  }

  return true;
}


 /*
 *Getters n Setters
 */
QString GenericOneStringMsg::getStrData() {return this->strData;}


// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 2 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=2 tabstop=8
