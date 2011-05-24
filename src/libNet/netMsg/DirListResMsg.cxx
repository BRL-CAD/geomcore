/*               D I R L I S T R E S M S G . C X X
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
/** @file DirListResMsg.cxx
 *
 * Brief description
 *
 */


#include "DirListResMsg.h"
#include <iostream>

/* Normal Constructor */
DirListResMsg::DirListResMsg(
    std::list<std::string>* items) :
    NetMsg(GEOMETRYMANIFEST)
{
    this->itemData = new std::list<std::string> (*items);
}

/* Reply Constructor */
DirListResMsg::DirListResMsg(NetMsg* msg, std::list<std::string>* items) :
	NetMsg(GEOMETRYMANIFEST, msg)
{
  this->itemData = new std::list<std::string> (*items);
}

/* Deserializing Constructor */
DirListResMsg::DirListResMsg(ByteBuffer* bb, Portal* origin) :
  NetMsg(bb, origin)
{
  this->itemData = new std::list<std::string>();
  uint32_t numOfItems = bb->get32bit();
  std::string tstr;
  for (uint32_t i = 0; i < numOfItems; ++i)
    {
      tstr = bb->getString();
      if (tstr.size() == 0)
        continue;
      this->itemData->push_back(tstr);
    }
}

/* Destructor */
DirListResMsg::~DirListResMsg()
{
  delete this->itemData;
}

bool
DirListResMsg::_serialize(ByteBuffer* bb)
{
  /* put in placeholder for count */
  int start = bb->position();
  bb->put32bit(0);

  //TODO this list serializer could be generalized....
  /* Add elements while counting. */
  int cnt = 0;
  std::string tstr;
  for (std::list<std::string>::iterator it = this->itemData->begin(); it
      != this->itemData->end(); it++)
    {
      tstr = *it;
      if (tstr.size() == 0)
        continue;
      bb->putString(tstr);
      ++cnt;
    }

  /* Go back and insert actual count */
  int stop = bb->position();
  bb->setPosition(start);
  bb->put32bit(cnt);
  bb->setPosition(stop);

  return true;
}

bool
DirListResMsg::_equals(const NetMsg& msg)
{
  DirListResMsg& gmsg = (DirListResMsg&) msg;
  if (this->itemData->size() != gmsg.itemData->size())
    return false;
  std::list<std::string>::iterator it = this->itemData->begin(), git =
      gmsg.itemData->begin();

  for (; it != this->itemData->end(); it++, git++)
    if (*it != *git)
      return false;
  return true;
}

std::string
DirListResMsg::toString()
{
  std::string out = "Number of items: " + this->itemData->size();

  std::list<std::string>::iterator it;
  for (it = this->itemData->begin(); it != this->itemData->end(); ++it)
    {
      out += "{" + *it + "} ";
    }

  return out;
}

/*
 *Getters n Setters
 */
uint32_t
DirListResMsg::getNumOfItems()
{
  return this->itemData->size();
}

uint32_t
DirListResMsg::getItems(std::list<std::string>* items)
{

  std::list<std::string>::iterator it = this->itemData->begin();
  for (;it != this->itemData->end(); ++it)
    items->push_back(*it);

  return items->size();
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
