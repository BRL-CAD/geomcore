/*                       G E D D A T A . C X X
 * BRL-CAD
 *
 * Copyright (c) 2008 United States Government as represented by
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

/** @file GedData.cxx
 *
 * @author Manuel A. Fernandez Montecelo <mafm@users.sourceforge.net>
 *
 * @brief
 *	Container for libged-related data (to not have it spread
 *	throughout the rest of the code).
 */

#include "GedData.h"

#include <brlcad/ged.h>

#include "Logger.h"


/*******************************************************************************
 * GedData
 ******************************************************************************/
GedData* GedData::INSTANCE = 0;

GedData& GedData::instance()
{
  if (!INSTANCE)
    INSTANCE = new GedData();
  return *INSTANCE;
}

GedData::GedData() :
  _dbi(0), _wdb(0), _ged(0)
{
  _dbi = db_open_inmem();
  _wdb = wdb_dbopen(_dbi, RT_WDB_TYPE_DB_INMEM);
  _ged = new struct ged();
  GED_INIT(_ged, _wdb);
}

struct db_i* GedData::getDB()
{
  return _dbi;
}

struct rt_wdb* GedData::getWDB()
{
  return _wdb;
}

struct ged* GedData::getGED()
{
  return _ged;
}


// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 2 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=2 tabstop=8
