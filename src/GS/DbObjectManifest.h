/*              D B O B J E C T M A N I F E S T . H
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
/** @file DbObjectManifest.h
 *
 */

#ifndef __DBOBJECTMANIFEST_H__
#define __DBOBJECTMANIFEST_H__

#include <list>
#include <map>
#include <string>

#include "GSUuid.h"

class DbObjectManifest
{
public:
    DbObjectManifest();
    virtual ~DbObjectManifest();

private:
    std::list<GSUuid*> DbObjectList;
    std::map<GSUuid*, std::string > DbObjectMap;
};

#endif /* __DBOBJECTMANIFEST_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */

