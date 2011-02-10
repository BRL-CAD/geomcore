/*                      M E M O R Y D A T A B A S E . C P P
 * BRL-CAD
 *
 * Copyright (c) 2008-2011 United States Government as represented by
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
/** @file MemoryDatabase.cpp
 *
 *  BRL-CAD core C++ interface:
 *      implements a handle on an in-memory database
 *
 *  Origin -
 *      TNO (Netherlands)
 *      IABG mbH (Germany)
 */

#include <cassert>

#include <brlcad/MemoryDatabase.h>

#include "raytrace.h"


using namespace BRLCAD;


MemoryDatabase::MemoryDatabase(void) throw(bad_alloc) : Database() {
    db_i* dbip = 0;

    if (!BU_SETJUMP) {
        dbip = db_create_inmem();
        RT_CK_DBI(dbip);
    }
    else {
        BU_UNSETJUMP;
        throw bad_alloc("BRLCAD::MemoryDatabase::MemoryDatabase");
    }

    BU_UNSETJUMP;

    if (!BU_SETJUMP) {
        m_rtip = rt_new_rti(dbip); // clones dbip
        rt_init_resource(m_resp, 0, m_rtip);
    }
    else {
        BU_UNSETJUMP;
        db_close(dbip);
        m_rtip = 0;
        throw bad_alloc("BRLCAD::MemoryDatabase::MemoryDatabase");
    }

    BU_UNSETJUMP;

    m_wdbp = dbip->dbi_wdbp;   // takes ownership of dbip
}


MemoryDatabase::~MemoryDatabase(void) throw() {}


bool MemoryDatabase::Load
(
    const char* fileName
) throw() {
    bool ret = false;

    if (!BU_SETJUMP) {
        rt_i* source = rt_dirbuild(fileName, 0, 0);

        if (source != 0) {
            ret = (db_dump(m_wdbp, source->rti_dbip) == 0);

            assert(m_wdbp->dbip == m_rtip->rti_dbip);
            db_update_ident(m_wdbp->dbip, source->rti_dbip->dbi_title, source->rti_dbip->dbi_base2local);

            rt_free_rti(source);
        }
    }

    BU_UNSETJUMP;

    return ret;
}


bool MemoryDatabase::Save
(
    const char* fileName
) throw() {
    bool ret = false;

    if (!BU_SETJUMP) {
        rt_wdb* target = wdb_fopen(fileName);

        if (target != 0) {
            ret = (db_dump(target, m_wdbp->dbip) == 0);
            wdb_close(target);
        }
    }

    BU_UNSETJUMP;

    return ret;
}
