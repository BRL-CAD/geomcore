/*                      M E M O R Y D A T A B A S E . H
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
/** @file MemoryDatabase.h
 *
 *  BRL-CAD core C++ interface:
 *      declares a handle on an in-memory database
 *
 *  Origin -
 *      TNO (Netherlands)
 *      IABG mbH (Germany)
 */

#ifndef BRLCAD_MEMORYDATABASE_INCLUDED
#define BRLCAD_MEMORYDATABASE_INCLUDED

#include <brlcad/Database.h>


namespace BRLCAD {
    class BRLCAD_COREINTERFACE_EXPORT MemoryDatabase : public Database {
    public:
        /// creates a new in-memory database with default _GLOBAL object
        MemoryDatabase(void) throw(bad_alloc);
        virtual ~MemoryDatabase(void) throw();

        /// loads a BRL-CAD database file (*.g) into the memory
        /** The old content of the in-memory database will be discarded.
            The file will be opened for reading only and closed after finishing the operation. */
        virtual bool Load(const char* fileName) throw();
        bool         Save(const char* fileName) throw();

    private:
        MemoryDatabase(const MemoryDatabase&);                  // not implemented
        const MemoryDatabase& operator=(const MemoryDatabase&); // not implemented
    };
}


#endif // BRLCAD_MEMORYDATABASE_INCLUDED

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
