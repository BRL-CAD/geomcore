/*                      D A T A B A S E . H
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
/** @file Database.h
 *
 *  BRL-CAD core C++ interface:
 *      declares the common part of the handles on a writable database
 *
 *  Origin -
 *      TNO (Netherlands)
 *      IABG mbH (Germany)
 */

#ifndef BRLCAD_DATABASE_INCLUDED
#define BRLCAD_DATABASE_INCLUDED

#include <brlcad/ConstDatabase.h>


struct rt_wdb;


namespace BRLCAD {
    class BRLCAD_COREINTERFACE_EXPORT Database : public ConstDatabase {
    public:
        virtual ~Database(void) throw();

        /// loads a BRL-CAD database file (*.g)
        /** The implementation will be determed by the subclasses. */
        virtual bool Load(const char* fileName) throw() = 0;

        void         SetTitle(const char* title) throw(bad_alloc);

        /// adds an object to the database
        bool         Add(const Object& object) throw();

        /// removes an object from the database
        /** The object but not its references are removed. */
        void         Delete(const char* objectName) throw();

        /// @name Accessing objects
        //@{
        using ConstDatabase::Get;

        class ObjectCallback {
        public:
            virtual ~ObjectCallback(void) throw() {}

            /// the user has to implement this object method to evaluate and modify the object
            virtual void operator()(Object& object) = 0;

        protected:
            ObjectCallback(void) throw() {}
            ObjectCallback(const ObjectCallback&) throw() {}
            const ObjectCallback& operator=(const ObjectCallback&) throw() {return *this;}
        };

        /// selects a single object and hand it over to an ObjectCallback (for read and write)
        void         Get(const char*     objectName,
                         ObjectCallback& callback);

        /// provided for convenience: selects a single object and sets it to \a object
        /** The type of the object in the database and \a object must match. */
        void         Set(const Object& object) throw(bad_alloc);
        //@}

    protected:
        rt_wdb* m_wdbp;

        Database(void) throw(bad_alloc);

    private:
        Database(const Database&);                  // not implemented
        const Database& operator=(const Database&); // not implemented
    };
}


#endif // BRLCAD_DATABASE_INCLUDED

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
