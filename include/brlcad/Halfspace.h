/*                      H A L F S P A C E . H
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
/** @file Halfspace.h
 *
 *  BRL-CAD core C++ interface:
 *      half-space (ID_HALF) database object declaration
 *
 *  Origin -
 *      TNO (Netherlands)
 *      IABG mbH (Germany)
 */

#ifndef BRLCAD_HALFSPACE_INCLUDED
#define BRLCAD_HALFSPACE_INCLUDED

#include <brlcad/Object.h>


struct rt_half_internal;


namespace BRLCAD {
    class BRLCAD_COREINTERFACE_EXPORT Halfspace : public Object {
    public:
        Halfspace(void) throw(bad_alloc);
        Halfspace(const Halfspace& original) throw(bad_alloc);
        virtual ~Halfspace(void) throw();

        const Halfspace&      operator=(const Halfspace& original) throw(bad_alloc);

        Vector3D              Normal(void) const throw();
        void                  SetNormal(const Vector3D& normal) throw();
        double                DistanceFromOrigin(void) const throw();
        void                  SetDistanceFromOrigin(double distance) throw();

        // inherited from BRLCAD::Object
        virtual const Object& operator=(const Object& original) throw(bad_alloc);
        virtual Object*       Clone(void) const throw(bad_alloc, std::bad_alloc);
        static const char*    ClassName(void) throw();
        virtual const char*   Type(void) const throw();
        virtual bool          IsValid(void) const throw();

    protected:
        Halfspace(resource*       resp,
                  directory*      pDir,
                  rt_db_internal* ip,
                  db_i*           dbip = 0) throw();

        friend class ConstDatabase;

    private:
        // holds Objects's content if not connected to a database
        rt_half_internal* m_internalp;

        const rt_half_internal* Internal(void) const throw();
        rt_half_internal*       Internal(void) throw();

        friend class Database;
    };
}


#endif // BRLCAD_HALFSPACE_INCLUDED

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
