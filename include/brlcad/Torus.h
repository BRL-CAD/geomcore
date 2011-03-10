/*                      T O R U S . H
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
/** @file Torus.h
 *
 *  BRL-CAD core C++ interface:
 *      torus (ID_TOR) database object declaration
 *
 *  Origin -
 *      TNO (Netherlands)
 *      IABG mbH (Germany)
 */

#ifndef BRLCAD_TORUS_INCLUDED
#define BRLCAD_TORUS_INCLUDED

#include <brlcad/Object.h>


struct rt_tor_internal;


namespace BRLCAD {
    class BRLCAD_COREINTERFACE_EXPORT Torus : public Object {
    public:
        Torus(void) throw(bad_alloc);                       ///< constructs a torus at origin with unit tubeRadius, radius from center to tube-center of 2 and unit normal pointing in positive z direction
        Torus(const Vector3D& center,
              const Vector3D& normalToTubeCenterLinePlane,
              double          tubeCenterLineRadius,
              double          tubeRadius) throw(bad_alloc); ///< center, normal of unit length (will be unitized of not of unit length), radius and radius of tube
        Torus(const Torus& original) throw(bad_alloc);
        virtual ~Torus(void) throw();

        const Torus&          operator=(const Torus& original) throw(bad_alloc);

        Vector3D              Center(void) const throw();
        void                  SetCenter(const Vector3D& center) throw();

        Vector3D              Normal(void) const throw();
        void                  SetNormal(const Vector3D& normal) throw();

        double                TubeCenterLineRadius(void) const throw();
        void                  SetTubeCenterLineRadius(double radius) throw();

        double                TubeRadius(void) const throw();
        void                  SetTubeRadius(double radius) throw();

        void                  Set(const Vector3D& center,
                                  const Vector3D& normalToTubeCenterLinePlane,
                                  double          tubeCenterLineRadius,
                                  double          tubeRadius) throw();

        // inherited from BRLCAD::Object
        virtual const Object& operator=(const Object& original) throw(bad_alloc);
        virtual Object*       Clone(void) const throw(bad_alloc, std::bad_alloc);
        static const char*    ClassName(void) throw();
        virtual const char*   Type(void) const throw();
        virtual bool          IsValid(void) const throw(); ///< checks if normal has length 1, if radii are positive and if tubeCenterLineRadius is at least tubeRadius

    protected:
        Torus(resource*       resp,
              directory*      pDir,
              rt_db_internal* ip,
              db_i*           dbip) throw();

        friend class ConstDatabase;

    private:
        rt_tor_internal* m_internalp;

        const rt_tor_internal* Internal(void) const throw();
        rt_tor_internal*       Internal(void) throw();

        friend class Database;
    };
}


#endif // BRLCAD_TORUS_INCLUDED

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
