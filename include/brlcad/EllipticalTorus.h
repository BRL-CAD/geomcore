/*               E L L I P T I C A L T O R U S . H
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
/** @file EllipticalTorus.h
 *
 *  BRL-CAD core C++ interface:
 *      elliptical torus (ID_ETO) database object declaration
 *
 *  Origin -
 *      TNO (Netherlands)
 *      IABG mbH (Germany)
 */

#ifndef BRLCAD_ELLIPTICALTORUS_INCLUDED
#define BRLCAD_ELLIPTICALTORUS_INCLUDED

#include <brlcad/Object.h>


struct rt_eto_internal;


namespace BRLCAD {
    class BRLCAD_COREINTERFACE_EXPORT EllipticalTorus : public Object {
    public:
        EllipticalTorus(void) throw(bad_alloc);                                    ///< constructs a torus at origin with circular tube of unit radius, radius from center to tube-center of 2 and unit normal pointing in positive z direction
        EllipticalTorus(const Vector3D& center,
                        const Vector3D& normalToTubeCenterLinePlane,
                        double          tubeCenterLineRadius,
                        const Vector3D& tubeSemiMajorAxis,
                        double          tubeSemiMinorAxisLength) throw(bad_alloc); ///< center, normal of unit length (will be unitized of not of unit length), radius of revolution, semi-major axis of ellipse, length semi-minor axis of ellipse
        EllipticalTorus(const EllipticalTorus& original) throw(bad_alloc);
        virtual ~EllipticalTorus(void) throw();

        const EllipticalTorus& operator=(const EllipticalTorus& original) throw(bad_alloc);

        Vector3D               Center(void) const throw();
        void                   SetCenter(const Vector3D& center) throw();

        Vector3D               Normal(void) const throw();
        void                   SetNormal(const Vector3D& normal) throw();

        double                 TubeCenterLineRadius(void) const throw();
        void                   SetTubeCenterLineRadius(double radius) throw();

        Vector3D               TubeSemiMajorAxis(void) const throw();
        void                   SetTubeSemiMajorAxis(const Vector3D& axis) throw();

        double                 TubeSemiMinorAxis(void) const throw();
        void                   SetTubeSemiMinorAxis(double length) throw();

        void                   Set(const Vector3D& center,
                                   const Vector3D& normalToTubeCenterLinePlane,
                                   double          tubeCenterLineRadius,
                                   const Vector3D& tubeSemiMajorAxis,
                                   double          tubeSemiMinorAxisLength) throw();

        // inherited from BRLCAD::Object
        virtual const Object&  operator=(const Object& original) throw(bad_alloc);
        virtual Object*        Clone(void) const throw(bad_alloc, std::bad_alloc);
        static const char*     ClassName(void) throw();
        virtual const char*    Type(void) const throw();
        virtual bool           IsValid(void) const throw(); ///< checks if normal has positive length, if radii are not 0 and if ellipse not overlap itself when revolved

    protected:
        EllipticalTorus(resource*       resp,
                        directory*      pDir,
                        rt_db_internal* ip,
                        db_i*           dbip) throw();

        friend class ConstDatabase;

    private:
        rt_eto_internal* m_internalp;

        const rt_eto_internal* Internal(void) const throw();
        rt_eto_internal*       Internal(void) throw();

        friend class Database;
    };
}


#endif // BRLCAD_ELLIPTICALTORUS_INCLUDED

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
