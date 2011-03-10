/*                P A R A B O L O I D . H
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
/** @file Paraboloid.h
 *
 *  BRL-CAD core C++ interface:
 *      elliptical paraboloid (ID_EPA) database object declaration
 *
 *  Origin -
 *      TNO (Netherlands)
 *      IABG mbH (Germany)
 */

#ifndef BRLCAD_PARABOLOID_INCLUDED
#define BRLCAD_PARABOLOID_INCLUDED

#include <brlcad/Object.h>


struct rt_epa_internal;


namespace BRLCAD {
    class BRLCAD_COREINTERFACE_EXPORT Paraboloid : public Object {
    public:
        Paraboloid(void) throw(bad_alloc);                                ///< constructs a paraboloid with unit height at origin with a circular base
        Paraboloid(const Vector3D& basePoint,
                   const Vector3D& height,
                   const Vector3D& semiMajorAxis,
                   double          semiMinorAxisLength) throw(bad_alloc); ///< length an direction of vector semi major axis used
        Paraboloid(const Vector3D& basePoint,
                   const Vector3D& height,
                   const Vector3D& semiMajorAxisDirection,
                   double          semiMajorAxisLength,
                   double          semiMinorAxisLength) throw(bad_alloc); ///< semi major axis will be unitized and length of semi major axis is given by semiMajorAxisLength
        Paraboloid(const Paraboloid& original) throw(bad_alloc);
        virtual ~Paraboloid(void) throw();

        const Paraboloid&     operator=(const Paraboloid& original) throw(bad_alloc);

        Vector3D              BasePoint(void) const throw();
        void                  SetBasePoint(const Vector3D& point) throw();

        Vector3D              Height(void) const throw();
        void                  SetHeight(const Vector3D& height) throw();

        Vector3D              SemiMajorAxis(void) const throw();
        void                  SetSemiMajorAxis(const Vector3D& axis) throw();
        void                  SetSemiMajorAxis(const Vector3D& direction,
                                               double          length) throw();

        Vector3D              SemiMajorAxisDirection(void) const throw();
        void                  SetSemiMajorAxisDirection(const Vector3D& direction) throw();

        double                SemiMajorAxisLength(void) const throw();
        void                  SetSemiMajorAxisLength(double length) throw();

        double                SemiMinorAxisLength(void) const throw();
        void                  SetSemiMinorAxisLength(double length) throw();

        void                  Set(const Vector3D& basePoint,
                                  const Vector3D& height,
                                  const Vector3D& semiMajorAxis,
                                  double          semiMinorAxisLength) throw();
        void                  Set(const Vector3D& basePoint,
                                  const Vector3D& height,
                                  const Vector3D& semiMajorAxisDirection,
                                  double          semiMajorAxisLength,
                                  double          semiMinorAxisLength) throw();

        // inherited from BRLCAD::Object
        virtual const Object& operator=(const Object& original) throw(bad_alloc);
        virtual Object*       Clone(void) const throw(bad_alloc, std::bad_alloc);
        static const char*    ClassName(void) throw();
        virtual const char*   Type(void) const throw();
        virtual bool          IsValid(void) const throw(); ///< checks for positive height, unit length of internal semi major axis vector, semi minor axis is not longer than semi major axis, both are nonzero and height and semi major axis are perpendicular

    protected:
        Paraboloid(resource*       resp,
                   directory*      pDir,
                   rt_db_internal* ip,
                   db_i*           dbip) throw();

        friend class ConstDatabase;

    private:
        rt_epa_internal* m_internalp;

        rt_epa_internal*       Internal(void) throw();
        const rt_epa_internal* Internal(void) const throw();

        friend class Database;
    };
}


#endif // BRLCAD_PARABOLOID_INCLUDED

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
