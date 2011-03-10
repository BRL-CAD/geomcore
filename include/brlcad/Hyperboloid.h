/*                   H Y P E R B O L O I D . H
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
/** @file Hyperboloid.h
 *
 *  BRL-CAD core C++ interface:
 *      elliptical hyperboloid (ID_EHY) database object declaration
 *
 *  Origin -
 *      TNO (Netherlands)
 *      IABG mbH (Germany)
 */

#ifndef BRLCAD_HYPERBOLOID_INCLUDED
#define BRLCAD_HYPERBOLOID_INCLUDED

#include <brlcad/Object.h>


struct rt_ehy_internal;


namespace BRLCAD {
    class BRLCAD_COREINTERFACE_EXPORT Hyperboloid : public Object {
    public:
        Hyperboloid(void) throw(bad_alloc);                                  ///< constructs a hyperboloid with height 1 at origin with a circular base and unit distance from apex to asymptote
        Hyperboloid(const Vector3D& basePoint,
                    const Vector3D& height,
                    const Vector3D& semiMajorAxis,
                    double          semiMinorAxisLength,
                    double          apexAsymptoteDistance) throw(bad_alloc); ///< length an direction of vector semi major axis used
        Hyperboloid(const Vector3D& basePoint,
                    const Vector3D& height,
                    const Vector3D& semiMajorAxisDirection,
                    double          semiMajorAxisLength,
                    double          semiMinorAxisLength,
                    double          apexAsymptoteDistance) throw(bad_alloc); ///< semi major axis will be unitized and length of semi major axis is given by semiMajorAxisLength
        Hyperboloid(const Hyperboloid& original) throw(bad_alloc);
        virtual ~Hyperboloid(void) throw();

        const Hyperboloid&    operator=(const Hyperboloid& original) throw(bad_alloc);

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

        double                ApexAsymptoteDistance(void) const throw();
        void                  SetApexAsymptoteDistance(double distance) throw();

        void                  Set(const Vector3D& basePoint,
                                  const Vector3D& height,
                                  const Vector3D& semiMajorAxis,
                                  double          semiMinorAxisLength,
                                  double          apexAsymptoteDistance) throw();
        void                  Set(const Vector3D& basePoint,
                                  const Vector3D& height,
                                  const Vector3D& semiMajorAxisDirection,
                                  double          semiMajorAxisLength,
                                  double          semiMinorAxisLength,
                                  double          apexAsymptoteDistance) throw();

        // inherited from BRLCAD::Object
        virtual const Object& operator=(const Object& original) throw(bad_alloc);
        virtual Object*       Clone(void) const throw(bad_alloc, std::bad_alloc);
        static const char*    ClassName(void) throw();
        virtual const char*   Type(void) const throw();
        virtual bool          IsValid(void) const throw(); ///< checks for positive height, unit length of internal semi major axis vector, semi minor axis is not longer than semi major axis, both are nonzero, nonzero apex distance, and height and semi major axis are perpendicular

    protected:
        Hyperboloid(resource*       resp,
                    directory*      pDir,
                    rt_db_internal* ip,
                    db_i*           dbip) throw();

        friend class ConstDatabase;

    private:
        rt_ehy_internal* m_internalp;

        rt_ehy_internal*       Internal(void) throw();
        const rt_ehy_internal* Internal(void) const throw();

        friend class Database;
    };
}


#endif // BRLCAD_HYPERBOLOID_INCLUDED

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
