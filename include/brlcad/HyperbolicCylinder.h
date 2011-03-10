/*          H Y P E R B O L I C C Y L I N D E R . H
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
/** @file HyperbolicCylinder.h
 *
 *  BRL-CAD core C++ interface:
 *      right hyperbolicCylinder (ID_RHC) database object declaration
 *
 *  Origin -
 *      TNO (Netherlands)
 *      IABG mbH (Germany)
 */

#ifndef BRLCAD_HYPERBOLICCYLINDER_INCLUDED
#define BRLCAD_HYPERBOLICCYLINDER_INCLUDED

#include <brlcad/Object.h>


struct rt_rhc_internal;


namespace BRLCAD {
    class BRLCAD_COREINTERFACE_EXPORT HyperbolicCylinder : public Object {
    public:
        HyperbolicCylinder(void) throw(bad_alloc); ///< constructs a hyperbolic cylinder at origin with unit height, unit depth in x direction, unit half width and apex to asymptotes distance of 1
        HyperbolicCylinder(const Vector3D& basePoint,
                           const Vector3D& height,
                           const Vector3D& depth,
                           double          halfWidth,
                           double          apexAsymptoteDistance) throw(bad_alloc);
        HyperbolicCylinder(const HyperbolicCylinder& original) throw(bad_alloc);
        virtual ~HyperbolicCylinder(void) throw();

        const HyperbolicCylinder& operator=(const HyperbolicCylinder& original) throw(bad_alloc);

        Vector3D                  BasePoint(void) const throw();
        void                      SetBasePoint(const Vector3D& basePoint) throw();

        Vector3D                  Height(void) const throw();
        void                      SetHeight(const Vector3D& height) throw();

        Vector3D                  Depth(void) const throw();
        void                      SetDepth(const Vector3D& depth) throw();

        double                    HalfWidth(void) const throw();
        void                      SetHalfWidth(double length) throw();

        double                    ApexAsymptoteDistance(void) const throw();
        void                      SetApexAsymptoteDistance(double distance) throw();

        void                      Set(const Vector3D& basePoint,
                                      const Vector3D& height,
                                      const Vector3D& depth,
                                      double          halfWidth,
                                      double          apexAsymptoteDistance) throw();

        // inherited from BRLCAD::Object
        virtual const Object&     operator=(const Object& original) throw(bad_alloc);
        virtual Object*           Clone(void) const throw(bad_alloc, std::bad_alloc);
        static const char*        ClassName(void) throw();
        virtual const char*       Type(void) const throw();
        virtual bool              IsValid(void) const throw(); ///< checks for positive length of height and depth, positive values of half width and apex to asymptotes distance and that height and depth are perpendicular

    protected:
        HyperbolicCylinder(resource*       resp,
                           directory*      pDir,
                           rt_db_internal* ip,
                           db_i*           dbip) throw();

        friend class ConstDatabase;

    private:
        rt_rhc_internal* m_internalp;

        rt_rhc_internal*       Internal(void) throw();
        const rt_rhc_internal* Internal(void) const throw();

        friend class Database;
    };
}


#endif // BRLCAD_HYPERBOLICCYLINDER_INCLUDED

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
