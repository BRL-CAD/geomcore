/*                      C O N E . H
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
/** @file Cone.h
 *
 *  BRL-CAD core C++ interface:
 *      truncated general cone (ID_TGC) database object declaration
 *
 *  Origin -
 *      TNO (Netherlands)
 *      IABG mbH (Germany)
 */

#ifndef BRLCAD_CONE_INCLUDED
#define BRLCAD_CONE_INCLUDED

#include <brlcad/Object.h>


struct rt_tgc_internal;


namespace BRLCAD {
    class BRLCAD_COREINTERFACE_EXPORT Cone : public Object {
    public:
        Cone(void) throw(bad_alloc);                               ///< constructs a cone at the origin with unit radius and unit height
        Cone(const Vector3D& basePoint,
             const Vector3D& height,
             const Vector3D& semiPrincipalAxisA,
             const Vector3D& semiPrincipalAxisB,
             double          ratioCtoA,
             double          ratioDtoB) throw(bad_alloc);          ///< tgc: basePoint, height, two perpendicular semi principal axes of base face and two scalars for the semi principal axes of top face
        Cone(const Vector3D& basePoint,
             const Vector3D& height,
             const Vector3D& semiPrincipalAxisA,
             const Vector3D& semiPrincipalAxisB,
             double          scale) throw(bad_alloc);              ///< tec: basePoint, height, two perpendicular semi principal axes of base face and one ratio for scaling the semi principal axes of top face
        Cone(const Vector3D& basePoint,
             const Vector3D& height,
             const Vector3D& semiPrincipalAxisA,
             const Vector3D& semiPrincipalAxisB) throw(bad_alloc); ///< rec: basePoint, height, two perpendicular semi principal axes of base and top face
        Cone(const Vector3D& basePoint,
             const Vector3D& height,
             double          radiusBase,
             double          radiusTop) throw(bad_alloc);          ///< trc: basePoint, height and two radii for base and top face respectively
        Cone(const Vector3D& basePoint,
             const Vector3D& height,
             double          radius) throw(bad_alloc);             ///< rcc: basePoint, height and one radius for base and top face
        Cone(const Cone& original) throw(bad_alloc);
        virtual ~Cone(void) throw();

        const Cone&           operator=(const Cone& original) throw(bad_alloc);

        Vector3D              BasePoint(void) const throw();
        void                  SetBasePoint(const Vector3D& basePoint) throw();

        Vector3D              Height(void) const throw();
        void                  SetHeight(const Vector3D& height) throw();

        /// semi principal axes (base and top face) are accessed by their index (0/1/2/3 for the vectos a/b/c/d)
        Vector3D              SemiPrincipalAxis(size_t index) const throw();
        void                  SetSemiPrincipalAxis(size_t          index,
                                                   const Vector3D& semiPrincipalAxis) throw();

        void                  Set(const Vector3D& basePoint,
                                  const Vector3D& height,
                                  const Vector3D& semiPrincipalAxisA,
                                  const Vector3D& semiPrincipalAxisB,
                                  double          ratioCtoA,
                                  double          ratioDtoB) throw();          ///< truncated general cone (tgc)
        void                  Set(const Vector3D& basePoint,
                                  const Vector3D& height,
                                  const Vector3D& semiPrincipalAxisA,
                                  const Vector3D& semiPrincipalAxisB,
                                  double          scale) throw();              ///< truncated elliptical cone (tec)
        void                  Set(const Vector3D& basePoint,
                                  const Vector3D& height,
                                  const Vector3D& semiPrincipalAxisA,
                                  const Vector3D& semiPrincipalAxisB) throw(); ///< right elliptical cylinder (rec)
        void                  Set(const Vector3D& basePoint,
                                  const Vector3D& height,
                                  double          radiusBase,
                                  double          radiusTop) throw();          ///< truncated right cone (trc)
        void                  Set(const Vector3D& basePoint,
                                  const Vector3D& height,
                                  double          radius) throw();             ///< right circular cylinder (rcc)

        // inherited from BRLCAD::Object
        virtual const Object& operator=(const Object& original) throw(bad_alloc);
        virtual Object*       Clone(void) const throw(bad_alloc, std::bad_alloc);
        static const char*    ClassName(void) throw();
        virtual const char*   Type(void) const throw();
        virtual bool          IsValid(void) const throw(); ///< checks if semi-principal axes are perpendicular, if the ones at the base have positive length, if height has positive length and is not in the plane of semi-principal axes and if corresponding base and top semi principal axes are parallel and pointing to the same direction

    protected:
        Cone(resource*       resp,
             directory*      pDir,
             rt_db_internal* ip,
             db_i*           dbip) throw();

        friend class ConstDatabase;

    private:
        rt_tgc_internal* m_internalp;

        rt_tgc_internal*       Internal(void) throw();
        const rt_tgc_internal* Internal(void) const throw();

        friend class Database;
    };
}


#endif // BRLCAD_CONE_INCLUDED

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
