/*                      E L L I P S O I D . H
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
/** @file Ellipsoid.h
 *
 *  BRL-CAD core C++ interface:
 *      ellipsoid (ID_ELL) database object declaration
 *
 *  Origin -
 *      TNO (Netherlands)
 *      IABG mbH (Germany)
 */

#ifndef BRLCAD_ELLIPSOID_INCLUDED
#define BRLCAD_ELLIPSOID_INCLUDED

#include <brlcad/Object.h>


struct rt_ell_internal;


namespace BRLCAD {
    class BRLCAD_COREINTERFACE_EXPORT Ellipsoid : public Object {
    public:
        Ellipsoid(void) throw(bad_alloc);                               ///< constructs the unit circle around the origin
        Ellipsoid(const Vector3D& center,
                  const Vector3D& semiPrincipalAxisA,
                  const Vector3D& semiPrincipalAxisB,
                  const Vector3D& semiPrincipalAxisC) throw(bad_alloc); ///< center and three perpendicular semi-principal-axes
        Ellipsoid(const Vector3D& center,
                  const Vector3D& semiPrincipalAxis,
                  double          radius) throw(bad_alloc);             ///< center, one semi-principal-axis and radius (body of revolution)
        Ellipsoid(const Vector3D& center,
                  double          radius) throw(bad_alloc);             ///< sphere with center and radius
        Ellipsoid(const Ellipsoid& original) throw(bad_alloc);
        virtual ~Ellipsoid(void) throw();

        const Ellipsoid&      operator=(const Ellipsoid& original) throw(bad_alloc);

        Vector3D              Center(void) const throw();
        void                  SetCenter(const Vector3D& center) throw();

        /// semi principal axes are accessed by their index (0/1/2 for the vectos a/b/c)*/
        Vector3D              SemiPrincipalAxis(size_t index) const throw();
        void                  SetSemiPrincipalAxis(size_t          index,
                                                   const Vector3D& semiPrincipalAxis) throw();

        void                  Set(const Vector3D& center,
                                  const Vector3D& semiPrincipalAxisA,
                                  const Vector3D& semiPrincipalAxisB,
                                  const Vector3D& semiPrincipalAxisC) throw();
        void                  Set(const Vector3D& center,
                                  const Vector3D& semiPrincipalAxis,
                                  double          radius) throw();
        void                  SetFocals(const Vector3D& focalA,
                                        const Vector3D& focalB,
                                        double          majorAxisLength) throw();
        void                  SetSphere(const Vector3D& center,
                                        double          radius) throw();

        // inherited from BRLCAD::Object
        virtual const Object& operator=(const Object& original) throw(bad_alloc);
        virtual Object*       Clone(void) const throw(bad_alloc, std::bad_alloc);
        static const char*    ClassName(void) throw();
        virtual const char*   Type(void) const throw();
        virtual bool          IsValid(void) const throw(); ///< checks if semi-principal axes are perpendicular and if they have positive length

    protected:
        Ellipsoid(resource*       resp,
                  directory*      pDir,
                  rt_db_internal* ip,
                  db_i*           dbip) throw();

        friend class ConstDatabase;

    private:
        rt_ell_internal* m_internalp;

        rt_ell_internal*       Internal(void) throw();
        const rt_ell_internal* Internal(void) const throw();

        friend class Database;
    };
}


#endif // BRLCAD_ELLIPSOID_INCLUDED

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
