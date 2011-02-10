/*                      E L L I P S O I D . C P P
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
/** @file Ellipsoid.cpp
 *
 *  BRL-CAD core C++ interface:
 *      ellipsoid (ID_ELL) database object implementation
 *
 *  Origin -
 *      TNO (Netherlands)
 *      IABG mbH (Germany)
 */

#include <cassert>

#include "rtgeom.h"
#include "raytrace.h"

#include <brlcad/Ellipsoid.h>


using namespace BRLCAD;


Ellipsoid::Ellipsoid(void) throw(bad_alloc) : Object() {
    if (!BU_SETJUMP) {
        BU_GETSTRUCT(m_internalp, rt_ell_internal);
        m_internalp->magic = RT_ELL_INTERNAL_MAGIC;

        SetSphere(Vector3D(), 1.);
    }
    else {
        BU_UNSETJUMP;
        throw bad_alloc("BRLCAD::Ellipsoid::Ellipsoid");
    }

    BU_UNSETJUMP;
}


Ellipsoid::Ellipsoid
(
    const Vector3D& center,
    const Vector3D& semiPrincipalAxisA,
    const Vector3D& semiPrincipalAxisB,
    const Vector3D& semiPrincipalAxisC
) throw(bad_alloc) {
    if (!BU_SETJUMP) {
        BU_GETSTRUCT(m_internalp, rt_ell_internal);
        m_internalp->magic = RT_ELL_INTERNAL_MAGIC;

        Set(center, semiPrincipalAxisA, semiPrincipalAxisB, semiPrincipalAxisC);
    }
    else {
        BU_UNSETJUMP;
        throw bad_alloc("BRLCAD::Ellipsoid::Ellipsoid");
    }

    BU_UNSETJUMP;
}


Ellipsoid::Ellipsoid
(
    const Vector3D& center,
    const Vector3D& semiPrincipalAxis,
    double          radius
) throw(bad_alloc) {
    if (!BU_SETJUMP) {
        BU_GETSTRUCT(m_internalp, rt_ell_internal);
        m_internalp->magic = RT_ELL_INTERNAL_MAGIC;

        Set(center, semiPrincipalAxis, radius);
    }
    else {
        BU_UNSETJUMP;
        throw bad_alloc("BRLCAD::Ellipsoid::Ellipsoid");
    }

    BU_UNSETJUMP;
}


Ellipsoid::Ellipsoid
(
    const Vector3D& center,
    double          radius
) throw(bad_alloc) {
    if (!BU_SETJUMP) {
        BU_GETSTRUCT(m_internalp, rt_ell_internal);
        m_internalp->magic = RT_ELL_INTERNAL_MAGIC;

        SetSphere(center, radius);
    }
    else {
        BU_UNSETJUMP;
        throw bad_alloc("BRLCAD::Ellipsoid::Ellipsoid");
    }

    BU_UNSETJUMP;
}


Ellipsoid::Ellipsoid
(
    const Ellipsoid& original
) throw(bad_alloc) : Object(original) {
    if (!BU_SETJUMP) {
        BU_GETSTRUCT(m_internalp, rt_ell_internal);
        memcpy(m_internalp, original.Internal(), sizeof(rt_ell_internal));
    }
    else {
        BU_UNSETJUMP;
        throw bad_alloc("BRLCAD::Ellipsoid::Ellipsoid");
    }

    BU_UNSETJUMP;
}


Ellipsoid::~Ellipsoid(void) throw() {
    if (m_internalp != 0)
        bu_free(m_internalp, "BRLCAD::Ellipsoid::~Ellipsoid::m_internalp");
}


const Ellipsoid& Ellipsoid::operator=
(
    const Ellipsoid& original
) throw(bad_alloc) {
    if(&original != this) {
        Copy(original);
        memcpy(Internal(), original.Internal(), sizeof(rt_ell_internal));
    }

    return *this;
}


Vector3D Ellipsoid::Center(void) const throw() {
    return Vector3D(Internal()->v);
}


void Ellipsoid::SetCenter
(
    const Vector3D& center
) throw(){
    VMOVE(Internal()->v, center.coordinates);
}


Vector3D Ellipsoid::SemiPrincipalAxis
(
    size_t index
) const throw(){
    Vector3D ret;

    assert(index < 3);

    switch (index) {
        case 0:
            VMOVE(ret.coordinates, Internal()->a);
            break;

        case 1:
            VMOVE(ret.coordinates, Internal()->b);
            break;

        case 2:
            VMOVE(ret.coordinates, Internal()->c);
    }

    return ret;
}


void Ellipsoid::SetSemiPrincipalAxis
(
    size_t          index,
    const Vector3D& semiPrincipalAxis
) throw(){
    assert(index < 3);

    switch (index) {
        case 0:
            VMOVE(Internal()->a, semiPrincipalAxis.coordinates);
            break;

        case 1:
            VMOVE(Internal()->b, semiPrincipalAxis.coordinates);
            break;

        case 2:
            VMOVE(Internal()->c, semiPrincipalAxis.coordinates);
    }
}


void Ellipsoid::Set
(
    const Vector3D& center,
    const Vector3D& semiPrincipalAxisA,
    const Vector3D& semiPrincipalAxisB,
    const Vector3D& semiPrincipalAxisC
) throw(){
    rt_ell_internal* internalp = Internal();

    VMOVE(internalp->v, center.coordinates);
    VMOVE(internalp->a, semiPrincipalAxisA.coordinates);
    VMOVE(internalp->b, semiPrincipalAxisB.coordinates);
    VMOVE(internalp->c, semiPrincipalAxisC.coordinates);
}


void Ellipsoid::Set
(
    const Vector3D& center,
    const Vector3D& semiPrincipalAxis,
    double          radius
) throw() {
    double length = MAGNITUDE(semiPrincipalAxis.coordinates);

    assert(length > SMALL_FASTF);

    if (length > SMALL_FASTF) {
        rt_ell_internal* internalp = Internal();

        VMOVE(internalp->v, center.coordinates);
        VMOVE(internalp->a, semiPrincipalAxis.coordinates);

        bn_vec_ortho(internalp->b, internalp->a);
        VSCALE(internalp->b, internalp->b, radius);

        VCROSS(internalp->c, internalp->a, internalp->b);
        VUNITIZE(internalp->c);
        VSCALE(internalp->c, internalp->c, radius);
    }
}


void Ellipsoid::SetFocals
(
    const Vector3D& focalA,
    const Vector3D& focalB,
    double          majorAxisLength
) throw(){
    assert(majorAxisLength > SMALL_FASTF);

    if (majorAxisLength > SMALL_FASTF) {
        rt_ell_internal* internalp = Internal();

        // v is halfway between the foci
        VADD2(internalp->v, focalA.coordinates, focalB.coordinates);
        VSCALE(internalp->v, internalp->v, 0.5);
        VSUB2(internalp->a, focalB.coordinates, focalA.coordinates);

        double magnitude = MAGNITUDE(Internal()->a);

        if (magnitude <= SMALL_FASTF) {
            // sphere
            VSET(internalp->a, majorAxisLength, 0., 0.);
            VSET(internalp->b, 0., majorAxisLength, 0.);
            VSET(internalp->c, 0., 0., majorAxisLength);
        }
        else {
            // calculate a
            VUNITIZE(internalp->a);
            VSCALE(internalp->a, internalp->a, majorAxisLength);

            // calculate b
            double radius = sqrt(majorAxisLength * majorAxisLength - magnitude * magnitude * 0.25);
            assert(radius > SMALL_FASTF);

            bn_vec_ortho(internalp->b, internalp->a);
            VSCALE(internalp->b, internalp->b, radius);

            // calculate c
            VCROSS(internalp->c, internalp->a, internalp->b);
            VUNITIZE(internalp->c);
            VSCALE(internalp->c, internalp->c, radius);
        }
    }
}


void Ellipsoid::SetSphere
(
    const Vector3D& center,
    double          radius
) throw() {
    assert(radius > SMALL_FASTF);

    if (!NEAR_ZERO(radius, SMALL_FASTF)) {
        rt_ell_internal* internalp = Internal();

        VMOVE(internalp->v, center.coordinates);
        VSET(m_internalp->a, radius, 0., 0.);
        VSET(m_internalp->b, 0., radius, 0.);
        VSET(m_internalp->c, 0., 0., radius);
    }
}


const Object& Ellipsoid::operator=
(
    const Object& original
) throw(bad_alloc) {
    const Ellipsoid* ell = dynamic_cast<const Ellipsoid*>(&original);
    assert(ell != 0);

    if (ell != 0)
        *this = *ell;

    return *this;
}


Object* Ellipsoid::Clone(void) const throw(bad_alloc, std::bad_alloc) {
    return new Ellipsoid(*this);
}


const char* Ellipsoid::ClassName(void) throw() {
    return "Ellipsoid";
}


const char* Ellipsoid::Type(void) const throw() {
    return ClassName();
}


bool Ellipsoid::IsValid(void) const throw() {
    bool                   ret       = false;
    const rt_ell_internal* internalp = Internal();

    if (!VNEAR_ZERO(internalp->a, SMALL_FASTF) &&
        !VNEAR_ZERO(internalp->b, SMALL_FASTF) &&
        !VNEAR_ZERO(internalp->c, SMALL_FASTF) &&
        NEAR_ZERO(VDOT(internalp->b, Internal()->a ), RT_DOT_TOL) &&
        NEAR_ZERO(VDOT(internalp->c, Internal()->a ), RT_DOT_TOL) &&
        NEAR_ZERO(VDOT(internalp->b, Internal()->c ), RT_DOT_TOL))
        ret = true;

    return ret;
}


Ellipsoid::Ellipsoid
(
    resource*       resp,
    directory*      pDir,
    rt_db_internal* ip,
    db_i*           dbip
) throw() : Object(resp, pDir, ip, dbip), m_internalp(0) {}


rt_ell_internal* Ellipsoid::Internal(void) throw() {
    rt_ell_internal* ret;

    if (m_ip != 0)
        ret = static_cast<rt_ell_internal*>(m_ip->idb_ptr);
    else
        ret = m_internalp;

    RT_ELL_CK_MAGIC(ret);

    return ret;
}


const rt_ell_internal* Ellipsoid::Internal(void) const throw() {
    const rt_ell_internal* ret;

    if (m_ip != 0)
        ret = static_cast<const rt_ell_internal*>(m_ip->idb_ptr);
    else
        ret = m_internalp;

    RT_ELL_CK_MAGIC(ret);

    return ret;
}
