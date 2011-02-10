/*                H Y P E R B O L I C C Y L I N D E R . C P P
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
/** @file HyperbolicCylinder.cpp
 *
 *  BRL-CAD core C++ interface:
 *      right hyperbolicCylinder (ID_RHC) database object implementation
 *
 *  Origin -
 *      TNO (Netherlands)
 *      IABG mbH (Germany)
 */

#include <cassert>

#include "rtgeom.h"
#include "raytrace.h"

#include<brlcad/HyperbolicCylinder.h>


using namespace BRLCAD;


HyperbolicCylinder::HyperbolicCylinder(void) throw(bad_alloc) : Object() {
    if (!BU_SETJUMP) {
        BU_GETSTRUCT(m_internalp, rt_rhc_internal);
        m_internalp->rhc_magic = RT_RHC_INTERNAL_MAGIC;

        Set(Vector3D(),Vector3D(0., 0., 1.),Vector3D(1., 0., 0.), 1., 1.);
    }
    else {
        BU_UNSETJUMP;
        throw bad_alloc("BRLCAD::HyperbolicCylinder::HyperbolicCylinder");
    }

    BU_UNSETJUMP;
}


HyperbolicCylinder::HyperbolicCylinder
(
    const Vector3D& basePoint,
    const Vector3D& height,
    const Vector3D& depth,
    double          halfWidth,
    double          apexAsymptoteDistance
) throw(bad_alloc) : Object() {
    if (!BU_SETJUMP) {
        BU_GETSTRUCT(m_internalp, rt_rhc_internal);
        m_internalp->rhc_magic = RT_RHC_INTERNAL_MAGIC;

        Set(basePoint, height, depth, halfWidth, apexAsymptoteDistance);
    }
    else {
        BU_UNSETJUMP;
        throw bad_alloc("BRLCAD::HyperbolicCylinder::HyperbolicCylinder");
    }

    BU_UNSETJUMP;
}


HyperbolicCylinder::HyperbolicCylinder
(
    const HyperbolicCylinder& original
) throw(bad_alloc) : Object(original) {
    if (!BU_SETJUMP) {
        BU_GETSTRUCT(m_internalp, rt_rhc_internal);
        memcpy(m_internalp, original.Internal(), sizeof(rt_rhc_internal));
    }
    else {
        BU_UNSETJUMP;
        throw bad_alloc("BRLCAD::HyperbolicCylinder::HyperbolicCylinder");
    }

    BU_UNSETJUMP;
}


HyperbolicCylinder::~HyperbolicCylinder(void) throw() {
    if (m_internalp != 0)
        bu_free(m_internalp, "BRLCAD::HyperbolicCylinder::~HyperbolicCylinder::m_internalp");
}


const HyperbolicCylinder& HyperbolicCylinder::operator =
(
    const HyperbolicCylinder& original
) throw(bad_alloc) {
    if(&original != this) {
        Copy(original);
        memcpy(Internal(), original.Internal(), sizeof(rt_rhc_internal));
    }

    return *this;
}


Vector3D HyperbolicCylinder::BasePoint(void) const throw() {
    return Vector3D(Internal()->rhc_V);
}


void HyperbolicCylinder::SetBasePoint
(
    const Vector3D& basePoint
) throw() {
    VMOVE(Internal()->rhc_V, basePoint.coordinates);
}


Vector3D HyperbolicCylinder::Height(void) const throw() {
    return Vector3D(Internal()->rhc_H);
}


void HyperbolicCylinder::SetHeight
(
    const Vector3D& height
) throw() {
    VMOVE(Internal()->rhc_H, height.coordinates);
}


Vector3D HyperbolicCylinder::Depth(void) const throw() {
    return Vector3D(Internal()->rhc_B);
}


void HyperbolicCylinder::SetDepth
(
    const Vector3D& depth
) throw() {
    VMOVE(Internal()->rhc_B, depth.coordinates);
}


double HyperbolicCylinder::HalfWidth(void) const throw() {
    return Internal()->rhc_r;
}


void HyperbolicCylinder::SetHalfWidth
(
    double length
) throw() {
    Internal()->rhc_r = length;
}


double HyperbolicCylinder::ApexAsymptoteDistance(void) const throw() {
    return Internal()->rhc_c;
}


void HyperbolicCylinder::SetApexAsymptoteDistance
(
    double distance
) throw() {
    Internal()->rhc_c = distance;
}


void HyperbolicCylinder::Set
(
    const Vector3D& basePoint,
    const Vector3D& height,
    const Vector3D& depth,
    double          halfWidth,
    double          apexAsymptoteDistance
) throw() {
    rt_rhc_internal* internalp = Internal();

    VMOVE(internalp->rhc_V, basePoint.coordinates);
    VMOVE(internalp->rhc_H, height.coordinates);
    VMOVE(internalp->rhc_B, depth.coordinates);
    internalp->rhc_r = halfWidth;
    internalp->rhc_c = apexAsymptoteDistance;
}


const Object& HyperbolicCylinder::operator=
(
    const Object& original
) throw(bad_alloc) {
    const HyperbolicCylinder* rhc = dynamic_cast<const HyperbolicCylinder*>(&original);
    assert(rhc != 0);

    if (rhc != 0)
        *this = *rhc;

    return *this;
}


Object* HyperbolicCylinder::Clone(void) const throw(bad_alloc, std::bad_alloc) {
    return new HyperbolicCylinder(*this);
}


const char* HyperbolicCylinder::ClassName(void) throw() {
    return "HyperbolicCylinder";
}


const char* HyperbolicCylinder::Type(void) const throw() {
    return ClassName();
}


bool HyperbolicCylinder::IsValid(void) const throw(){
    bool                   ret       = false;
    const rt_rhc_internal* internalp = Internal();

    if (!VNEAR_ZERO(internalp->rhc_H, SMALL_FASTF) &&
        !VNEAR_ZERO(internalp->rhc_B, SMALL_FASTF) &&
        (internalp->rhc_r > SMALL_FASTF) &&
        (internalp->rhc_c > SMALL_FASTF) &&
        NEAR_ZERO(VDOT(internalp->rhc_H, Internal()->rhc_B ), RT_DOT_TOL))
        ret = true;

    return ret;
}


HyperbolicCylinder::HyperbolicCylinder
(
    resource*       resp,
    directory*      pDir,
    rt_db_internal* ip,
    db_i*           dbip
) throw() : Object(resp, pDir, ip, dbip), m_internalp(0) {}


rt_rhc_internal* HyperbolicCylinder::Internal(void) throw() {
    rt_rhc_internal* ret;

    if(m_ip != 0)
        ret = static_cast<rt_rhc_internal*>(m_ip->idb_ptr);
    else
        ret = m_internalp;

    RT_RHC_CK_MAGIC(ret);

    return ret;
}


const rt_rhc_internal* HyperbolicCylinder::Internal(void) const throw() {
    const rt_rhc_internal* ret;

    if (m_ip != 0)
        ret = static_cast<const rt_rhc_internal*>(m_ip->idb_ptr);
    else
        ret = m_internalp;

    RT_RHC_CK_MAGIC(ret);

    return ret;
}
