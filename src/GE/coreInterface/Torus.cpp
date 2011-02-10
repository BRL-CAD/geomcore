/*                      T O R U S . C P P
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
/** @file Torus.cpp
 *
 *  BRL-CAD core C++ interface:
 *      torus (ID_TOR) database object implementation
 *
 *  Origin -
 *      TNO (Netherlands)
 *      IABG mbH (Germany)
 */

#include <cassert>

#include "rtgeom.h"
#include "raytrace.h"

#include<brlcad/Torus.h>


using namespace BRLCAD;


Torus::Torus(void) throw(bad_alloc) : Object() {
    if (!BU_SETJUMP) {
        BU_GETSTRUCT(m_internalp, rt_tor_internal);
        m_internalp->magic = RT_TOR_INTERNAL_MAGIC;

        Set(Vector3D(), Vector3D(0,0,1), 2, 1);
    }
    else {
        BU_UNSETJUMP;
        throw bad_alloc("BRLCAD::Torus::Torus");
    }

    BU_UNSETJUMP;
}


Torus::Torus
(
    const Vector3D& center,
    const Vector3D& normalToTubeCenterLinePlane,
    double          tubeCenterLineRadius,
    double          tubeRadius
) throw(bad_alloc) : Object() {
    if (!BU_SETJUMP) {
        BU_GETSTRUCT(m_internalp, rt_tor_internal);
        m_internalp->magic = RT_TOR_INTERNAL_MAGIC;

        Set(center, normalToTubeCenterLinePlane, tubeCenterLineRadius, tubeRadius);
    }
    else {
        BU_UNSETJUMP;
        throw bad_alloc("BRLCAD::Torus::Torus");
    }

    BU_UNSETJUMP;
}


Torus::Torus
(
    const Torus& original
) throw(bad_alloc) : Object(original) {
    if (!BU_SETJUMP) {
        BU_GETSTRUCT(m_internalp, rt_tor_internal);
        memcpy(m_internalp, original.Internal(), sizeof(rt_tor_internal));
    }
    else {
        BU_UNSETJUMP;
        throw bad_alloc("BRLCAD::Torus::Torus");
    }

    BU_UNSETJUMP;
}


Torus::~Torus(void) throw() {
    if (m_internalp != 0)
        bu_free(m_internalp, "BRLCAD::Torus::~Torus::m_internalp");
}


const Torus& Torus::operator =
(
    const Torus& original
) throw(bad_alloc) {
    if (&original != this) {
        Copy(original);
        memcpy(Internal(), original.Internal(), sizeof(rt_tor_internal));
    }

    return *this;
}


Vector3D Torus::Center(void) const throw() {
    return Vector3D(Internal()->v);
}


void Torus::SetCenter
(
    const Vector3D& center
) throw() {
    VMOVE(Internal()->v, center.coordinates);
}


Vector3D Torus::Normal(void) const throw(){
    return Vector3D(Internal()->h);
}


void Torus::SetNormal
(
    const Vector3D& normal
) throw() {
    assert(!VNEAR_ZERO(normal.coordinates, SMALL_FASTF));

    VMOVE(Internal()->h, normal.coordinates);
    VUNITIZE(Internal()->h);
}


double Torus::TubeCenterLineRadius(void) const throw() {
    return Internal()->r_a;
}


void Torus::SetTubeCenterLineRadius
(
    double radius
) throw(){
    Internal()->r_a = radius;
}


double Torus::TubeRadius(void) const throw(){
    return Internal()->r_h;
}


void Torus::SetTubeRadius
(
    double radius
) throw() {
    Internal()->r_h = radius;
}


void Torus::Set
(
    const Vector3D& center,
    const Vector3D& normalToTubeCenterLinePlane,
    double          tubeCenterLineRadius,
    double          tubeRadius
) throw() {
        rt_tor_internal* internalp = Internal();

        VMOVE(internalp->v, center.coordinates);

        assert(!VNEAR_ZERO(normalToTubeCenterLinePlane.coordinates, SMALL_FASTF));
        VMOVE(internalp->h, normalToTubeCenterLinePlane.coordinates);
        VUNITIZE(internalp->h);

        internalp->r_a = tubeCenterLineRadius;
        internalp->r_h = tubeRadius;
}


const Object& Torus::operator=
(
    const Object& original
) throw(bad_alloc) {
    const Torus* tor = dynamic_cast<const Torus*>(&original);
    assert(tor != 0);

    if (tor != 0)
        *this = *tor;

    return *this;
}


Object* Torus::Clone(void) const throw(bad_alloc, std::bad_alloc) {
    return new Torus(*this);
}


const char* Torus::ClassName(void) throw() {
    return "Torus";
}


const char* Torus::Type(void) const throw() {
    return ClassName();
}


bool Torus::IsValid(void) const throw() {
    bool                   ret       = false;
    const rt_tor_internal* internalp = Internal();

    if (!NEAR_ZERO(internalp->r_a, SMALL_FASTF) &&
        !NEAR_ZERO(internalp->r_h, SMALL_FASTF) &&
        !BN_VEC_NON_UNIT_LEN(internalp->h) &&
        (internalp->r_a >= internalp->r_h))
        ret = true;

    return ret;
}


Torus::Torus
(
    resource*       resp,
    directory*      pDir,
    rt_db_internal* ip,
    db_i*           dbip
) throw() : Object(resp, pDir, ip, dbip), m_internalp(0) {}


const rt_tor_internal* Torus::Internal(void) const throw() {
    const rt_tor_internal* ret;

    if (m_ip != 0)
        ret = static_cast<const rt_tor_internal*>(m_ip->idb_ptr);
    else
        ret = m_internalp;

    RT_TOR_CK_MAGIC(ret);

    return ret;
}


rt_tor_internal* Torus::Internal(void) throw() {
    rt_tor_internal* ret;

    if(m_ip != 0)
        ret = static_cast<rt_tor_internal*>(m_ip->idb_ptr);
    else
        ret = m_internalp;

    RT_TOR_CK_MAGIC(ret);

    return ret;
}
