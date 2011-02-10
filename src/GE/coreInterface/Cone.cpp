/*                      C O N E . C P P
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
/** @file Cone.cpp
 *
 *  BRL-CAD core C++ interface:
 *      truncated general cone (ID_TGC) database object implementation
 *
 *  Origin -
 *      TNO (Netherlands)
 *      IABG mbH (Germany)
 */

#include <cassert>

#include "rtgeom.h"
#include "raytrace.h"

#include <brlcad/Cone.h>


using namespace BRLCAD;


Cone::Cone(void) throw(bad_alloc) : Object () {
    if(!BU_SETJUMP){
        BU_GETSTRUCT(m_internalp, rt_tgc_internal);
        m_internalp->magic = RT_TGC_INTERNAL_MAGIC;

        Set(Vector3D(), Vector3D(0., 0., 1.), 1., 0.);
    }
    else{
        BU_UNSETJUMP;
        throw bad_alloc("BRLCAD::Cone::Cone");
    }

    BU_UNSETJUMP;
}


Cone::Cone
(
    const Vector3D& basePoint,
    const Vector3D& height,
    const Vector3D& semiPrincipalAxisA,
    const Vector3D& semiPrincipalAxisB,
    double          ratioCtoA,
    double          ratioDtoB
) throw(bad_alloc)
{
    if(!BU_SETJUMP){
        BU_GETSTRUCT(m_internalp, rt_tgc_internal);
        m_internalp->magic = RT_TGC_INTERNAL_MAGIC;

        Set(basePoint, height, semiPrincipalAxisA, semiPrincipalAxisB, ratioCtoA, ratioDtoB);
    }
    else{
        BU_UNSETJUMP;
        throw bad_alloc("BRLCAD::Cone::Cone");
    }

    BU_UNSETJUMP;

}


Cone::Cone
(
    const Vector3D& basePoint,
    const Vector3D& height,
    const Vector3D& semiPrincipalAxisA,
    const Vector3D& semiPrincipalAxisB,
    double          scale
) throw(bad_alloc)
{
    if(!BU_SETJUMP){
        BU_GETSTRUCT(m_internalp, rt_tgc_internal);
        m_internalp->magic = RT_TGC_INTERNAL_MAGIC;

        Set(basePoint, height, semiPrincipalAxisA, semiPrincipalAxisB, scale);
    }
    else{
        BU_UNSETJUMP;
        throw bad_alloc("BRLCAD::Cone::Cone");
    }

    BU_UNSETJUMP;

}


Cone::Cone
(
    const Vector3D& basePoint,
    const Vector3D& height,
    const Vector3D& semiPrincipalAxisA,
    const Vector3D& semiPrincipalAxisB
) throw(bad_alloc)
{
    if(!BU_SETJUMP){
        BU_GETSTRUCT(m_internalp, rt_tgc_internal);
        m_internalp->magic = RT_TGC_INTERNAL_MAGIC;

        Set(basePoint, height, semiPrincipalAxisA, semiPrincipalAxisB);
    }
    else{
        BU_UNSETJUMP;
        throw bad_alloc("BRLCAD::Cone::Cone");
    }

    BU_UNSETJUMP;

}


Cone::Cone
(
    const Vector3D& basePoint,
    const Vector3D& height,
    double          radiusBase,
    double          radiusTop
) throw(bad_alloc)
{
    if(!BU_SETJUMP){
        BU_GETSTRUCT(m_internalp, rt_tgc_internal);
        m_internalp->magic = RT_TGC_INTERNAL_MAGIC;

        Set(basePoint, height, radiusBase, radiusTop);
    }
    else{
        BU_UNSETJUMP;
        throw bad_alloc("BRLCAD::Cone::Cone");
    }

    BU_UNSETJUMP;

}


Cone::Cone
(
    const Vector3D& basePoint,
    const Vector3D& height,
    double          radius
) throw(bad_alloc)
{
    if(!BU_SETJUMP){
        BU_GETSTRUCT(m_internalp, rt_tgc_internal);
        m_internalp->magic = RT_TGC_INTERNAL_MAGIC;

        Set(basePoint, height, radius);
    }
    else{
        BU_UNSETJUMP;
        throw bad_alloc("BRLCAD::Cone::Cone");
    }

    BU_UNSETJUMP;

}


Cone::Cone
(
    const Cone& original
) throw(bad_alloc) : Object(original) {
    if (!BU_SETJUMP) {
        BU_GETSTRUCT(m_internalp, rt_tgc_internal);
        memcpy(m_internalp, original.Internal(), sizeof(rt_tgc_internal));
    }
    else {
        BU_UNSETJUMP;
        throw bad_alloc("BRLCAD::Cone::Cone");
    }

    BU_UNSETJUMP;
}


Cone::~Cone(void) throw() {
    if (m_internalp != 0)
        bu_free(m_internalp, "BRLCAD::Cone::~Cone::m_internalp");
}


const Cone& Cone::operator=
(
    const Cone& original
) throw(bad_alloc) {
    if(&original != this) {
        Copy(original);
        memcpy(Internal(), original.Internal(), sizeof(rt_tgc_internal));
    }

    return *this;
}


Vector3D Cone::BasePoint(void) const throw() {
    return Vector3D(Internal()->v);
}


void Cone::SetBasePoint
(
    const Vector3D& basePoint
) throw() {
    VMOVE(Internal()->v, basePoint.coordinates);
}


Vector3D Cone::Height(void) const throw() {
    return Vector3D(Internal()->h);
}


void Cone::SetHeight
(
    const Vector3D& height
) throw() {
    VMOVE(Internal()->h, height.coordinates);
}


Vector3D Cone::SemiPrincipalAxis
(
    size_t index
) const throw(){
    Vector3D ret;

    assert(index < 4);

    switch (index) {
        case 0:
            VMOVE(ret.coordinates, Internal()->a);
            break;

        case 1:
            VMOVE(ret.coordinates, Internal()->b);
            break;

        case 2:
            VMOVE(ret.coordinates, Internal()->c);
            break;

        case 3:
            VMOVE(ret.coordinates, Internal()->d);
    }

    return ret;
}


void Cone::SetSemiPrincipalAxis
(
    size_t          index,
    const Vector3D& semiPrincipalAxis
) throw(){
    assert(index < 4);

    switch (index) {
        case 0:
            VMOVE(Internal()->a, semiPrincipalAxis.coordinates);
            break;

        case 1:
            VMOVE(Internal()->b, semiPrincipalAxis.coordinates);
            break;

        case 2:
            VMOVE(Internal()->c, semiPrincipalAxis.coordinates);
            break;

        case 3:
            VMOVE(Internal()->d, semiPrincipalAxis.coordinates);
    }
}


void Cone::Set
(
    const Vector3D& basePoint,
    const Vector3D& height,
    const Vector3D& semiPrincipalAxisA,
    const Vector3D& semiPrincipalAxisB,
    double          ratioCtoA,
    double          ratioDtoB
) throw() {
    rt_tgc_internal* internalp = Internal();

    VMOVE(internalp->v, basePoint.coordinates);
    VMOVE(internalp->h, height.coordinates);
    VMOVE(internalp->a, semiPrincipalAxisA.coordinates);
    VMOVE(internalp->b, semiPrincipalAxisB.coordinates);
    VSCALE(internalp->c, internalp->a, ratioCtoA);
    VSCALE(internalp->d, internalp->b, ratioDtoB);

}


void Cone::Set
(
    const Vector3D& basePoint,
    const Vector3D& height,
    const Vector3D& semiPrincipalAxisA,
    const Vector3D& semiPrincipalAxisB,
    double          scale
) throw() {
    rt_tgc_internal* internalp = Internal();

    VMOVE(internalp->v, basePoint.coordinates);
    VMOVE(internalp->h, height.coordinates);
    VMOVE(internalp->a, semiPrincipalAxisA.coordinates);
    VMOVE(internalp->b, semiPrincipalAxisB.coordinates);
    VSCALE(internalp->c, internalp->a, scale);
    VSCALE(internalp->d, internalp->b, scale);

}


void Cone::Set
(
    const Vector3D& basePoint,
    const Vector3D& height,
    const Vector3D& semiPrincipalAxisA,
    const Vector3D& semiPrincipalAxisB
) throw() {
    rt_tgc_internal* internalp = Internal();

    VMOVE(internalp->v, basePoint.coordinates);
    VMOVE(internalp->h, height.coordinates);
    VMOVE(internalp->a, semiPrincipalAxisA.coordinates);
    VMOVE(internalp->b, semiPrincipalAxisB.coordinates);
    VMOVE(internalp->c, internalp->a);
    VMOVE(internalp->d, internalp->b);

}


void Cone::Set
(
    const Vector3D& basePoint,
    const Vector3D& height,
    double          radiusBase,
    double          radiusTop
) throw() {
    rt_tgc_internal* internalp = Internal();
    vect_t           semiPrincipalAxisA = {0.};
    vect_t           semiPrincipalAxisB = {0.};

    VMOVE(internalp->v, basePoint.coordinates);
    VMOVE(internalp->h, height.coordinates);

    bn_vec_ortho(semiPrincipalAxisA, internalp->h);
    VCROSS(semiPrincipalAxisB, semiPrincipalAxisA, internalp->h);
    VUNITIZE(semiPrincipalAxisB);

    VSCALE(internalp->a, semiPrincipalAxisA, radiusBase);
    VSCALE(internalp->b, semiPrincipalAxisB, radiusBase);
    VSCALE(internalp->c, semiPrincipalAxisA, radiusTop);
    VSCALE(internalp->d, semiPrincipalAxisB, radiusTop);
}


void Cone::Set
(
    const Vector3D& basePoint,
    const Vector3D& height,
    double          radius
) throw() {
    rt_tgc_internal* internalp = Internal();
    vect_t           semiPrincipalAxisA = {0.};
    vect_t           semiPrincipalAxisB = {0.};

    VMOVE(internalp->v, basePoint.coordinates);
    VMOVE(internalp->h, height.coordinates);

    bn_vec_ortho(semiPrincipalAxisA, internalp->h);
    VCROSS(semiPrincipalAxisB, semiPrincipalAxisA, internalp->h);
    VUNITIZE(semiPrincipalAxisB);

    VSCALE(internalp->a, semiPrincipalAxisA, radius);
    VSCALE(internalp->b, semiPrincipalAxisB, radius);
    VMOVE(internalp->c, internalp->a);
    VMOVE(internalp->d, internalp->b);
}


const Object& Cone::operator=
(
    const Object& original
) throw(bad_alloc) {
    const Cone* tgc = dynamic_cast<const Cone*>(&original);
    assert(tgc != 0);

    if (tgc != 0)
        *this = *tgc;

    return *this;
}


Object* Cone::Clone(void) const throw(bad_alloc, std::bad_alloc) {
    return new Cone(*this);
}


const char* Cone::ClassName(void) throw() {
    return "Cone";
}


const char* Cone::Type(void) const throw() {
    return ClassName();
}


bool Cone::IsValid(void) const throw(){
    bool                   ret       = false;
    double                 workScal = 0;
    const rt_tgc_internal* internalp = Internal();
        double magA = MAGNITUDE(internalp->a);
        double magB = MAGNITUDE(internalp->b);
        double magH = MAGNITUDE(internalp->h);

    if (!NEAR_ZERO(magA, RT_LEN_TOL) &&
        !NEAR_ZERO(magB, RT_LEN_TOL) &&
        !NEAR_ZERO(magH, RT_LEN_TOL) &&
        NEAR_ZERO(VDOT(internalp->a, internalp->b) / magA * magB, RT_DOT_TOL)) {
        double magC = MAGNITUDE(internalp->c);
        double magD = MAGNITUDE(internalp->d);

        if ((((magC * magD) <= SMALL) || NEAR_ZERO(VDOT(internalp->c, internalp->d), RT_DOT_TOL)) &&
            ((magC <= SMALL) || NEAR_ZERO(1. - VDOT(internalp->a, internalp->c) / (magA * magC), RT_DOT_TOL)) &&
            ((magD <= SMALL) || NEAR_ZERO(1. - VDOT(internalp->b, internalp->d) / (magB * magD), RT_DOT_TOL))) {
            vect_t aCrossB;
            VCROSS(aCrossB, internalp->a, internalp->b );

            ret = !NEAR_ZERO(VDOT(internalp->h, aCrossB ) / ( magA * magB * magH), RT_DOT_TOL);
        }
    }

    return ret;
}


Cone::Cone
(
    resource*       resp,
    directory*      pDir,
    rt_db_internal* ip,
    db_i*           dbip
) throw() : Object(resp, pDir, ip, dbip), m_internalp(0) {}


rt_tgc_internal* Cone::Internal(void) throw() {
    rt_tgc_internal* ret;

    if(m_ip != 0)
        ret = static_cast<rt_tgc_internal*>(m_ip->idb_ptr);
    else
        ret = m_internalp;

    RT_TGC_CK_MAGIC(ret);

    return ret;
}


const rt_tgc_internal* Cone::Internal(void) const throw() {
    const rt_tgc_internal* ret;

    if (m_ip != 0)
        ret = static_cast<const rt_tgc_internal*>(m_ip->idb_ptr);
    else
        ret = m_internalp;

    RT_TGC_CK_MAGIC(ret);

    return ret;
}
