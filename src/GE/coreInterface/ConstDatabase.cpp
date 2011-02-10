/*                      C O N S T D A T A B A S E . C P P
 * BRL-CAD
 *
 * Copyright (c) 2008-2011 United States Government as represented by
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
/** @file ConstDatabase.cpp
 *
 *  BRL-CAD core C++ interface:
 *      implements a handle on a read-only database
 *
 *  Origin -
 *      TNO (Netherlands)
 *      IABG mbH (Germany)
 */

#include <cstdlib>
#include <cstring>
#include <cassert>

#include "raytrace.h"

#include <brlcad/Torus.h>
#include <brlcad/Cone.h>
#include <brlcad/Ellipsoid.h>
#include <brlcad/Arb8.h>
#include <brlcad/Halfspace.h>
#include <brlcad/Particle.h>
#include <brlcad/ParabolicCylinder.h>
#include <brlcad/HyperbolicCylinder.h>
#include <brlcad/Paraboloid.h>
#include <brlcad/Hyperboloid.h>
#include <brlcad/EllipticalTorus.h>
#include <brlcad/Combination.h>
#include <brlcad/Unknown.h>
#include <brlcad/ConstDatabase.h>

#if defined (_DEBUG)
#   define LOOKUP_NOISE LOOKUP_NOISY
#else
#   define LOOKUP_NOISE LOOKUP_QUIET
#endif


using namespace BRLCAD;


static int NullLogger
(
    genptr_t data,
    genptr_t string
) {
    return 0;
}


static void InitBrlCad(void) throw() {
    static bool init = true;

    if (init) { // do it only once
        init = false;

        if (!BU_SETJUMP)
            bu_log_add_hook(NullLogger, 0);

        BU_UNSETJUMP;
    }
}


//
// class ConstDatabase
//

ConstDatabase::ConstDatabase(void) throw(bad_alloc) : m_rtip(0), m_resp(0) {
    InitBrlCad();

    if (rt_uniresource.re_magic != RESOURCE_MAGIC)
        rt_init_resource(&rt_uniresource, 0, NULL);

    if (!BU_SETJUMP) {
        m_resp = static_cast<resource*>(bu_calloc(1, sizeof(resource), "BRLCAD::ConstDatabase::~ConstDatabase::m_resp"));
        rt_init_resource(m_resp, 0, NULL);
    }
    else {
        BU_UNSETJUMP;
        throw bad_alloc("BRLCAD::Object::Object");
    }

    BU_UNSETJUMP;
}


ConstDatabase::~ConstDatabase(void) throw() {
    if (m_rtip != 0) {
        if (!BU_SETJUMP)
            rt_free_rti(m_rtip);

        BU_UNSETJUMP;
    }

    if (m_resp != 0) {
        rt_clean_resource_complete(0, m_resp);
        bu_free(m_resp, "BRLCAD::ConstDatabase::~ConstDatabase::m_resp");
    }
}


bool ConstDatabase::Load
(
    const char* fileName
) throw() {
    if (m_resp != 0) {
        if (m_rtip != 0) {
            if (!BU_SETJUMP)
                rt_free_rti(m_rtip);

            BU_UNSETJUMP;

            m_rtip = 0;
        }

        if (!BU_SETJUMP)
            m_rtip = rt_dirbuild(fileName, 0, 0);

        BU_UNSETJUMP;

        if (m_rtip != 0) {
            if (!BU_SETJUMP)
                rt_init_resource(m_resp, 0, m_rtip);
            else {
                BU_UNSETJUMP;

                if (!BU_SETJUMP)
                    rt_free_rti(m_rtip);

                m_rtip = 0;
            }

            BU_UNSETJUMP;
        }
    }

    return (m_rtip != 0);
}


const char* ConstDatabase::Title(void) const throw() {
    const char* ret = 0;

    if (m_rtip != 0)
        ret = m_rtip->rti_dbip->dbi_title;

    return ret;
}


const ConstDatabase::TopObjectIterator& ConstDatabase::TopObjectIterator::operator++(void) throw() {
    if (m_pDir != 0) {
        const directory* oldPDir = m_pDir;

        for (const directory* pDir = m_pDir->d_forw; pDir != RT_DIR_NULL; pDir = pDir->d_forw) {
            if (pDir->d_nref == 0) {
                m_pDir = pDir;
                break;
            }
        }

        if (m_pDir == oldPDir) {
            for (size_t i = m_hashTablePosition + 1; (i < RT_DBNHASH) && (m_pDir == oldPDir); ++i) {
                for (const directory* pDir = m_rtip->rti_dbip->dbi_Head[i]; pDir != RT_DIR_NULL; pDir = pDir->d_forw) {
                    if (pDir->d_nref == 0) {
                        m_hashTablePosition = i;
                        m_pDir              = pDir;
                        break;
                    }
                }
            }
        }

        if (m_pDir == oldPDir)
            m_pDir = 0;
    }

    return *this;
}


bool ConstDatabase::TopObjectIterator::Good(void) const throw() {
    return (m_pDir != 0);
}


const char* ConstDatabase::TopObjectIterator::Name(void) const throw() {
    assert(m_pDir != 0);

    const char* ret = 0;

    if (m_pDir != 0)
        ret = m_pDir->d_namep;

    return ret;
}


ConstDatabase::TopObjectIterator::TopObjectIterator
(
    size_t           hashTablePosition,
    const directory* pDir,
    const rt_i*      rtip
) throw() : m_hashTablePosition(hashTablePosition), m_pDir(pDir), m_rtip(rtip) {}


ConstDatabase::TopObjectIterator ConstDatabase::FirstTopObject(void) const throw() {
    size_t           hashTablePosition = 0;
    const directory* pDirectory        = 0;

    if (m_rtip != 0) {
        if (!BU_SETJUMP) {
            db_update_nref(m_rtip->rti_dbip, m_resp);

            for (size_t i = 0; (i < RT_DBNHASH) && (pDirectory == 0); ++i) {
                for (const directory* pDir = m_rtip->rti_dbip->dbi_Head[i]; pDir != RT_DIR_NULL; pDir = pDir->d_forw) {
                    if (pDir->d_nref == 0) {
                        hashTablePosition = i;
                        pDirectory        = pDir;
                        break;
                    }
                }
            }
        }

        BU_UNSETJUMP;
    }

    return ConstDatabase::TopObjectIterator(hashTablePosition, pDirectory, m_rtip);
}


void ConstDatabase::Get
(
    const char*     objectName,
    ObjectCallback& callback
) const {
    if (m_rtip != 0) {
        if (!BU_SETJUMP) {
            if ((objectName != 0) && (strlen(objectName) > 0)) {
                directory* pDir = db_lookup(m_rtip->rti_dbip, objectName, LOOKUP_NOISE);

                if (pDir != RT_DIR_NULL) {
                    rt_db_internal intern;
                    int            id = rt_db_get_internal(&intern, pDir, m_rtip->rti_dbip, 0, m_resp);

                    try {
                        switch(id) {
                        case ID_TOR: // 1
                            callback(Torus(m_resp, pDir, &intern, m_rtip->rti_dbip));
                            break;

                        case ID_TGC: // 2
                            callback(Cone(m_resp, pDir, &intern, m_rtip->rti_dbip));
                            break;

                        case ID_ELL: // 3
                            callback(Ellipsoid(m_resp, pDir, &intern, m_rtip->rti_dbip));
                            break;

                        case ID_ARB8: // 4
                            callback(Arb8(m_resp, pDir, &intern, m_rtip->rti_dbip));
                            break;

                        case ID_HALF: // 6
                            callback(Halfspace(m_resp, pDir, &intern, m_rtip->rti_dbip));
                            break;

                        case ID_PARTICLE: // 16
                            callback(Particle(m_resp, pDir, &intern, m_rtip->rti_dbip));
                            break;

                        case ID_RPC: // 17
                            callback(ParabolicCylinder(m_resp, pDir, &intern, m_rtip->rti_dbip));
                            break;

                        case ID_RHC: // 18
                            callback(HyperbolicCylinder(m_resp, pDir, &intern, m_rtip->rti_dbip));
                            break;

                        case ID_EPA: // 19
                            callback(Paraboloid(m_resp, pDir, &intern, m_rtip->rti_dbip));
                            break;

                        case ID_EHY: // 20
                            callback(Hyperboloid(m_resp, pDir, &intern, m_rtip->rti_dbip));
                            break;

                        case ID_ETO: // 21
                            callback(EllipticalTorus(m_resp, pDir, &intern, m_rtip->rti_dbip));
                            break;

                        case ID_COMBINATION: // 31
                            callback(Combination(m_resp, pDir, &intern, m_rtip->rti_dbip));
                            break;

                        default:
                            callback(Unknown(m_resp, pDir, &intern, m_rtip->rti_dbip));
                        }
                    }
                    catch(...) {
                        BU_UNSETJUMP;
                        rt_db_free_internal(&intern);
                        throw;
                    }

                    rt_db_free_internal(&intern);
                }
            }
        }

        BU_UNSETJUMP;
    }
}


Object* ConstDatabase::Get
(
    const char* objectName
) const {
    class ObjectCallbackIntern : public ObjectCallback {
    public:
        ObjectCallbackIntern(void) : ConstDatabase::ObjectCallback(),
                                     m_object(0) {}

        virtual ~ObjectCallbackIntern(void) throw() {}

        virtual void operator()(const Object& object) {
            try {
                m_object = object.Clone();
            }
            catch(std::bad_alloc&) {}
        }

        Object*      GetObject(void) const throw() {
            return m_object;
        }

    private:
        Object* m_object;
    } callbackIntern;

    Get(objectName, callbackIntern);

    return callbackIntern.GetObject();
}


void ConstDatabase::Select
(
    const char* objectName
) throw() {
    if (m_rtip != 0) {
        if (!BU_SETJUMP)
            rt_gettree(m_rtip, objectName);

        BU_UNSETJUMP;
    }
}


void ConstDatabase::UnSelectAll(void) throw() {
    if (m_rtip != 0) {
        if (!BU_SETJUMP)
            rt_clean(m_rtip);

        BU_UNSETJUMP;
    }
}


bool ConstDatabase::SelectionIsEmpty(void) const throw() {
    bool ret = true;

    if (m_rtip != 0)
        ret = m_rtip->nsolids < 1;

    return ret;
}


Vector3D ConstDatabase::BoundingBoxMinima(void) const throw() {
    Vector3D ret;

    if (!SelectionIsEmpty()) {
        if (!BU_SETJUMP) {
            if (m_rtip->needprep)
                rt_prep(m_rtip);

            VMOVE(ret.coordinates, m_rtip->mdl_min);
        }

        BU_UNSETJUMP;
    }

    return ret;
}


Vector3D ConstDatabase::BoundingBoxMaxima(void) const throw() {
    Vector3D ret;

    if (!SelectionIsEmpty()) {
        if (!BU_SETJUMP) {
            if (m_rtip->needprep)
                rt_prep(m_rtip);

            VMOVE(ret.coordinates, m_rtip->mdl_max);
        }

        BU_UNSETJUMP;
    }

    return ret;
}


class ConstDatabaseHit : public ConstDatabase::Hit {
public:
    ConstDatabaseHit(application* ap,
                     partition*   part) throw() : ConstDatabase::Hit(),
                                                  m_application(ap),
                                                  m_partition(part),
                                                  m_inVectorsComputed(false),
                                                  m_outVectorsComputed(false) {
        assert(m_application != 0);
        assert(m_partition != 0);
   }

    virtual const char* Name(void) const throw() {
        return m_partition->pt_regionp->reg_name;
    }

    virtual double      DistanceIn(void) const throw() {
        return m_partition->pt_inhit->hit_dist;
    }

    virtual double      DistanceOut(void) const throw() {
        return m_partition->pt_outhit->hit_dist;
    }

    virtual Vector3D    PointIn(void) const throw() {
        ComputeInVectors();
        return m_partition->pt_inhit->hit_point;
    }

    virtual Vector3D    PointOut(void) const throw() {
        ComputeOutVectors();
        return m_partition->pt_outhit->hit_point;
    }

    virtual Vector3D    SurfaceNormalIn(void) const throw() {
        ComputeInVectors();
        return m_partition->pt_inhit->hit_normal;
    }

    virtual Vector3D    SurfaceNormalOut(void) const throw() {
        ComputeOutVectors();
        return m_partition->pt_outhit->hit_normal;
    }

    virtual Curvature3D SurfaceCurvatureIn(void) const throw() {
        ComputeInVectors();

        curvature curv;
        RT_CURVATURE(&curv, m_partition->pt_inhit, m_partition->pt_inflip, m_partition->pt_inseg->seg_stp);

        return Curvature3D(curv.crv_pdir, curv.crv_c1, curv.crv_c2);
    }

    virtual Curvature3D SurfaceCurvatureOut(void) const throw() {
        ComputeOutVectors();

        curvature curv;
        RT_CURVATURE(&curv, m_partition->pt_outhit, m_partition->pt_outflip, m_partition->pt_outseg->seg_stp);

        return Curvature3D(curv.crv_pdir, curv.crv_c1, curv.crv_c2);
    }

    virtual Mapping2D   Surface2DMappingIn(void) const throw() {
        uvcoord uv;
        RT_HIT_UVCOORD(m_application, m_partition->pt_inseg->seg_stp, m_partition->pt_inhit, &uv);

        return Mapping2D(Vector2D(uv.uv_u, uv.uv_v), Vector2D(uv.uv_du, uv.uv_dv));
    }

    virtual Mapping2D   Surface2DMappingOut(void) const throw() {
        uvcoord uv;
        RT_HIT_UVCOORD(m_application, m_partition->pt_outseg->seg_stp, m_partition->pt_outhit, &uv);

        return Mapping2D(Vector2D(uv.uv_u, uv.uv_v), Vector2D(uv.uv_du, uv.uv_dv));
    }

    virtual bool        HasColor(void) const throw() {
        return (m_partition->pt_regionp->reg_mater.ma_color_valid != 0);
    }

    virtual double      Red(void) const throw() {
        return m_partition->pt_regionp->reg_mater.ma_color[0];
    }

    virtual double      Green(void) const throw() {
        return m_partition->pt_regionp->reg_mater.ma_color[1];
    }

    virtual double      Blue(void) const throw() {
        return m_partition->pt_regionp->reg_mater.ma_color[2];
    }

private:
    application* m_application;
    partition*   m_partition;
    mutable bool m_inVectorsComputed;
    mutable bool m_outVectorsComputed;

    void ComputeInVectors(void) const throw() {
        if (!m_inVectorsComputed) {
            hit* pHit = m_partition->pt_inhit;

            RT_HIT_NORMAL(pHit->hit_normal, pHit, m_partition->pt_inseg->seg_stp, 0, m_partition->pt_inflip);
            m_inVectorsComputed = true;
        }
    }

    void ComputeOutVectors(void) const throw() {
        if (!m_outVectorsComputed) {
            hit* pHit = m_partition->pt_outhit;

            RT_HIT_NORMAL(pHit->hit_normal, pHit, m_partition->pt_outseg->seg_stp, 0, m_partition->pt_outflip);
            m_outVectorsComputed = true;
        }
    }
};


static int HitDo
(
    application* ap,
    partition*   partitionHead,
    seg*         segment
) {
    ConstDatabase::HitCallback* callback = static_cast<ConstDatabase::HitCallback*>(ap->a_uptr);

    for (partition* part = partitionHead->pt_forw;
         part != partitionHead;
         part = part->pt_forw) {
        if (!((*callback)(ConstDatabaseHit(ap, part))))
            break;
    }

    return 0; // return won't be evaluated
}


void ConstDatabase::ShootRay
(
    const Ray3D& ray,
    HitCallback& callback
) const {
    if (!SelectionIsEmpty()) {
        application ap = {0};

        ap.a_hit      = HitDo;
        ap.a_miss     = 0;
        ap.a_overlap  = 0;
        ap.a_rt_i     = m_rtip;
        ap.a_level    = 0;
        ap.a_onehit   = 0; // all hits
        ap.a_resource = m_resp;
        ap.a_uptr     = &callback;

        VMOVE(ap.a_ray.r_pt, ray.origin.coordinates);
        VMOVE(ap.a_ray.r_dir, ray.direction.coordinates);
        VUNITIZE(ap.a_ray.r_dir);

        if (!BU_SETJUMP) {
            try {
                rt_shootray(&ap);
            }
            catch(...) {
                BU_UNSETJUMP;
                throw;
            }
        }

        BU_UNSETJUMP;
    }
}
