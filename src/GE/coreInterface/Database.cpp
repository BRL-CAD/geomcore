/*                      D A T A B A S E . C P P
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
/** @file Database.cpp
 *
 *  BRL-CAD core C++ interface:
 *      implements the common part of the handles on a writable database
 *
 *  Origin -
 *      TNO (Netherlands)
 *      IABG mbH (Germany)
 */

#include <cassert>

#include "raytrace.h"
#include "rtgeom.h"

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
#include <brlcad/Database.h>

#if defined (_DEBUG)
#   define LOOKUP_NOISE LOOKUP_NOISY
#else
#   define LOOKUP_NOISE LOOKUP_QUIET
#endif


using namespace BRLCAD;


Database::~Database(void) throw() {
    if (m_wdbp != 0) {
        if (!BU_SETJUMP)
            wdb_close(m_wdbp);

        BU_UNSETJUMP;
    }
}


void Database::SetTitle
(
    const char* title
) throw(bad_alloc) {
    if (m_wdbp != 0) {
        if (!BU_SETJUMP)
            db_update_ident(m_wdbp->dbip, title, m_wdbp->dbip->dbi_base2local);
        else {
            BU_UNSETJUMP;
            throw bad_alloc("BRLCAD::Database::SetTitle");
        }

        BU_UNSETJUMP;
    }
}


bool Database::Add
(
    const Object& object
) throw() {
    bool ret = false;

    if (m_wdbp != 0) {
        if (!BU_SETJUMP) {
            int   id         = ID_NULL;
            void* rtInternal = 0;

            if (object.Type() == Torus::ClassName()) {
                id = ID_TOR; // 1

                const Torus* torus = dynamic_cast<const Torus*>(&object);

                assert(torus != 0);

                BU_GETSTRUCT(rtInternal, rt_tor_internal);
                memcpy(rtInternal, torus->Internal(), sizeof(rt_tor_internal));
            }
            else if (object.Type() == Cone::ClassName()) {
                id = ID_TGC; // 2

                const Cone* cone = dynamic_cast<const Cone*>(&object);

                assert(cone != 0);

                BU_GETSTRUCT(rtInternal, rt_tgc_internal);
                memcpy(rtInternal, cone->Internal(), sizeof(rt_tgc_internal));
            }
            else if (object.Type() == Ellipsoid::ClassName()) {
                id = ID_ELL; // 3

                const Ellipsoid* ellipsoid = dynamic_cast<const Ellipsoid*>(&object);

                assert(ellipsoid != 0);

                BU_GETSTRUCT(rtInternal, rt_ell_internal);
                memcpy(rtInternal, ellipsoid->Internal(), sizeof(rt_ell_internal));
            }
            else if (object.Type() == Arb8::ClassName()) {
                id = ID_ARB8; // 4

                const Arb8* arb8 = dynamic_cast<const Arb8*>(&object);

                assert(arb8 != 0);

                BU_GETSTRUCT(rtInternal, rt_arb_internal);
                memcpy(rtInternal, arb8->Internal(), sizeof(rt_arb_internal));
            }
            else if (object.Type() == Halfspace::ClassName()) {
                id = ID_HALF; // 6

                const Halfspace* halfspace = dynamic_cast<const Halfspace*>(&object);

                assert(halfspace != 0);

                BU_GETSTRUCT(rtInternal, rt_half_internal);
                memcpy(rtInternal, halfspace->Internal(), sizeof(rt_half_internal));
            }
            else if (object.Type() == Particle::ClassName()) {
                id = ID_PARTICLE; // 16

                const Particle* particle = dynamic_cast<const Particle*>(&object);

                assert(particle != 0);

                BU_GETSTRUCT(rtInternal, rt_part_internal);
                memcpy(rtInternal, particle->Internal(), sizeof(rt_part_internal));
            }
            else if (object.Type() == ParabolicCylinder::ClassName()) {
                id = ID_RPC; // 17

                const ParabolicCylinder* parabolicCylinder = dynamic_cast<const ParabolicCylinder*>(&object);

                assert(parabolicCylinder != 0);

                BU_GETSTRUCT(rtInternal, rt_rpc_internal);
                memcpy(rtInternal, parabolicCylinder->Internal(), sizeof(rt_rpc_internal));
            }
            else if (object.Type() == HyperbolicCylinder::ClassName()) {
                id = ID_RHC; // 18

                const HyperbolicCylinder* hyperbolicCylinder = dynamic_cast<const HyperbolicCylinder*>(&object);

                assert(hyperbolicCylinder != 0);

                BU_GETSTRUCT(rtInternal, rt_rhc_internal);
                memcpy(rtInternal, hyperbolicCylinder->Internal(), sizeof(rt_rhc_internal));
            }
            else if (object.Type() == Paraboloid::ClassName()) {
                id = ID_EPA; // 19

                const Paraboloid* paraboloid = dynamic_cast<const Paraboloid*>(&object);

                assert(paraboloid != 0);

                BU_GETSTRUCT(rtInternal, rt_epa_internal);
                memcpy(rtInternal, paraboloid->Internal(), sizeof(rt_epa_internal));
            }
            else if (object.Type() == Hyperboloid::ClassName()) {
                id = ID_EHY; // 20

                const Hyperboloid* hyperboloid = dynamic_cast<const Hyperboloid*>(&object);

                assert(hyperboloid != 0);

                BU_GETSTRUCT(rtInternal, rt_ehy_internal);
                memcpy(rtInternal, hyperboloid->Internal(), sizeof(rt_ehy_internal));
            }
            else if (object.Type() == EllipticalTorus::ClassName()) {
                id = ID_ETO; // 21

                const EllipticalTorus* ellipticalTorus = dynamic_cast<const EllipticalTorus*>(&object);

                assert(ellipticalTorus != 0);

                BU_GETSTRUCT(rtInternal, rt_eto_internal);
                memcpy(rtInternal, ellipticalTorus->Internal(), sizeof(rt_eto_internal));
            }
            else if (object.Type() == Combination::ClassName()) {
                id = ID_COMBINATION;  // 31

                const Combination* combination = dynamic_cast<const Combination*>(&object);

                assert(combination != 0);

                const rt_comb_internal* internalFrom = combination->Internal();

                BU_GETSTRUCT(rtInternal, rt_comb_internal);
                memcpy(rtInternal, internalFrom, sizeof(rt_comb_internal));

                rt_comb_internal* internalTo = static_cast<rt_comb_internal*>(rtInternal);

                if (internalFrom->tree != 0)
                    internalTo->tree = db_dup_subtree(internalFrom->tree, object.m_resp);

                bu_vls_strcpy(&internalTo->shader, bu_vls_addr(&internalFrom->shader));
                bu_vls_strcpy(&internalTo->material, bu_vls_addr(&internalFrom->material));
            }

            if (id != ID_NULL) {
                ret = (wdb_export(m_wdbp, object.Name(), rtInternal, id, 1.) == 0);

                // copy attributes
                if (ret) {
                    const bu_attribute_value_set* origAvs = object.GetAvs();

                    if ((origAvs != 0) && (origAvs->count > 0)) {
                        directory* pDir = db_lookup(m_rtip->rti_dbip, object.Name(), LOOKUP_NOISE);

                        assert(pDir != 0);

                        if (pDir != 0) {
                            bu_attribute_value_set avs;
                            bu_avs_init(&avs, origAvs->count, "BRLCAD::Database::Add");

                            for (size_t i = 0; i < origAvs->count; ++i)
                                bu_avs_add_nonunique(&avs, origAvs->avp->name, origAvs->avp->value);

                            ret = (db5_replace_attributes(pDir, &avs, m_rtip->rti_dbip) == 0);
                        }
                    }
                }

            }
        }

        BU_UNSETJUMP;
    }

    return ret;
}


void Database::Delete
(
    const char* objectName
) throw() {
    if (m_wdbp != 0) {
        if (!BU_SETJUMP) {
            directory* pDir = db_lookup(m_rtip->rti_dbip, objectName, LOOKUP_NOISE);

            if (pDir != RT_DIR_NULL) {
                if (db_delete(m_wdbp->dbip, pDir) == 0)
                    db_dirdelete(m_wdbp->dbip, pDir);
            }
        }

        BU_UNSETJUMP;
    }
}


void Database::Get
(
    const char*     objectName,
    ObjectCallback& callback
) {
    class ObjectCallbackIntern : public ConstDatabase::ObjectCallback {
    public:
        ObjectCallbackIntern(Database::ObjectCallback& callback) : ConstDatabase::ObjectCallback(),
                                                                   m_callback(callback) {}

        virtual ~ObjectCallbackIntern(void) throw() {}

        virtual void operator()(const Object& object) {
            Object& objectIntern = const_cast<Object&>(object);

            m_callback(objectIntern);

            if (!BU_SETJUMP)
                rt_db_put_internal(objectIntern.m_pDir,
                                   objectIntern.m_dbip,
                                   objectIntern.m_ip,
                                   objectIntern.m_resp);

            BU_UNSETJUMP;
        }

    private:
        Database::ObjectCallback& m_callback;
    } callbackIntern(callback);

    ConstDatabase::Get(objectName, callbackIntern);
}


void Database::Set
(
    const Object& object
) throw(bad_alloc) {
    class ObjectCallbackIntern : public ObjectCallback {
    public:
        ObjectCallbackIntern(const Object& object) : ObjectCallback(),
                                                     m_object(object) {}

        virtual ~ObjectCallbackIntern(void) throw() {}

        virtual void operator()(Object& object) {
            object = m_object;
        }

    private:
        const Object& m_object;
    } callbackIntern(object);

    Get(object.Name(), callbackIntern);
}


Database::Database(void) throw(bad_alloc) : ConstDatabase(), m_wdbp(0) {}
