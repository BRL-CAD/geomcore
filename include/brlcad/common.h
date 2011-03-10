/*                      C O M M O N . H
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
/** @file Common.h
 *
 *  BRL-CAD core C++ interface:
 *      declares common understanding
 *
 *  Origin -
 *      TNO (Netherlands)
 *      IABG mbH (Germany)
 */

#ifndef BRLCAD_COMMON_INCLUDED
#define BRLCAD_COMMON_INCLUDED

#include <new>

#ifdef _MSC_VER // Microsoft Visual C++ compiler
#   pragma warning(disable: 4290) // C++ exception specification ignored
#endif

/// a define for the interface's binding
/** Meaningful values are for example __declspec(dllexport) or __declspec(dllimport).
    It has to be defined by the build environment (Makefile, *.vcproj etc.).
    The default is no special declaration. */
#ifndef BRLCAD_COREINTERFACE_EXPORT
#   define BRLCAD_COREINTERFACE_EXPORT
#endif


namespace BRLCAD {
    struct Vector2D {
        double coordinates[2];

        Vector2D(void) throw() {
            coordinates[0] = 0.;
            coordinates[1] = 0.;
        }

        Vector2D(const double vector[2]) throw() {
            coordinates[0] = vector[0];
            coordinates[1] = vector[1];
        }

        Vector2D(double x,
                 double y) throw() {
            coordinates[0] = x;
            coordinates[1] = y;
        }
    };


    struct Mapping2D {
        Vector2D point;
        Vector2D delta;

        Mapping2D(const Vector2D& pt,
                  const Vector2D& dl) throw() : point(pt), delta(dl) {}
    };


    struct Vector3D {
        double coordinates[3];

        Vector3D(void) throw() {
            coordinates[0] = 0.;
            coordinates[1] = 0.;
            coordinates[2] = 0.;
        }

        Vector3D(const double vector[3]) throw() {
            coordinates[0] = vector[0];
            coordinates[1] = vector[1];
            coordinates[2] = vector[2];
        }

        Vector3D(double x,
                 double y,
                 double z) throw() {
            coordinates[0] = x;
            coordinates[1] = y;
            coordinates[2] = z;
        }
    };


    struct Ray3D {
        Vector3D origin;
        Vector3D direction;
    };


    struct Curvature3D {
        Vector3D minPrincipalDirection;
        double   minPrincipalCurvature;
        double   maxPrincipalCurvature;

        Curvature3D(const Vector3D& minDirection,
                    double          minCurvature,
                    double          maxCurvature) throw() {
            minPrincipalDirection = minDirection;
            minPrincipalCurvature = minCurvature;
            maxPrincipalCurvature = maxCurvature;
        }

        Curvature3D(void) throw() : minPrincipalDirection(),
                                    minPrincipalCurvature(0.),
                                    maxPrincipalCurvature(0.) {}
    };


    class bad_alloc : public std::bad_alloc {
    public:
        bad_alloc(const char* hint) throw() : std::bad_alloc(), m_hint(hint) {}

        bad_alloc(const bad_alloc& original) throw() : std::bad_alloc(original), m_hint(original.m_hint) {}

        virtual const bad_alloc& operator=(const bad_alloc& original) throw() {
            std::bad_alloc::operator=(original);
            m_hint = original.m_hint;

            return *this;
        }

        virtual const char* what(void) const throw() {
            return m_hint;
        }

    private:
        const char* m_hint;

        bad_alloc(void); // not implemented
    };
}


#endif // BRLCAD_COMMON_INCLUDED

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
