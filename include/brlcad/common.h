/*                      C O M M O N . H
 * BRL-CAD
 *
 * Copyright (c) 2008-2009 United States Government as represented by
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

/// a define for the interface's binding
/** Meaningful values are for example __declspec(dllexport), __declspec(dllimport) or virtual.
    It has to be defined by the build environment (Makefile, *.vcproj etc.).
    The default is no special declaration. */
#ifndef BRLCAD_COREINTERFACE_EXPORT
#   define BRLCAD_COREINTERFACE_EXPORT
#endif


namespace BRLCAD {
    struct Vector2D {
        double coordinates[2];

        Vector2D(void) {
            coordinates[0] = 0.;
            coordinates[1] = 0.;
        }

        Vector2D(const double vector[2]) {
            coordinates[0] = vector[0];
            coordinates[1] = vector[1];
        }

        Vector2D(double x,
                 double y) {
            coordinates[0] = x;
            coordinates[1] = y;
        }
    };


    struct Mapping2D {
        Vector2D point;
        Vector2D delta;

        Mapping2D(const Vector2D& pt,
                  const Vector2D& dl) : point(pt), delta(dl) {}
    };


    struct Vector3D {
        double coordinates[3];

        Vector3D(void) {
            coordinates[0] = 0.;
            coordinates[1] = 0.;
            coordinates[2] = 0.;
        }

        Vector3D(const double vector[3]) {
            coordinates[0] = vector[0];
            coordinates[1] = vector[1];
            coordinates[2] = vector[2];
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
                    double          maxCurvature) {
            minPrincipalDirection = minDirection;
            minPrincipalCurvature = minCurvature;
            maxPrincipalCurvature = maxCurvature;
        }

        Curvature3D(void) : minPrincipalDirection(),
                            minPrincipalCurvature(0.),
                            maxPrincipalCurvature(0.) {}
    };


    class StringCalback {
    public:
        virtual ~StringCalback(void) {}

        /** return true: go on; false: stop
            The return value gives the calling function the possibility to optimize.
            However be aware the return value may be ignored. */
        virtual bool operator()(const char* string) = 0;

    protected:
        StringCalback(void) {}
        StringCalback(const StringCalback&) {}
        const StringCalback& operator=(const StringCalback&) {return *this;}
    };
}


#endif // BRLCAD_COMMON_INCLUDED
