/*                    P A R T I C L E . H
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
/** @file Particle.h
 *
 *  BRL-CAD core C++ interface:
 *      particle (ID_PARTICLE) database object declaration
 *
 *  Origin -
 *      TNO (Netherlands)
 *      IABG mbH (Germany)
 */

#ifndef BRLCAD_PARTICLE_INCLUDED
#define BRLCAD_PARTICLE_INCLUDED

#include <brlcad/Object.h>


struct rt_part_internal;


namespace BRLCAD {
    class BRLCAD_COREINTERFACE_EXPORT Particle : public Object {
    public:
        Particle(void) throw(bad_alloc);  ///< constructs a particle at the origin with hight (0, 0, 1) and radii 1
        Particle(const Vector3D& basePoint,
                 const Vector3D& height,
                 double          baseRadius,
                 double          topRadius) throw(bad_alloc);
        Particle(const Particle& original) throw(bad_alloc);
        virtual ~Particle(void) throw();

        const Particle&       operator=(const Particle& original) throw(bad_alloc);

        Vector3D              BasePoint(void) const throw();
        void                  SetBasePoint(const Vector3D& basePoint) throw();

        Vector3D              Height(void) const throw();
        void                  SetHeight(const Vector3D& height) throw();

        double                BaseRadius(void) const throw();
        void                  SetBaseRadius(double baseRadius) throw();

        double                TopRadius(void) const throw();
        void                  SetTopRadius(double topRadius) throw();

        void                  Set(const Vector3D& basePoint,
                                  const Vector3D& height,
                                  double          baseRadius,
                                  double          topRadius) throw();

        // inherited from BRLCAD::Object
        virtual const Object& operator=(const Object& original) throw(bad_alloc);
        virtual Object*       Clone(void) const throw(bad_alloc, std::bad_alloc);
        static const char*    ClassName(void) throw();
        virtual const char*   Type(void) const throw();
        virtual bool          IsValid(void) const throw(); ///< checks for positive radii, one radius > 0 and if the height is 0 the radii have to be equal

    protected:
        Particle(resource*       resp,
                 directory*      pDir,
                 rt_db_internal* ip,
                 db_i*           dbip) throw();

        friend class ConstDatabase;

    private:
        rt_part_internal* m_internalp;

        void                    SetType(void) throw();

        const rt_part_internal* Internal(void) const throw();
        rt_part_internal*       Internal(void) throw();

        friend class Database;
    };
}


#endif // BRLCAD_PARTICLE_INCLUDED

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
