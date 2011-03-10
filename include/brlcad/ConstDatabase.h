/*                      C O N S T D A T A B A S E . H
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
/** @file ConstDatabase.h
 *
 *  BRL-CAD core C++ interface:
 *      declares a handle on a read-only database
 *
 *  Origin -
 *      TNO (Netherlands)
 *      IABG mbH (Germany)
 */

#ifndef BRLCAD_CONSTDATABASE_INCLUDED
#define BRLCAD_CONSTDATABASE_INCLUDED

#include <brlcad/common.h>
#include <brlcad/Object.h>


struct rt_i;
struct resource;
struct directory;


namespace BRLCAD {
    class BRLCAD_COREINTERFACE_EXPORT ConstDatabase {
    public:
        ConstDatabase(void) throw(bad_alloc);
        virtual ~ConstDatabase(void) throw();

        /// associates the handle with a BRL-CAD database file (*.g)
        /** If the handle was already associated with a database file this association will be discarded.
            The file will be opened for reading only. */
        virtual bool      Load(const char* fileName) throw();

        /// @name Parsing the database
        //@{
        /// title string read from the database
        const char*       Title(void) const throw();


        class BRLCAD_COREINTERFACE_EXPORT TopObjectIterator {
        public:
            TopObjectIterator(const TopObjectIterator& original) throw() : m_hashTablePosition(original.m_hashTablePosition),
                                                                           m_pDir(original.m_pDir),
                                                                           m_rtip(original.m_rtip) {}

            ~TopObjectIterator(void) throw() {}

            const TopObjectIterator& operator=(const TopObjectIterator& original) throw() {
                m_hashTablePosition = original.m_hashTablePosition;
                m_pDir              = original.m_pDir;
                m_rtip              = original.m_rtip;

                return *this;
            }

            const TopObjectIterator& operator++(void) throw();
            bool                     Good(void) const throw();
            const char*              Name(void) const throw();

        private:
            size_t           m_hashTablePosition;
            const directory* m_pDir;
            const rt_i*      m_rtip;

            TopObjectIterator(size_t           hashTablePosition,
                              const directory* pDir,
                              const rt_i*      rtip) throw();

            friend class ConstDatabase;

            TopObjectIterator(void);
        };


        /// returns the first of the top level objects via an iterator object
        /** To get a list of all top level objects you have to use the iterator returned by this function. */
        TopObjectIterator FirstTopObject(void) const throw();
        //@}

        /// @name Accessing objects
        //@{
        class ObjectCallback {
        public:
            virtual ~ObjectCallback(void) throw() {}

            /// the user has to implement this object method to evaluate the object
            virtual void operator()(const Object& object) = 0;

        protected:
            ObjectCallback(void) throw() {}
            ObjectCallback(const ObjectCallback&) throw() {}
            const ObjectCallback& operator=(const ObjectCallback&) throw() {return *this;}
        };

        /// selects a single object and hand it over to an ObjectCallback (read only)
        void              Get(const char*     objectName,
                              ObjectCallback& callback) const;

        /// overloaded member function, provided for convenience: selects a single object and and returns a copy of it
        /** Do not forget to BRLCAD::Object::Destroy() the copy when you are finished with it! */
        Object*           Get(const char* objectName) const;
        //@}

        /// @name Active set functions
        //@{
        /// add the database object \a objectName to the active set
        /** The function accepts a space separated list of object names too,
            but it is not sure that this behaviour will be kept in future versions. */
        void              Select(const char* objectName) throw();
        /// clear the active set
        void              UnSelectAll(void) throw();

        bool              SelectionIsEmpty(void) const throw();
        Vector3D          BoundingBoxMinima(void) const throw();
        Vector3D          BoundingBoxMaxima(void) const throw();

        /// ray trace
        class Hit {
        public:
            virtual ~Hit(void) throw() {}

            virtual const char* Name(void) const throw()                = 0;
            virtual double      DistanceIn(void) const throw()          = 0;
            virtual double      DistanceOut(void) const throw()         = 0;
            virtual Vector3D    PointIn(void) const throw()             = 0;
            virtual Vector3D    PointOut(void) const throw()            = 0;
            virtual Vector3D    SurfaceNormalIn(void) const throw()     = 0;
            virtual Vector3D    SurfaceNormalOut(void) const throw()    = 0;
            virtual Curvature3D SurfaceCurvatureIn(void) const throw()  = 0;
            virtual Curvature3D SurfaceCurvatureOut(void) const throw() = 0;
            virtual Mapping2D   Surface2DMappingIn(void) const throw()  = 0;
            virtual Mapping2D   Surface2DMappingOut(void) const throw() = 0;

            /// some extracted data (originally from the region)
            virtual bool        HasColor(void) const throw()            = 0;
            virtual double      Red(void) const throw()                 = 0;
            virtual double      Green(void) const throw()               = 0;
            virtual double      Blue(void) const throw()                = 0;

        protected:
            Hit(void) throw() {}
            Hit(const Hit&) throw() {}
            const Hit& operator=(const Hit&) throw() {return *this;}
        };


        class HitCallback {
        public:
            virtual ~HitCallback(void) {}

            /** return true: go on; false: stop
                The return value gives the calling function the possibility to optimize.
                However be aware the return value may be ignored. */
            /** Do not throw en exception here.
                This method will be called from deep inside the brlcad libraries.
                The status of the program after an exception will be undetermined. */
            virtual bool operator()(const Hit& hit) throw() = 0;

        protected:
            HitCallback(void) throw() {}
            HitCallback(const HitCallback&) throw() {}
            const HitCallback& operator=(const HitCallback&) throw() {return *this;}
        };


        void              ShootRay(const Ray3D& ray,
                                   HitCallback& callback) const;
        //@}

    protected:
        rt_i*     m_rtip;
        resource* m_resp;

    private:
        ConstDatabase(const ConstDatabase&);                  // not implemented
        const ConstDatabase& operator=(const ConstDatabase&); // not implemented
    };
}


#endif // BRLCAD_CONSTDATABASE_INCLUDED

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
