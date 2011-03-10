/*                      O B J E C T . H
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
/** @file Object.h
 *
 *  BRL-CAD core C++ interface:
 *      database object declaration
 *
 *  Origin -
 *      TNO (Netherlands)
 *      IABG mbH (Germany)
 */

#ifndef BRLCAD_OBJECT_INCLUDED
#define BRLCAD_OBJECT_INCLUDED

#include <brlcad/common.h>


struct resource;
struct directory;
struct rt_db_internal;
struct db_i;
struct bu_attribute_value_set;


namespace BRLCAD {
    class BRLCAD_COREINTERFACE_EXPORT Object {
    public:
        virtual ~Object(void) throw();

        // works if both Objects are of the same derived class
        virtual const Object& operator=(const Object& original) throw(bad_alloc) = 0;

        // a virtual constructor which creates the right derived class
        // and the corresponding destructor
        // which keeps the memory management in a healthy state
        virtual Object*       Clone(void) const throw(bad_alloc, std::bad_alloc) = 0;
        void                  Destroy(void) throw();

        // these two functions can be used to determine the type of the object
        static const char*    ClassName(void) throw();
        virtual const char*   Type(void) const throw()                           = 0;

        // Is this object functional?
        virtual bool          IsValid(void) const throw()                        = 0;

        // for all objects
        const char*           Name(void) const throw();
        void                  SetName(const char* name) throw(bad_alloc);


        class BRLCAD_COREINTERFACE_EXPORT AttributeIterator {
        public:
            AttributeIterator(const AttributeIterator& original) throw() : m_avs(original.m_avs),
                                                                           m_searchKey(original.m_searchKey),
                                                                           m_index(original.m_index) {}
            ~AttributeIterator(void) throw() {}

            const AttributeIterator& operator=(const AttributeIterator& original) throw() {
                m_avs       = original.m_avs;
                m_searchKey = original.m_searchKey;
                m_index     = original.m_index;
                return *this;
            }

            const AttributeIterator& operator++(void) throw();
            bool                     Good(void) const throw();
            const char*              Key(void) const throw();
            const char*              Value(void) const throw();

        private:
            const bu_attribute_value_set* m_avs;
            const char*                   m_searchKey;
            size_t                        m_index;

            AttributeIterator(const bu_attribute_value_set* avs,
                              const char*                   searchKey,
                              size_t                        index) throw();

            friend class Object;

            AttributeIterator(void); // not implemented
        };


        bool                  HasAttribute(const char* key) const throw();
        AttributeIterator     FirstAttribute(void) const throw();                    ///> returns an iterator pointing on the first attribute
        const char*           Attribute(const char* key) const throw();              ///> returns the value of the first attribute with this key
        AttributeIterator     MultiAttribute(const char* key) const throw();         ///> returns an iterator pointing on the first attribute with this key
        void                  SetAttribute(const char* key,
                                           const char* value) throw(bad_alloc);      ///> overwrites the attribute entry with this key or creates a new one if there is none
        void                  AddMultiAttribute(const char* key,
                                                const char* value) throw(bad_alloc); ///> creates an attribute entry with this values even if there exitsts already one with this key
        void                  RemoveAttribute(const char* key) throw();              ///> removes the first attribute with this key
        void                  ClearAttributes(void) throw();                         ///> removes all attributes


    protected:
        resource*       m_resp;
        directory*      m_pDir;
        rt_db_internal* m_ip;
        db_i*           m_dbip;

        Object(void) throw(bad_alloc);
        Object(resource*       resp,
               directory*      pDir,
               rt_db_internal* ip,
               db_i*           dbip) throw();
        Object(const Object& original) throw(bad_alloc);

        void Copy(const Object& original) throw(bad_alloc);
        bool Validate(void) const throw();

    private:
        // holds Objects's name if not connected to a database
        char*                   m_name;
        bu_attribute_value_set* m_avs;

        const bu_attribute_value_set* GetAvs(void) const throw();
        bu_attribute_value_set*       GetAvs(bool create) throw();

        friend class Database;
    };
}


#endif // BRLCAD_OBJECT_INCLUDED

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
