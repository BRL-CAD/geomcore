/*                      U N K N O W N . C P P
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
/** @file Unknown.cpp
 *
 *  BRL-CAD core C++ interface:
 *      yet unknown database object implementation
 *
 *  Origin -
 *      TNO (Netherlands)
 *      IABG mbH (Germany)
 */

#include <cassert>

#include <brlcad/Unknown.h>


using namespace BRLCAD;


Unknown::Unknown
(
    const Unknown& original
) throw(bad_alloc) : Object(original) {}


Unknown::~Unknown(void) throw() {}


const Unknown& Unknown::operator=
(
    const Unknown& original
) throw(bad_alloc) {
    Copy(original);

    return *this;
}


const Object& Unknown::operator=
(
    const Object& original
) throw(bad_alloc) {
    const Unknown* unknown = dynamic_cast<const Unknown*>(&original);
    assert(unknown != 0);

    if (unknown != 0)
        *this = *unknown;

    return *this;
}


Object* Unknown::Clone(void) const throw(bad_alloc, std::bad_alloc) {
    return new Unknown(*this);
}


const char* Unknown::ClassName(void) throw() {
    return "Unknown";
}


const char* Unknown::Type(void) const throw() {
    return ClassName();
}


bool Unknown::IsValid(void) const throw() {
    return Validate(); // the opportunistic solution
}


Unknown::Unknown
(
    resource*       resp,
    directory*      pDir,
    rt_db_internal* ip,
    db_i*           dbip
) throw() : Object(resp, pDir, ip, dbip) {}
