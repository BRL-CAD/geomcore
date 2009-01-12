
/*                T H R O W A B L E . C X X
 * BRL-CAD
 *
 * Copyright (c) 1997-2009 United States Government as represented by
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

/** @file Throwable.cxx
 *
 *  Description -
 *      
 *
 *  Author - David Loman
 *
 */

#include "lang/Throwable.h"

Throwable::Throwable() throw ()
{
}

Throwable::Throwable(const String& message) throw ()
{
	_msg = message;
}

Throwable::Throwable(const Throwable& copy) throw ()
{
	_msg = copy._msg;
}

const String& Throwable::getMessage() const throw ()
{
	return _msg;
}

// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 2 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=2 tabstop=8
