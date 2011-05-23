/*                 C O M M O N D E F I N E S . H
 * BRLCAD
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
/** @file commonDefines.h
 *
 * A single place to put various defines.  No #include-ing of any files in this file!
 *
 */

#ifndef __COMMONDEFINES_H__
#define __COMMONDEFINES_H__

/**
 * Maximum length of a filesystem path.  Typically defined in a system
 * file but if it isn't set, we create it.
 */
#ifndef MAXPATHLEN
#  ifdef PATH_MAX
#    define MAXPATHLEN PATH_MAX
#  else
#    ifdef _MAX_PATH
#      define MAXPATHLEN _MAX_PATH
#    else
#      define MAXPATHLEN 1024
#    endif
#  endif
#endif

#define ALL_EVENT_PUBLISHERS	0

#endif /* __COMMONDEFINES_H__ */

/*
 * Local Variables: ***
 * mode: C
 * tab-width: 8
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t
 * End: ***
 * ex: shiftwidth=4 tabstop=8
*/
