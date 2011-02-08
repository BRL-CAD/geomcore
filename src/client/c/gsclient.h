/*                      G S C L I E N T . H
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
/** @addtogroup GS */
/** @{ */
/** @file gsclient.h
 *
 *
 */

#ifndef __GSCLIENT_H__
#define __GSCLIENT_H__

#ifdef __cplusplus
extern "C" {
#endif

void gs_init();
void gs_cleanup();
char *gs_version();

#ifdef __cplusplus
}
#endif

#endif
