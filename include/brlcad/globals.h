/*                      G L O B A L S . H
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
/** @file globals.h
 *
 *  BRL-CAD core C++ interface:
 *      declares global functions which influence the behavior
 *      of the BRL-CAD core
 *
 *  Origin -
 *      TNO (Netherlands)
 *      IABG mbH (Germany)
 */

#ifndef BRLCAD_GLOBALS_INCLUDED
#define BRLCAD_GLOBALS_INCLUDED

#include <brlcad/common.h>


namespace BRLCAD {
    /// @name get BRL-CAD version information
    //@{
    int BRLCAD_COREINTERFACE_EXPORT  MajorVersion(void) throw();
    int BRLCAD_COREINTERFACE_EXPORT  MinorVersion(void) throw();
    int BRLCAD_COREINTERFACE_EXPORT  PatchVersion(void) throw();
    //@}


    /// @name capturing the logging output
    //@{
    /// log handler (to be implemented by the caller)
    class LogHandler {
    public:
        virtual ~LogHandler(void) throw() {}

        virtual void operator()(const char* logString) throw() = 0;

    protected:
        LogHandler(void) throw() {}
        LogHandler(const LogHandler&) throw() {}
        const LogHandler& operator=(const LogHandler&) throw() {return *this;}
    };

    void BRLCAD_COREINTERFACE_EXPORT RegisterLogHandler(LogHandler& logHandler) throw(bad_alloc);
    void BRLCAD_COREINTERFACE_EXPORT DeRegisterLogHandler(LogHandler& logHandler) throw();
    //@}
}


#endif // BRLCAD_GLOBALS_INCLUDED

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
