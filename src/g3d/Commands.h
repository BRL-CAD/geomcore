/*                  C O M M A N D S . H
 * BRL-CAD
 *
 * Copyright (c) 2008 United States Government as represented by
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

/** @file Commands.h
 *
 * @author Manuel A. Fernandez Montecelo <mafm@users.sourceforge.net>
 *
 * @brief
 *	Implementation of commands.
 */

#ifndef __G3D_COMMANDS_H__
#define __G3D_COMMANDS_H__


#include "Command.h"

#include <string>

#include "Application.h"
#include "CameraManager.h"
#include "GedData.h"
#include "GeometryConversion.h"

#include <brlcad/ged.h>


/** @brief Quit the application
 *
 * @author Manuel A. Fernandez Montecelo <mafm@users.sourceforge.net>
 */
class CommandQuit : public Command
{
public:
  CommandQuit() :
    Command("quit", "Quit the application", "")
    { }

  virtual void execute(std::vector<std::string>& args, CommandOutput& out) {
    if (args.size() > 0) {
      out.appendLine("Command doesn't accept arguments, ignoring");
    }
    
    Application::instance().quit();
  }
};


/** @brief Set log message level.
 *
 * @author Manuel A. Fernandez Montecelo <mafm@users.sourceforge.net>
 */
class CommandSetLogLevel : public Command
{
public:
  CommandSetLogLevel() :
    Command("loglevel",
	    "Set the log message level",
	    "Argument is first letter of Debug, Info, Warning, Error, Fatal")
    {
      _argNames.push_back("level");
    }

  virtual void execute(std::vector<std::string>& args, CommandOutput& out) {
    if (args.size() != 1) {
      out.appendLine("This command needs exactly one argument");
      return;
    }

    Logger::Level l = static_cast<Logger::Level>(0); // invalid level
    char firstChar = args[0][0];
    if (firstChar == 'D') {
      l = Logger::DEBUG;
    } else if (firstChar == 'I') {
      l = Logger::INFO;
    } else if (firstChar == 'W') {
      l = Logger::WARNING;
    } else if (firstChar == 'E') {
      l = Logger::ERROR;
    } else if (firstChar == 'F') {
      l = Logger::FATAL;
    }

    Logger::instance().setLevelFilter(l);
  }
};


/** @brief Set Polygon mode.
 *
 * @author Manuel A. Fernandez Montecelo <mafm@users.sourceforge.net>
 */
class CommandSetPolygonMode : public Command
{
public:
  CommandSetPolygonMode() :
    Command("polygonmode",
	    "Set the polygon mode",
	    "Argument is [solid|wireframe|points] (initial chars are enough)")
    {
      _argNames.push_back("mode");

      // setting wireframe by default
      Application::instance().setPolygonMode(Ogre::PM_WIREFRAME);
    }

  virtual void execute(std::vector<std::string>& args, CommandOutput& out) {
    if (args.size() != 1) {
      out.appendLine("This command needs exactly one argument");
      return;
    }

    if (args[0][0] == 's') {
      Application::instance().setPolygonMode(Ogre::PM_SOLID);
    } else if (args[0][0] == 'w') {
      Application::instance().setPolygonMode(Ogre::PM_WIREFRAME);
    } else if (args[0][0] == 'p') {
      Application::instance().setPolygonMode(Ogre::PM_POINTS);
    } else {
      out.appendLine("Mode not recognized");
    }
  }
};


/** @brief Set Camera projection type.
 *
 * @author Manuel A. Fernandez Montecelo <mafm@users.sourceforge.net>
 */
class CommandSetCameraProjectionType : public Command
{
public:
  CommandSetCameraProjectionType() :
    Command("projection",
	    "Set camera projection type",
	    "Argument is [orthogonal|perspective] (initial chars are enough)")
    {
      _argNames.push_back("type");
    }

  virtual void execute(std::vector<std::string>& args, CommandOutput& out) {
    if (args.size() != 1) {
      out.appendLine("This command needs exactly one argument");
      return;
    }

    char firstChar = args[0][0];
    if (firstChar == 'o') {
      CameraManager::instance().setProjectionOrthogonal(true);
    } else {
      CameraManager::instance().setProjectionOrthogonal(false);
    }
  }
};


/** @brief Cycle Camera mode.
 *
 * @author Manuel A. Fernandez Montecelo <mafm@users.sourceforge.net>
 */
class CommandCycleCameraMode : public Command
{
public:
  CommandCycleCameraMode() :
    Command("cyclecam", "Cycle the camera mode", "")
    { }

  virtual void execute(std::vector<std::string>& args, CommandOutput& out) {
    if (args.size() > 0) {
      out.appendLine("Command doesn't accept arguments, ignoring");
    }

    CameraManager::instance().cycleCameraMode();
  }
};


/** @brief Create sample geometries.
 *
 * @author Manuel A. Fernandez Montecelo <mafm@users.sourceforge.net>
 */
class CommandCreateSampleGeometry : public Command
{
public:
  CommandCreateSampleGeometry() :
    Command("create",
	    "Create a sample geometry",
	    "Argument is [tetrahedron|cube] (initial chars are enough)"),
    _sampleTetrahedron(50)
    {
      _argNames.push_back("shape");

      // creating an initial shape, for convenience while in initial
      // phases of development...
      Application::instance().addGeometry("tetrahedron", "TetrahedronMesh");
    }

  virtual void execute(std::vector<std::string>& args, CommandOutput& out) {
    if (args.size() != 1) {
      out.appendLine("This command needs exactly one argument");
      return;
    }

    if (args[0][0] == 't') {
      Application::instance().addGeometry("tetrahedron", "TetrahedronMesh");
    } else if (args[0][0] == 'c') {
      out.appendLine("Shape not implemented yet");
    } else {
      out.appendLine("Shape not recognized");
    }
  }

private:
  /** Sample geometry */
  SampleTetrahedron _sampleTetrahedron;
};


/** @brief Get libged version.
 *
 * @author Manuel A. Fernandez Montecelo <mafm@users.sourceforge.net>
 *
 * \todo mafm: WIP
 */
class CommandGedVersion : public Command
{
public:
  CommandGedVersion() :
    Command("ged_version", "Get libged version", "") { }

  virtual void execute(std::vector<std::string>& args, CommandOutput& out) {
    if (args.size() > 0) {
      out.appendLine("Command doesn't accept arguments, ignoring");
    }

    ged* g = GedData::instance().getGED();
    ged_version(g, 0, 0);
    std::string s(g->ged_output_script);
    out.appendLine(s);
  }
};


/** @brief Get libged summary.
 *
 * @author Manuel A. Fernandez Montecelo <mafm@users.sourceforge.net>
 *
 * \todo mafm: WIP
 */
class CommandGedSummary : public Command
{
public:
  CommandGedSummary() :
    Command("ged_summary",
	    "Get libged summary",
	    "Argument is [primitives|regions|groups] (initial chars are enough)")
    {
      _argNames.push_back("type");
    }

  virtual void execute(std::vector<std::string>& args, CommandOutput& out) {
    ged* g = GedData::instance().getGED();

    if (args.size() > 1) {
      out.appendLine("This command needs exactly zero or one argument");
      return;
    } else if (args.size() == 1) {
      std::string type;
      if (args[0][0] == 'p') {
	type = "p";
      } else if (args[0][0] == 'r') {
	type = "r";
      } else if (args[0][0] == 'g') {
	type = "g";
      } else {
	out.appendLine("Summary type not recognized");
	return;
      }

      const char* arg[] = { type.c_str() };
      ged_summary(g, 1, arg);
      std::string s(g->ged_output_script);
      out.appendLine(s);
    } else {
      ged_summary(g, 0, 0);
      std::string s(g->ged_output_script);
      out.appendLine(s);
    }
  }
};


/** @brief Get/Set libged DB title.
 *
 * @author Manuel A. Fernandez Montecelo <mafm@users.sourceforge.net>
 *
 * \todo mafm: WIP
 */
class CommandGedTitle : public Command
{
public:
  CommandGedTitle() :
    Command("ged_title",
	    "Get/Set libged database title",
	    "Argument is title (empty to get)")
    {
      _argNames.push_back("title");
    }

  virtual void execute(std::vector<std::string>& args, CommandOutput& out) {
    if (args.size() > 1) {
      out.appendLine("This command needs exactly zero or one argument");
      return;
    }

    ged* g = GedData::instance().getGED();
    ged_title(g, 0, 0);
    std::string s(g->ged_output_script);
    out.appendLine(s);
  }
};

#endif


// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 2 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=2 tabstop=8

