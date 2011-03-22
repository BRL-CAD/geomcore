/*         G E O M E T R Y S E R V I C E T E S T . C X X
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
/** @file GeometryServiceTest.cxx
 *
 * This is a high-level integration test harness for exploring the
 * capabilities of the Geometry Service base functionality.
 *
 */

#include <stdlib.h>
#include <string>
#include <iostream>
#include <vector>
#include <iomanip>

#include "GeometryService.h"
#include "GSClient.h"

#include "GSUuid.h"

static const uint16_t DEFAULT_PORT = 5309;

/**
 * wrapper server class for staring up an object that instantiates and
 * represents a geometry service instance.  this should simply stub
 * into GS API calls or exec a GS process.
 */
class GeometryServer
{
    private:
	uint16_t _port;
	std::string* _addy;
	GeometryService* gs;

    public:
	GeometryServer(std::string* addy = new std::string("127.0.0.1"), const uint16_t port = DEFAULT_PORT)
	{
	    if (port > 0)
		start(addy, port);
	}

	~GeometryServer()
	{
	    if (this->gs != NULL)
		delete this->gs;
	}

	bool stillRunning() const
	{
	    sleep(1);
	    std::cout << "\tGS:" << this->gs << std::endl;

	    if (this->gs == NULL)
		return false;

	    bool isRun = this->gs->isRunning();

	    sleep(1);
	    std::cout << "\tisRun "<< isRun << std::endl;


	    return isRun;
	}

	void start(std::string* addy, const uint16_t port = DEFAULT_PORT)
	{
	    std::string localName("Athena");
	    /* FIXME Are these two internals really needed? */
	    this->_port = port;
	    this->_addy = addy;

	    this->gs = new GeometryService(localName, *addy, port);

	    this->gs->start();
	}

	void start()
	{
	    this->gs->start();
	}


	void stop() const
	{
	    if (this->gs == NULL) {
		return;
	    }

	    this->gs->shutdown();
	    usleep(100000);
	    delete this->gs;
	}
};

/**
 * wrapper client class for staring up an object that can connect to
 * and communicate with a geometry server.  this should simply stub
 * into GS API calls or direct raw network protocol.
 */
class GeometryClient
{
    private:
	GSUuid* testClientID;
	GSClient* gsClient;
	Portal* portal;

	bool exists(std::string object) const
	{
	    if (object.size() == 0)
		return false;

	    // ask server if object exists
	    return false;
	}
    public:
	GeometryClient()
	{
	    this->testClientID = GSUuid::createUuid();
	    this->gsClient = new GSClient(*this->testClientID->toString());
	    this->portal == NULL;
	    std::cerr << "Portal:" << this->portal << std::endl;
	}

	~GeometryClient()
	{
	    if (this->gsClient != NULL)
		delete gsClient;
	}

	void connect(const char *address = "127.0.0.1", int port = DEFAULT_PORT)
	{
	    if (port < 0) {
		std::cerr << "Unexpected test harness state: port<0" << std::endl;
		exit(1);
	    }
	    if (this->gsClient == NULL) {
		std::cerr << "Unexpected test harness state: Null GSClient" << std::endl;
		exit(1);
	    }

	    PortalManager* clientPortMan = this->gsClient->getPortMan();

	    if (clientPortMan == NULL) {
		std::cerr << "Unexpected test harness state: Null PortalManager" << std::endl;
		exit(1);
	    }

	    Portal* p = clientPortMan->connectToHost("localhost", 12345);

	    if (p == NULL) {
		std::cerr << "Unexpected test harness state: Failure on ConnectToHost.  Portal:" << this->portal << std::endl;
		return;
	    }

	    this->portal = p;
	}

	void disconnect() const
	{
	}

	bool connected() const
	{
	    return (this->portal != NULL);
	}

	//TODO implement 'getDirectory'
	std::vector<std::string> getDirectory() const
	{
	    std::vector<std::string> v;
	    return v;
	}

	//TODO implement 'getObject'
	std::string getObject(const std::string object, const std::string version =
		std::string("")) const
	{
	    std::string encoding = "";

	    // get encoding from server for a given object version ("" is current)

	    return encoding;
	}

	//TODO implement 'getVersion'
	std::string getVersion(const std::string object) const
	{
	    if (exists(object))
		return std::string("");

	    // get the version from server, return as string

	    return std::string("");
	}

	//TODO implement 'putObject'
	bool putObject(const std::string encoding) const
	{
	    if (encoding.size() == 0)
		return false;

	    // write object encoding to server

	    return false;
	}

	//TODO implement 'addObject'
	bool addObject(const std::string object) const
	{
	    if (exists(object))
		return false;

	    // encode object for put
	    std::string encoding = object;

	    return putObject(encoding);
	}

	//TODO implement 'updateObject'
	bool updateObject(const std::string object) const
	{
	    if (!exists(object))
		return false;

	    std::string encoding = getObject(object);
	    if (encoding.size() == 0)
		return false;

	    // modify the encoding

	    return putObject(object);
	}

	//TODO implement 'deleteObject'
	bool deleteObject(const std::string object) const
	{
	    if (!exists(object))
		return false;

	    // remove it, return true

	    return false;
	}

	//TODO implement 'getAttribute'
	bool getAttribute(const std::string object, const std::string name,
		std::string &value) const
	{
	    if (!exists(object))
		return false;

	    // see if object has a name attribute, set value, return true

	    return false;
	}

	//TODO implement 'setAttribute'
	bool setAttribute(const std::string object, const std::string name,
		std::string value) const
	{
	    if (!exists(object))
		return false;

	    // set name=value on object, return true

	    return false;
	}

	typedef enum _representation_t
	{
	    WIREFRAME, TRIANGLES, POINTS, NURBS, CAD
	} representation_t;

	//TODO implement 'getRepresentation'
	std::string getRepresentation(const std::string object,
		representation_t representation, bool blocking = true) const
	{
	    if (!blocking) {
		// get unevaluated representation handle, return it

		return std::string("");
	    }

	    switch (representation) {
		case WIREFRAME:
		    {
			// get wireframe representation from server, return true

			break;
		    }
		case TRIANGLES:
		    {
			// get triangle mesh representation from server, return true

			break;
		    }
		case POINTS:
		    {
			// get point-cloud representation from server, return true

			break;
		    }
		case NURBS:
		    {
			// get NURBS representation from server, return true

			break;
		    }
		case CAD:
		    {
			// get .g representation from server, return true

			break;
		    }
	    }
	    return std::string("");
	}

	//TODO implement 'evaluateRepresentation'
	void evaluateRepresentation(const std::string representation) const
	{
	    // tell server to begin evaluating the non-blocking representation handle
	}

	//TODO implement 'subscribeEvent'
	bool subscribeEvent() const
	{
	    // subscribe to all events on server, return true

	    return false;
	}

	//TODO implement 'unsubscribeEvent'
	bool unsubscribeEvent() const
	{
	    // unsubscribe to all events on server, return true

	    return false;
	}

	//TODO implement 'eventsReceived'
	int eventsReceived() const
	{
	    // retrieved any events buffered on server, return count

	    return 0;
	}

	//TODO implement 'shootRay'
	bool shootRay(const double point[3], const double direction[3],
		const std::string object) const
	{
	    // tell server to fire ray at object, return true

	    return false;
	}
};

/*******************************************/
/* basic scaffolding for reporting results */
/*******************************************/

/* used for keeping track of failures */
static int failures = 0;

/* assert that a specific test condition is true */
#define GAS(cond, m) bu_log("      [ \x1B[3%s\x1B[m ] %s\n", cond?"2mSUCCESS":(failures++,"1mFAILURE"), m);

/* helper function to report each section while keeping count */
static void Report(const std::string msg, bool showTitle = false)
{
    static int steps = 0;
    if (showTitle)
	std::cout << std::setw(2) << std::setfill(' ') << steps++ << ": ";
    std::cout << msg << std::endl;
}

/* basic end-of-section sanity testing with summary reporting */
#define RESULT() { \
    GAS(gs != NULL && gs->stillRunning(), "Server is still running"); \
    if (failures == 0) { \
	std::string msg = "\x1B[32mOK :)\x1B[m"; \
	Report(msg, true); \
	std::cout << std::endl; \
    } else { \
	std::string msg = "\x1B[21mNO !!\x1B[m"; \
	Report(msg, true);   \
	std::cout << std::endl; \
    } \
    failures = 0; }

/* basic beginning of section marker */
#define REQUIREMENT(m) \
    std::cout << std::setw(74) << std::setfill('=') << std::string(" ") + m + std::string(" ") << std::setw(4) << "=" << std::endl;

/************************************************************/
/* convenience wrappers to simplify the repetitive patterns */
/************************************************************/

static void Connect(GeometryClient *gc, GeometryClient *gc2 = NULL,
	GeometryClient *gc3 = NULL)
{
    if (gc && !gc2 && !gc3) {
	gc->connect();
	GAS(gc->connected(), "Connecting client");
    } else if (gc && gc2 && !gc3) {
	gc->connect();
	gc2->connect();
	GAS(gc->connected() && gc2->connected(), "Connecting two clients");
    } else if (gc && gc2 && gc3) {
	gc->connect();
	gc2->connect();
	gc3->connect();
	GAS(gc->connected() && gc2->connected() && gc3->connected(), "Connecting three clients");
    } else {
	std::cerr << "Unexpected test harness state" << std::endl;
	exit(1);
    }
}

static void Disconnect(GeometryClient *gc, GeometryClient *gc2 = NULL,
	GeometryClient *gc3 = NULL)
{
    if (gc && !gc2 && !gc3) {
	if (gc->connected()) {
	    gc->disconnect();
	    GAS(!gc->connected(), "Disconnecting client");
	} else
	    GAS(gc->connected(), "Disconnecting client");
    } else if (gc && gc2 && !gc3) {
	if (gc->connected() || gc2->connected()) {
	    gc->disconnect();
	    gc2->disconnect();
	    GAS(!gc->connected() || !gc2->connected(), "Disconnecting two clients");
	} else {
	    GAS(gc->connected(), "Disconnecting client one");
	    GAS(gc2->connected(), "Disconnecting client two");
	}
    } else if (gc && gc2 && gc3) {
	if (gc->connected() || gc2->connected() || gc3->connected()) {
	    gc->disconnect();
	    gc2->disconnect();
	    gc3->disconnect();
	    GAS(!gc->connected() || !gc2->connected() || !gc3->connected(), "Disconnecting three clients");
	} else {
	    GAS(gc->connected(), "Disconnecting client one");
	    GAS(gc2->connected(), "Disconnecting client two");
	    GAS(gc3->connected(), "Disconnecting client three");
	}
    } else {
	std::cerr << "Unexpected test harness state" << std::endl;
	exit(1);
    }
}

/************/
/* THE BOSS */
/************/

int main(int ac, char *av[])
{
    Logger::getInstance();
    JobManager::getInstance();

    //disable the logger for now.
    Logger::getInstance()->disableLogToConsole();

    std::vector<std::string> gcdir, gc2dir, gc3dir;
    std::string rep, rep2;

    /* don't need no params just yet */
    if (ac > 1) {
	for (int i = 1; i < ac; i++)
	    std::cerr << "Unexpected test harness parameter: [" << av[i] << "]" << std::endl;
	exit(1);
    }

    REQUIREMENT("Initialization of server and client");

    GeometryServer *gs = new GeometryServer();
    GAS(gs != NULL, "Starting up a geometry server");

    GeometryClient *gc = new GeometryClient();
    GAS(gc != NULL, "Starting up a geometry client");

    RESULT();

    /*****************************************/
    /* MAKE SURE THE SERVER CAN BE RESTARTED */
    /*****************************************/

    REQUIREMENT("Server restarts");

    gs->stop();
    sleep(1);
    std::cout << "\t1\n";

    GAS(!gs->stillRunning(), "Server shutting down");
    sleep(1);
    std::cout << "\t2\n";

#if 0	/* it makes no sense to connect without a server... */
    gc->connect();
    sleep(1);
    std::cout << "\t3\n";

    GAS(!gc->connected(), "Client prevented from connecting");
    sleep(1);
    std::cout << "\t4\n";
#endif

    gs->start();
    sleep(1);
    std::cout << "\t5\n";


    RESULT();

    /* cleanup */
    usleep(1000 * 1000 * 3);
    gs->stop();

    //TODO BOOKMARK

    /* delete gc3; */
    /* delete gc2; */
    delete gc;
    delete gs;

    JobManager::getInstance()->shutdown(true);

    return 0;

    /**********************************************/
    /* MAKE SURE A CLIENT CAN CONNECT TO A SERVER */
    /**********************************************/

    REQUIREMENT("One client connecting (no action)");

    Connect(gc);
    Disconnect(gc);

    RESULT();

    /*************************************************/
    /* MAKE SURE TWO CLIENTS CAN CONNECT TO A SERVER */
    /*************************************************/

    REQUIREMENT("Two clients simultaneously connecting (no action)");

    GeometryClient *gc2 = new GeometryClient();
    GAS(gc2 != NULL, "Starting up a second geometry client");

    Connect(gc, gc2);
    Disconnect(gc, gc2);

    RESULT();

    /*********************************/
    /* MAKE SURE ONE CLIENT CAN READ */
    /*********************************/

    REQUIREMENT("Client reading from server");

    Connect(gc);
    gcdir = gc->getDirectory();
    GAS(gcdir.size() != 0, "Client getting a directory");
    Disconnect(gc);

    RESULT();

    /*******************************************/
    /* MAKE SURE ONE CLIENT CAN READ AND WRITE */
    /*******************************************/

    REQUIREMENT("Client reading and writing objects");

    Connect(gc);
    gcdir = gc->getDirectory();
    GAS(gcdir.size() > 0, "Client getting a directory");
    GAS(gc->addObject("object_1"), "Client adding object");
    Disconnect(gc);

    RESULT();

    /**********************************/
    /* MAKE SURE TWO CLIENTS CAN READ */
    /**********************************/

    REQUIREMENT("Two clients reading");

    Connect(gc, gc2);
    gcdir = gc->getDirectory();
    GAS(gcdir.size() != 0, "Client getting a directory");
    gc2dir = gc2->getDirectory();
    GAS(gc2dir.size() != 0, "Second client getting a directory");
    Disconnect(gc, gc2);

    RESULT();

    /****************************************************/
    /* MAKE SURE ONE CLIENT CAN WRITE, ANOTHER CAN READ */
    /****************************************************/

    REQUIREMENT("One client writing, one client reading");

    Connect(gc, gc2);
    gcdir = gc->getDirectory();
    GAS(gcdir.size() != 0, "Client getting a directory");
    GAS(gc->addObject("object_2"), "Client adding object2");
    gc2dir = gc2->getDirectory();
    GAS(gc2dir.size() != 0, "Second client getting a directory");
    GAS(gcdir.size() > 0 && gc2dir.size() > 0 && gcdir.size() != gc2dir.size(), "Comparing two directory sizes for not equal"); // should compare contents
    gcdir = gc->getDirectory();
    GAS(gcdir.size() > 0 && gc2dir.size() > 0 && gcdir.size() == gc2dir.size(), "Comparing two directory sizes for equal"); // should compare contents

    GAS(gc2->getObject("object_2").size() != 0, "Second client verifying object_2 exists");
    GAS(gc->addObject("object_3"), "Client adding object_3");
    GAS(!gc2->addObject("object_2"), "Second client prevented from adding object_2");
    gcdir = gc->getDirectory();
    GAS(gcdir.size() != 0, "Client getting a directory");
    gc2dir = gc2->getDirectory();
    GAS(gc2dir.size() != 0, "Second client getting a directory");
    GAS(gcdir.size() > 0 && gc2dir.size() > 0 && gcdir.size() == gc2dir.size(), "Comparing two directory sizes for equal"); // should compare contents
    Disconnect(gc, gc2);

    RESULT();

    /********************************************************/
    /* MAKE SURE ONE CLIENT CAN WRITE, ANOTHER TWO CAN READ */
    /********************************************************/

    REQUIREMENT("One client writing, two clients reading");

    GeometryClient *gc3 = new GeometryClient();
    GAS(gc3 != NULL, "Starting up a third geometry client");

    Connect(gc, gc2, gc3);
    gcdir = gc->getDirectory();
    GAS(gcdir.size() != 0, "Client getting a directory");
    gc2dir = gc2->getDirectory();
    GAS(gc2dir.size() != 0, "Second client getting a directory");
    GAS(gc->addObject("object_4"), "Client adding object_4");
    std::string obj4 = gc2->getObject("object_4");
    GAS(obj4.size() != 0, "Client one verifying object_4 created");
    gc3dir = gc3->getDirectory();
    GAS(gc3dir.size() != 0, "Third client getting a directory");
    std::string obj4_2 = gc2->getObject("object_4");
    GAS(obj4_2.size() != 0, "Client two reading object_4");
    GAS(gc2->updateObject("object_4"), "Client two attempting to modify object_4");
    std::string obj4_3 = gc2->getObject("object_4");
    GAS(obj4_3.size() != 0, "Client two re-reading object_4");
    GAS(obj4.size() > 0 && obj4_2.size() > 0 && obj4 != obj4_2, "Comparing second object_4 to original for not equal");
    GAS(obj4.size() > 0 && obj4_3.size() > 0 && obj4 != obj4_3, "Comparing third object_4 to original for not equal");
    GAS(obj4_2.size() > 0 && obj4_3.size() > 0 && obj4_2 == obj4_3, "Comparing second object_4 to third for equal");
    GAS(gc3->getObject("object_4").size() != 0, "Client three reading object_4");
    GAS(gc3->deleteObject("object_4"), "Client three deleting object_4");
    GAS(gc3->getObject("object_4").size() == 0, "Client three verifying object_4 deleted");
    GAS(gc2->getObject("object_4").size() == 0, "Client two verifying object_4 deleted");
    GAS(gc->getObject("object_4").size() == 0, "Client one verifying object_4 deleted");
    Disconnect(gc, gc2, gc3);

    RESULT();

    /*****************************/
    /* CAN READ/WRITE ATTRIBUTES */
    /*****************************/

    REQUIREMENT("Client reading and writing attributes");

    Connect(gc);
    gcdir = gc->getDirectory();
    GAS(gcdir.size() != 0, "Client getting a directory");
    if (gcdir.size() > 0) {
	std::string attr;
	gc->getAttribute(gcdir[0], "foo", attr);
	GAS(attr.size() == 0, "Client ensuring attribute does not already exist");
	GAS(gc->setAttribute(gcdir[0], "foo", "bar"), "Client setting attribute");
	attr.clear();
	gc->getAttribute(gcdir[0], "foo", attr);
	GAS(attr.size() == 0, "Client ensuring attribute now exists");
    } else
	GAS(false, "Unable to test attributes");
    Disconnect(gc);

    RESULT();

    /*****************************************/
    /* CAN GET .g REPRESENTATION OF GEOMETRY */
    /*****************************************/

    REQUIREMENT("Retrievable .g representation for client-side ray tracing");

    Connect(gc);
    GAS(gc->addObject("object_5"), "Client adding object5");
    rep = gc->getRepresentation("object_5", GeometryClient::CAD);
    GAS(rep.size() != 0, "Getting .g representation");
    GAS(gc->updateObject("object_5"), "Client attempting to modify object_5");
    rep2 = gc->getRepresentation("object_5", GeometryClient::CAD);
    GAS(rep2.size() != 0, "Getting new .g representation");
    GAS(rep.size() > 0 && rep2.size() > 0 && rep != rep2, "Ensuring representations are different");
    // write(inmem);
    // rt_dirbuild();
    // rt_prep();
    // rt_shootray();
    Disconnect(gc);

    RESULT();

    /***************************************/
    /* CAN READ A WIREFRAME REPRESENTATION */
    /***************************************/

    REQUIREMENT("Client reading wireframe representation");

    Connect(gc);
    gcdir = gc->getDirectory();
    GAS(gcdir.size() != 0, "Client getting a directory");
    rep = gc->getRepresentation("object_5", GeometryClient::WIREFRAME);
    GAS(rep.size() != 0, "Getting a wireframe representation");
    GAS(gc->updateObject("object_5"), "Client attempting to modify object_5");
    rep2 = gc->getRepresentation("object_5", GeometryClient::WIREFRAME);
    GAS(rep2.size() != 0, "Getting a wireframe representation");
    GAS(rep.size() > 0 && rep2.size() > 0 && rep != rep2, "Ensuring representations are different");
    Disconnect(gc);

    RESULT();

    /*******************************/
    /* CAN GET EVENT NOTIFICATIONS */
    /*******************************/

    REQUIREMENT("Client event notifications");

    Connect(gc, gc2);
    GAS(gc->subscribeEvent(), "Client subscribing to events");
    GAS(gc2->addObject("object_6"), "Client two adding object_6");
    GAS(gc2->updateObject("object_6"), "Client two modifing object_6");
    GAS(gc2->setAttribute("object_6", "foo", "bar"), "Client two setting foo attribute on object_6");
    GAS(gc->eventsReceived() >= 3, "Client receiving events");
    GAS(gc->unsubscribeEvent(), "Client unsubscribing to events");
    GAS(gc2->addObject("object_7"), "Client adding object_7");
    GAS(gc2->updateObject("object_6"), "Client two modifing object_6 (again)");
    GAS(gc2->setAttribute("object_6", "bar", "foo"), "Client two setting bar attribute on object_6");
    GAS(gc->eventsReceived() == 0, "Client not receiving events");
    Disconnect(gc, gc2);

    RESULT();

    /*********************************/
    /* CAN PERSIST GEOMETRY VERSIONS */
    /*********************************/

    REQUIREMENT("Multiple versions of geometry are persisted and retrievable");

    Connect(gc, gc2);
    GAS(gc->addObject("object_8"), "Client adding object8");
    std::string obj8_ver = gc->getVersion("object_8");
    std::string obj8 = gc2->getObject("object_8");
    GAS(obj8.size() != 0, "Second client verifying object_8 added");
    GAS(gc->updateObject("object_8"), "Client modifing object_8");
    GAS(gc2->updateObject("object_8"), "Client two modifing object_8");
    std::string obj8_2 = gc2->getObject("object_8");
    GAS(obj8_2.size() != 0, "Second client verifying object_8 updated");
    GAS(obj8.size() > 0 && obj8_2.size() > 0 && obj8 != obj8_2, "Verifying object_8 different");
    std::string obj8_2_ver = gc->getVersion("object_8");
    GAS(obj8_ver.size() > 0 && obj8_2_ver.size() > 0 && obj8_ver != obj8_2_ver, "Verifying versions are different");
    std::string obj8_3 = gc2->getObject("object_8", obj8_ver);
    GAS(obj8_3.size() != 0, "Second client getting original object_8");
    GAS(obj8.size() > 0 && obj8_3.size() > 0 && obj8 == obj8_3, "Verifying object_8 same as original");

    Disconnect(gc, gc2);

    RESULT();

    /******************************/
    /* CAN SHOOT RAYS AT GEOMETRY */
    /******************************/

    REQUIREMENT("Server-side ray tracing");

    Connect(gc);
    GAS(gc->addObject("object_9"), "Client adding object9");
    GAS(gc->subscribeEvent(), "Client subscribing to events");
    double pnt[3] = { 0.0, 0.0, -10000.0 };
    double dir[3] = { 0.0, 0.0, 1.0 };
    GAS(gc->shootRay(pnt, dir, "object_9"), "Server shot ray");
    GAS(gc->eventsReceived() > 0, "Client receiving events");
    Disconnect(gc);

    RESULT();

    /***************************************/
    /* CAN READ A POLYGONAL REPRESENTATION */
    /***************************************/

    REQUIREMENT("Client retrieving polygonal representation");

    Connect(gc);
    gcdir = gc->getDirectory();
    GAS(gcdir.size() != 0, "Client getting a directory");
    if (gcdir.size() > 0) {
	rep = gc->getRepresentation(gcdir[0], GeometryClient::TRIANGLES);
	GAS(rep.size() != 0, "Getting mesh representation of object");
	GAS(gc->updateObject(gcdir[0]), "Client modifing object");
	rep2 = gc->getRepresentation(gcdir[0], GeometryClient::TRIANGLES);
	GAS(rep2.size() != 0, "Getting updated mesh representation of object");
	GAS(rep.size() > 0 && rep2.size() > 0 && rep != rep2, "Ensuring representations are different");
    }
    else
	GAS(false, "Unable to test getting mesh representations");
    Disconnect(gc);

    RESULT();

    /*****************************************/
    /* CAN READ A POINT-CLOUD REPRESENTATION */
    /*****************************************/

    REQUIREMENT("Client retrieving point-cloud representation");

    Connect(gc);
    gcdir = gc->getDirectory();
    GAS(gcdir.size() != 0, "Client getting a directory");
    if (gcdir.size() > 0) {
	rep = gc->getRepresentation(gcdir[0], GeometryClient::POINTS);
	GAS(rep.size() != 0, "Getting point cloud representation of object");
	rep2 = gc->getRepresentation(gcdir[0], GeometryClient::POINTS, false);
	GAS(rep2.size() != 0, "Getting updated mesh representation of object");
	GAS(rep.size() > 0 && rep2.size() > 0 && rep != rep2, "Ensuring representations are different");
	GAS(gc->subscribeEvent(), "Client subscribing to events");
	gc->evaluateRepresentation(rep2);
	//sleep(1); // give server time to process
	GAS(gc->eventsReceived() > 0, "Client received representation events");
    }
    else
	GAS(false, "Unable to test getting point cloud representations");

    Disconnect(gc);

    RESULT();

    /* cleanup */

    delete gc3;
    delete gc2;
    delete gc;
    delete gs;

    JobManager::getInstance()->shutdown(true);

    return 0;
}

// Local Variables:
// tab-width: 8
// mode: C++
// c-basic-offset: 4
// indent-tabs-mode: t
// c-file-style: "stroustrup"
// End:
// ex: shiftwidth=4 tabstop=8
