/*              B A S I C E V E N T T E S T . C X X
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
/** @file BasicEventTest.cxx
 *
 * Brief description
 *
 */

#include "libevent.h"
#include "libutility.h"
#include "libjob.h"
#include "commonDefines.h"

#include <QtCore/Qt>

class TestSubscriber : public EventSubscriber
{
public:
    TestSubscriber(std::string name) :
	_name(name)
    {
    }
    ;

    void handleEvent(Event* e)
    {
	char buf[BUFSIZ];

	snprintf(buf, BUFSIZ, "Recv Event of type: %d with msg: %s", e->getEventType(), e->getMessage().c_str());
	Logger::getInstance()->logINFO(_name, std::string(buf));
    }
    ;

private:
    std::string _name;
};

class TestPublisher : public EventPublisher
{
public:
    TestPublisher(std::string name) : _name(name){};

    void generateEvent(uint32_t type, std::string message = "")
    {
	char buf[BUFSIZ];
	snprintf(buf, BUFSIZ, "Generating Event of type: %d with msg: %s", type, message.c_str());
	Logger::getInstance()->logINFO(_name, buf);
	EventPublisher::generateEvent(type, message);
    }

    void generateEvent01()
    {
	this->generateEvent(99, _name + "-99");
    }
    void generateEvent02()
    {
	this->generateEvent(100, _name + "-100");
    }
    void generateEvent03()
    {
	this->generateEvent(101, _name + "-101");
    }
    void generateEventSequence()
    {
	this->generateEvent01();
	this->generateEvent02();
	this->generateEvent03();
    }

private:
    std::string _name;
};

int main(int argc, char* argv[])
{
    JobManager* jMan = JobManager::getInstance();
    jMan->startup();
    GSThread::usleep(100);//Give the JobWorkers a chance to init.

    EventManager* eMan = EventManager::getInstance();
    Logger* log = Logger::getInstance();
    log->disableVerbose();

    log->logBANNER("BasicEventTest", "Basic Event Test");

    TestSubscriber tSub01("Sub01");
    TestSubscriber tSub02("Sub02");
    TestSubscriber tSub03("Sub03");
    TestPublisher tPubA("Pub-A");
    TestPublisher tPubB("Pub-B");
    TestPublisher tPubC("Pub-C");

    log->logBANNER("BasicEventTest", "Testing Duplicate Subscriptions");
    eMan->subscribe(&tSub01, 100, ALL_EVENT_PUBLISHERS);
    eMan->subscribe(&tSub01, 100, ALL_EVENT_PUBLISHERS);
    tPubA.generateEvent02();

    GSThread::sleep(1);
    log->logBANNER("BasicEventTest", "Testing Subscription Unsubscribe");
    eMan->unsubscribe(&tSub01, 100, ALL_EVENT_PUBLISHERS);
    tPubA.generateEvent02();

    GSThread::sleep(1);
    log->logBANNER("BasicEventTest", "All Publishers, Subscriber 01(Event #100)");
    eMan->subscribe(&tSub01, 100, ALL_EVENT_PUBLISHERS);
    tPubA.generateEventSequence();

    GSThread::sleep(1);
    log->logBANNER("BasicEventTest", "All Publishers, Subscriber 02(Event #100)");
    eMan->subscribe(&tSub02, 100, ALL_EVENT_PUBLISHERS);
    tPubA.generateEventSequence();

    GSThread::sleep(1);
    log->logBANNER("BasicEventTest", "Publisher A, Subscriber 02(Event# 101)");
    eMan->subscribe(&tSub02, 101, &tPubA);
    tPubA.generateEventSequence();
    tPubB.generateEventSequence();

    GSThread::sleep(3);
    jMan->shutdown();
    return 0;

    GSThread::sleep(1);
    log->logBANNER("BasicEventTest", "Publisher B, Subscriber 03(All Events)");
    eMan->subscribe(&tSub03, ALL_EVENT_TYPES, &tPubB);

    tPubA.generateEventSequence();
    tPubB.generateEventSequence();

    GSThread::sleep(1);
    log->logBANNER("BasicEventTest", "DONE");

    jMan->shutdown();
    return 0;
}

// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 2 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=2 tabstop=8
