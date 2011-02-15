/*                E V E N T M A N A G E R . C X X
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
/** @file EventManager.cxx
 *
 * Brief description
 *
 */

#include "EventManager.h"
#include "EventSubscription.h"
#include "DeliverEventJob.h"
#include "SubmitEventJob.h"

#include <GSThread.h>

EventManager* EventManager::pInstance = NULL;

EventManager::EventManager()
{
    this->subscriptions = new std::list<EventSubscription*> ();
    this->subscriptionsLock = new GSMutex();
    this->log = Logger::getInstance();
}

EventManager::~EventManager()
{
    this->subscriptionsLock->lock();
    for (std::list<EventSubscription*>::iterator it=subscriptions->begin(); it!=subscriptions->end(); it++)
        delete *it;
    delete subscriptions;

    this->subscriptionsLock->unlock();
    delete this->subscriptionsLock;
}

EventManager* EventManager::getInstance()
{
    if (!EventManager::pInstance) {
	pInstance = new EventManager();
    }
    return EventManager::pInstance;
}

void EventManager::submitEvent(Event* e)
{
    SubmitEventJob* j = new SubmitEventJob(e);
    JobManager::getInstance()->submitJob(j);
}

void EventManager::processEvent(Event* e)
{
    //First build a SubscriberList
    std::list<EventSubscriber*>* subList = this->buildSubscriberList(e);

    //Now notify subscribers of the event
    for (std::list<EventSubscriber*>::iterator it=subList->begin(); it != subList->end(); it++) {
	DeliverEventJob* edj = new DeliverEventJob(*it, e);
	edj->submit();
    }
}

std::list<EventSubscriber*>* EventManager::buildSubscriberList(Event* e)
{
    GSMutexLocker locker(this->subscriptionsLock);

    uint32_t eType = e->getEventType();
    EventPublisher* ePub = e->getPublisher();

    std::list<EventSubscriber*>* subscriberList = new std::list<EventSubscriber*> ();

    for (std::list<EventSubscription*>::iterator it=subscriptions->begin(); it!=subscriptions->end(); it++) {
	EventSubscription* subscription = *it;

	uint32_t ssType = subscription->getEventType();
	EventPublisher* ssPub = subscription->getPublisher();

	bool isSubscribedType = ((ssType == eType) || (ssType == ALL_EVENT_TYPES));
	bool isSubscribedPublisher = ((ssPub == ePub) || (ssPub
		== ALL_EVENT_PUBLISHERS));

	if (isSubscribedType && isSubscribedPublisher) {
	    subscriberList->push_back(subscription->getEventSubscriber());
	}
    }

    return subscriberList;
}

void EventManager::subscribe(EventSubscriber* sub, uint32_t eventType,
	EventPublisher* pub)
{
    GSMutexLocker locker(this->subscriptionsLock);

    EventSubscription* es = new EventSubscription(sub, eventType, pub);

    for (std::list<EventSubscription*>::iterator it=subscriptions->begin(); it!=subscriptions->end(); it++) {
 	EventSubscription* subscription = *it;

 	if (*subscription == *es) {
 	   log->logINFO("EventManager", "Duplicate Subscription");

 	   delete es;
 	   return;
	}
    }

    this->subscriptions->push_back(es);
}

void EventManager::unsubscribe(EventSubscriber* sub, uint32_t eventType,
	EventPublisher* pub)
{
    GSMutexLocker locker(this->subscriptionsLock);
    subscriptions->remove(new EventSubscription(sub, eventType, pub));
}

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
