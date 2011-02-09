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

#include <QtCore/QMutexLocker>

EventManager* EventManager::pInstance = NULL;

EventManager::EventManager()
{
    this->subscriptions = new QList<EventSubscription*> ();
    this->subscriptionsLock = new QMutex();
    this->log = Logger::getInstance();
}

EventManager::~EventManager()
{
    this->subscriptionsLock->lock();
    for (uint32_t i = 0; i < subscriptions->size(); ++i) {
	EventSubscription* es = subscriptions->at(i);
	delete es;
    }
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
    QList<EventSubscriber*>* subList = this->buildSubscriberList(e);

    //Now notify subscribers of the event
    for (int i = 0; i < subList->size(); ++i) {
	EventSubscriber* sub = subList->at(i);

	DeliverEventJob* edj = new DeliverEventJob(sub, e);
	edj->submit();
    }
}

QList<EventSubscriber*>* EventManager::buildSubscriberList(Event* e)
{
    QMutexLocker locker(this->subscriptionsLock);

    uint32_t eType = e->getEventType();
    EventPublisher* ePub = e->getPublisher();

    QList<EventSubscriber*>* subscriberList = new QList<EventSubscriber*> ();

    for (uint32_t i = 0; i < subscriptions->size(); ++i) {
	EventSubscription* subscription = subscriptions->at(i);

	uint32_t ssType = subscription->getEventType();
	EventPublisher* ssPub = subscription->getPublisher();

	bool isSubscribedType = ((ssType == eType) || (ssType == ALL_EVENT_TYPES));
	bool isSubscribedPublisher = ((ssPub == ePub) || (ssPub
		== ALL_EVENT_PUBLISHERS));

	if (isSubscribedType && isSubscribedPublisher) {
	    subscriberList->append(subscription->getEventSubscriber());
	}
    }

    return subscriberList;
}

void EventManager::subscribe(EventSubscriber* sub, uint32_t eventType,
	EventPublisher* pub)
{
    QMutexLocker locker(this->subscriptionsLock);

    EventSubscription* es = new EventSubscription(sub, eventType, pub);

    for (uint32_t i = 0; i < subscriptions->size(); ++i) {
 	EventSubscription* subscription = subscriptions->at(i);

 	if (*subscription == *es) {
 	   log->logINFO("EventManager", "Duplicate Subscription");

 	   delete es;
 	   return;
	}
    }

    this->subscriptions->append(es);
}

void EventManager::unsubscribe(EventSubscriber* sub, uint32_t eventType,
	EventPublisher* pub)
{
    QMutexLocker locker(this->subscriptionsLock);

    EventSubscription* es = new EventSubscription(sub, eventType, pub);

    for (uint32_t i = 0; i < subscriptions->size(); ++i) {
 	EventSubscription* subscription = subscriptions->at(i);

 	if (*subscription == *es) {
 	    this->subscriptions->removeAt(i);
	}
    }
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
