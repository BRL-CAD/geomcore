/*                  E V E N T M A N A G E R . H
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
/** @file EventManager.h
 *
 * Brief description
 *
 */

#ifndef __EVENTMANAGER_H__
#define __EVENTMANAGER_H__

#include "Event.h"
#include "commonDefines.h"
#include "Logger.h"
#include "JobManager.h"

#include <QtCore/QMutex>

class EventSubscriber;
class EventSubscription;

class EventManager
{
public:
	virtual ~EventManager();
	static EventManager* getInstance();

	/**
	 * Use submitEvent(...) when you want the call to processEvent(...) to be done from a different thread than the one calling submitEvent(...)
	 */
	void submitEvent(Event* e);
	void processEvent(Event* e);

	void subscribe(EventSubscriber* sub, quint32 eventType,
					EventPublisher* pub);
	void unsubscribe(EventSubscriber* sub, quint32 eventType,
			EventPublisher* pub);

private:
	static EventManager* pInstance;
	EventManager();

	QList<EventSubscriber*>* buildSubscriberList(Event* e);

	Logger* log;

	QMutex* subscriptionsLock;
	QList<EventSubscription*>* subscriptions;

	/* Disable copy cstr and =operator */
	EventManager(EventManager const&){};
	EventManager& operator=(EventManager const&){};
};

#endif /* __EVENTMANAGER_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
