/*             E V E N T S U B S C R I P T I O N . H
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
/** @file EventSubscription.h
 *
 * Brief description
 *
 */

#ifndef __EVENTSUBSCRIPTION_H__
#define __EVENTSUBSCRIPTION_H__

#include "EventTypes.h"
#include "EventSubscriber.h"
#include "EventPublisher.h"
#include "commonDefines.h"

class EventSubscription
{
public:
    EventSubscription(EventSubscriber* sub, uint32_t eventType = ALL_EVENT_TYPES,
	    EventPublisher* pub = ALL_EVENT_PUBLISHERS);
    virtual ~EventSubscription();

    EventPublisher* getPublisher();
    uint32_t getEventType();
    EventSubscriber* getEventSubscriber();

    bool operator==(const EventSubscription &other) const;

private:
    EventPublisher* _pub;
    uint32_t _eventType;
    EventSubscriber* _sub;
};

#endif /* __EVENTSUBSCRIPTION_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
