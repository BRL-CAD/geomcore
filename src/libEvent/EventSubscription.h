/*             E V E N T S U B S C R I P T I O N . H
 * BRLCAD
 *
 * Copyright (c) 2010 United States Government as represented by
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

#define ALL_PUBLISHERS 	0
#define ALL_TYPES	0

#include <Qt>
#include "IEventPublisher.h"
#include "IEventSubscriber.h"

class EventSubscription
{
public:
    EventSubscription(IEventSubscriber* sub, quint32 eventType = ALL_TYPES, IEventPublisher* pub = ALL_PUBLISHERS);
    virtual ~EventSubscription();

    IEventPublisher* getPublisher();
    quint32 getEventType();
    IEventSubscriber* getEventSubscriber();

private:
    IEventPublisher* _pub;
    quint32 _eventType;
    IEventSubscriber* _sub;
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