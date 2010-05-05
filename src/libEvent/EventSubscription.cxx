/*           E V E N T S U B S C R I P T I O N . C X X
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
/** @file EventSubscription.cxx
 *
 * Brief description
 *
 */

#include "EventSubscription.h"

EventSubscription::EventSubscription(IEventSubscriber* sub, quint32 eventType, IEventPublisher* pub):
_sub(sub), _eventType(eventType), _pub(pub)
{
}

EventSubscription::~EventSubscription()
{
}

IEventPublisher* EventSubscription::getPublisher()
{
    return this->_pub;
}
quint32 EventSubscription::getEventType()
{
    return this->_eventType;
}
IEventSubscriber* EventSubscription::getEventSubscriber()
{
    return this->_sub;
}

// Local Variables:
// tab-width: 8
// mode: C++
// c-basic-offset: 4
// indent-tabs-mode: t
// c-file-style: "stroustrup"
// End:
// ex: shiftwidth=4 tabstop=8
