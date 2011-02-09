/*           E V E N T S U B S C R I P T I O N . C X X
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
/** @file EventSubscription.cxx
 *
 * Brief description
 *
 */

#include "EventSubscription.h"

EventSubscription::EventSubscription(EventSubscriber* sub, uint32_t eventType,
	EventPublisher* pub) :
    _sub(sub), _eventType(eventType), _pub(pub)
{}

EventSubscription::~EventSubscription()
{}

EventPublisher* EventSubscription::getPublisher()
{
    return this->_pub;
}

uint32_t EventSubscription::getEventType()
{
    return this->_eventType;
}

EventSubscriber* EventSubscription::getEventSubscriber()
{
    return this->_sub;
}

bool EventSubscription::operator==(const EventSubscription &other) const
{
    Logger* log = Logger::getInstance();

    if (other._eventType != this->_eventType) {
//	log->logINFO("EventSubscription", "Equality Check: Event Type Mismatch (" + QString::number(other._eventType) + "/" + QString::number(this->_eventType) + ")");
	return false;
    }
    if (other._pub != this->_pub) {
//	log->logINFO("EventSubscription", "Equality Check: Publisher Pointer Mismatch");
	return false;
    }
    if (other._sub != this->_sub) {
//	log->logINFO("EventSubscription", "Equality Check: Subscriber Pointer Mismatch");
	return false;
    }

//	log->logINFO("EventSubscription", "Equality Check: EQUAL");
    return true;
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
