/*                         E V E N T . H
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
/** @file Event.h
 *
 * Brief description
 *
 */

#ifndef __EVENT_H__
#define __EVENT_H__

#include <string>

class EventPublisher;

class Event
{
public:
	Event(EventPublisher* pub, uint32_t eventType);
	Event(EventPublisher* pub, uint32_t eventType, std::string message);
	virtual ~Event();

	EventPublisher* getPublisher() const;
	uint32_t getEventType() const;
	std::string getMessage() const;

private:
	EventPublisher* _pub;
	uint32_t _eventType;
	std::string _message;

	/* Disable copy cstr and =operator */
	Event(Event const&){};
	Event& operator=(Event const&){};
};

#endif /* __EVENT_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
