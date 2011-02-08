/*              S E S S I O N M A N A G E R . C X X
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
/** @file SessionManager.cxx
 *
 * Provides management functions for active Sessions.
 *
 */

#include "SessionManager.h"
#include "AccountManager.h"
#include "NetMsgTypes.h"
#include "FailureMsg.h"

#include <QtCore/QMutexLocker>

SessionManager* SessionManager::pInstance = NULL;

SessionManager::SessionManager()
{
    this->log = Logger::getInstance();
}

SessionManager::~SessionManager()
{}

SessionManager* SessionManager::getInstance()
{
    if (!SessionManager::pInstance) {
    	pInstance = new SessionManager();
    }
    return SessionManager::pInstance;
}

Session*
SessionManager::newSession(Account* a)
{
	Session* s = this->getSession(a);

    if (s != 0) {
    	s->stampLastAccess();
    } else {
		s = new Session(a);
		this->putCache(s);
    }
    return s;
}

Session*
SessionManager::getSession(Account* a) {
	QMutexLocker locker(&this->listLock);
	for (int i = 0; i < this->sessionList.size(); ++i) {
		Session* s = this->sessionList[i];
		if (s->getAccount() == a)
			return s;
	}
	return NULL;
}

//TODO need to verify the QUuid == QUuid works.
Session*
SessionManager::getSession(QUuid sessID) {
	QMutexLocker locker(&this->listLock);
	for (int i = 0; i < this->sessionList.size(); ++i) {
		Session* s = this->sessionList[i];
		if (s->getSessionID() == sessID)
			return s;
	}
	return NULL;
}

Session*
SessionManager::getSession(Portal* p) {
	QMutexLocker locker(&this->listLock);
	for (int i = 0; i < this->sessionList.size(); ++i) {
		Session* s = this->sessionList[i];
		if (s->getAccount()->getPortal() == p)
			return s;
	}
	return NULL;
}

void
SessionManager::putCache(Session* s)
{
	QMutexLocker locker(&this->listLock);
	if (this->sessionList.contains(s)) {
		log->logWARNING("SessionManager", "Attempted to cache an already cached Session.");
	} else {
		this->sessionList.append(s);
	}
}

void
SessionManager::remCache(Session* s)
{
	QMutexLocker locker(&this->listLock);
	if (this->sessionList.contains(s)) {
		this->sessionList.removeAll(s);
	} else {
		log->logWARNING("SessionManager", "Attempted to remove a Session that wasn't cached.");
	}
}

bool
SessionManager::handleNetMsg(NetMsg* msg)
{
	quint16 type = msg->getMsgType();
	switch(type) {
	case NEWSESSIONREQ:
		this->handleNewSessionReqMsg((NewSessionReqMsg*)msg);
		return true;
	case SESSIONINFO:
		//Dunno why someone would be sending the GS this message!
		return true;
	case DISCONNECTREQ:
		this->handleDisconnectReqMsg((TypeOnlyMsg*)msg);
		return true;
	}
	return false;
}

/*
 * Msg Handlers
 */
void SessionManager::handleNewSessionReqMsg(NewSessionReqMsg* msg)
{
/*	log->logINFO("SessionManager", "Recv'ed NewSessionReqMsg."); */

	/* Check for a good pointer to msg */
	if (msg == NULL) {
		log->logERROR("SessionManager", "handleNewSessionReMsg(): NULL NewSessionReqMsg!");
		return;
	}

	/* Get values from msg */
	Portal* origin = msg->getOrigin();
	QString uname = msg->getUName();
	QString passwd = msg->getPasswd();

	/* Validate data */
	if (origin == 0) {
		//TODO Figure out how to how to handle NULL Portal
		log->logERROR("SessionManager", "handleNewSessionReMsg(): NULL Portal!");
		return;
	}

	/* Quick LEN check on strings. */
	if (uname.length() <=0 || passwd.length() <= 0) {
		log->logINFO("SessionManager", "Auth FAILED.  Zero len uname or passwd. Disconnecting.");
		FailureMsg fail(msg, ACCOUNT_VALIDATION_FAIL);
		origin->sendThenDisconnect(&fail);
		return;
	}

	/* Attempt to validate creds */
	Account* a = AccountManager::getInstance()->login(uname, passwd, origin);

	if (a == 0) {
		/* Account validation failed */
		log->logINFO("SessionManager", "Auth FAILED.  Bad uname or passwd. Disconnecting.");
		FailureMsg fail(msg, ACCOUNT_VALIDATION_FAIL);
		origin->sendThenDisconnect(&fail);
		return;
	}

	/* Make new session */
	Session* s = this->newSession(a);

	/* Tell client about it. */
	SessionInfoMsg* info = s->generateSessionInfoMsg();
	origin->send(info);
}

void
SessionManager::handleDisconnectReqMsg(TypeOnlyMsg* msg)
{
/*	log->logINFO("SessionManager", "Recv'ed DisconnectReqMsg."); */
	Portal* origin = msg->getOrigin();

	/* validate incoming data */
	if (origin == 0) {
		//TODO Figure out how to how to handle NULL Portal
		log->logERROR("SessionManager", "handleDisconnectReqMsg(): NULL Portal!");
		return;
	}

	/* Remove Session */
	Session* s = this->getSession(origin);
	this->remCache(s);

	/* Remove Account */
	Account* a = s->getAccount();
	AccountManager::getInstance()->logout(a);
	delete a;
}

/*
 * Local Variables: ***
 * mode: C++ ***
 * tab-width: 8 ***
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t ***
 * End: ***
 * ex: shiftwidth=2 tabstop=8
*/
