// interface header
#include "Utility/Timer.h"

// XXX temp
#include <iostream>

using namespace Utility;

/* protected */

double Timer::elapsedSinceStart()
{
  if (!_started) {
    return 0.0;
  }
  return ((double)clock() - (double)_start) / (double)CLOCKS_PER_SEC;
}


/* public */

Timer::Timer() :
  _running(false),
  _started(false),
  _accumulated(0.0)
{
}


Timer::Timer(const Timer& timer)
{
  _running = timer._running;
  _started = timer._started;
  _start = timer._start;
  _accumulated = timer._accumulated;
}


Timer::~Timer()
{
}


void Timer::start()
{
  if (_running) {
    return;
  }

  _running = _started = true;
  _start = clock();
}


void Timer::stop()
{
  if (!_running) {
    return;
  }
  _running = false;

  _accumulated += elapsedSinceStart();
}


void Timer::reset()
{
  _accumulated = 0.0;
  _start = clock();
}


double Timer::elapsed()
{
  if (_running) {
    return elapsedSinceStart() + _accumulated;
  }
  return _accumulated;
}

// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 2 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=2 tabstop=8
