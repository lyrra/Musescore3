//=============================================================================
//  MuseScore
//  Music Composition & Notation
//
//  Copyright (C) 2009 Werner Schweer
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License version 2
//  as published by the Free Software Foundation and appearing in
//  the file LICENCE.GPL
//=============================================================================

#include "fraction.h"
#include "mscore.h"

namespace Ms {

//---------------------------------------------------------
//   gcd
//    greatest common divisor
//---------------------------------------------------------

static int gcd(int a, int b)
      {
      if (b == 0)
            return a < 0 ? -a : a;
      return gcd(b, a % b);
      }

//---------------------------------------------------------
//   lcm
//    least common multiple
//---------------------------------------------------------

static unsigned lcm(int a, int b)
      {
      return a * b / gcd(a, b);
      }

//---------------------------------------------------------
//   Fraction
//---------------------------------------------------------

Fraction::Fraction(int z, int n)
   : _numerator(z), _denominator(n)
      {
      }

//---------------------------------------------------------
//   reduce
//---------------------------------------------------------

void Fraction::reduce()
      {
      int tmp = gcd(_numerator, _denominator);
      _numerator /= tmp;
      _denominator /= tmp;
      }

//---------------------------------------------------------
//   reduced
//---------------------------------------------------------

Fraction Fraction::reduced() const
      {
      int tmp = gcd(_numerator, _denominator);
      return Fraction(_numerator / tmp, _denominator / tmp);
      }

//---------------------------------------------------------
//   absValue
//---------------------------------------------------------

Fraction Fraction::absValue() const
      {
      return Fraction(std::abs(_numerator), std::abs(_denominator));
      }

// --- comparison --- //

bool Fraction::operator<(const Fraction& val) const
      {
      const int v = lcm(_denominator, val._denominator);
      return _numerator * (v / _denominator) < val._numerator * (v / val._denominator);
      }

bool Fraction::operator<=(const Fraction& val) const
      {
      const int v = lcm(_denominator, val._denominator);
      return _numerator * (v / _denominator) <= val._numerator * (v / val._denominator);
      }

bool Fraction::operator>=(const Fraction& val) const
      {
      const int v = lcm(_denominator, val._denominator);
      return _numerator * (v / _denominator) >= val._numerator * (v / val._denominator);
      }

bool Fraction::operator>(const Fraction& val) const
      {
      const int v = lcm(_denominator, val._denominator);
      return (_numerator * (v / _denominator)) > (val._numerator * (v / val._denominator));
      }

bool Fraction::operator==(const Fraction& val) const
      {
      const int v = lcm(_denominator, val._denominator);
      return (_numerator * (v / _denominator)) == (val._numerator * (v / val._denominator));
//      return (_numerator == val._numerator) && (_denominator == val._denominator);
      }

bool Fraction::operator!=(const Fraction& val) const
      {
      const int v = lcm(_denominator, val._denominator);
      return (_numerator * (v / _denominator)) != (val._numerator * (v / val._denominator));
//      return (_numerator != val._numerator) || (_denominator != val._denominator);
      }

//---------------------------------------------------------
//   operator+=
//---------------------------------------------------------

Fraction& Fraction::operator+=(const Fraction& val)
      {
      reduce();  // here and after: this is necessary to prevent overflow of int multiplication
      Fraction value = val.reduced();
      const int tmp = lcm(_denominator, value._denominator);
      _numerator = _numerator * (tmp / _denominator) + value._numerator * (tmp / value._denominator);
      _denominator = tmp;
      reduce();
      return *this;
      }

//---------------------------------------------------------
//   operator-=
//---------------------------------------------------------

Fraction& Fraction::operator-=(const Fraction& val)
      {
      reduce();
      Fraction value = val.reduced();
      const unsigned tmp = lcm(_denominator, value._denominator);
      _numerator = _numerator * (tmp / _denominator) - value._numerator * (tmp / value._denominator);
      _denominator  = tmp;
      reduce();
      return *this;
      }

//---------------------------------------------------------
//   operator*=
//---------------------------------------------------------

Fraction& Fraction::operator*=(const Fraction& val)
      {
      reduce();
      Fraction value = val.reduced();
      _numerator *= value._numerator;
      _denominator  *= value._denominator;
      reduce();
      return *this;
      }

Fraction& Fraction::operator*=(int val)
      {
      reduce();
      _numerator *= val;
      reduce();
      return *this;
      }

//---------------------------------------------------------
//   operator/=
//---------------------------------------------------------

Fraction& Fraction::operator/=(const Fraction& val)
      {
      reduce();
      Fraction value = val.reduced();
      _numerator *= value._denominator;
      _denominator  *= value._numerator;
      reduce();
      return *this;
      }

Fraction& Fraction::operator/=(int val)
      {
      reduce();
      _denominator *= val;
      return *this;
      reduce();
      }

//---------------------------------------------------------
//   fromTicks
//---------------------------------------------------------

Fraction Fraction::fromTicks(int ticks)
      {
      return Fraction(ticks, MScore::division * 4).reduced();
      }

//---------------------------------------------------------
//   ticks
//---------------------------------------------------------

int Fraction::ticks() const
      {
      // MScore::division - ticks per quarter note
      // MScore::division * 4 - ticks per whole note
      // result: rounded (MScore::division * 4 * _numerator * 1.0 / _denominator) value
      return (_numerator * MScore::division * 4 + (_denominator/2)) / _denominator;
      }

} // namespace Ms

