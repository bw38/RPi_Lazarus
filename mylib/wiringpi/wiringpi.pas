
unit wiringpi;

{ Pascal wrapper unit for Gordon Henderson wiringPi library. The source can
  be found at https://http://wiringpi.com

 * wiringPi:
 *	Arduino compatable (ish) Wiring library for the Raspberry Pi
 *	Copyright (c) 2012 Gordon Henderson
 ***********************************************************************
 * This file is part of wiringPi:
 *	https://projects.drogon.net/raspberry-pi/wiringpi/
 *
 *    wiringPi is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    (at your option) any later version.
 *
 *    wiringPi is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with wiringPi.  If not, see <http://www.gnu.org/licenses/>.
 ***********************************************************************
}

{$linklib c}
{$linklib libwiringPi}

interface

//uses ;

type  //redefinition of wirinPi-Const - s. wirinPi.h


//set in tPinMode  => int 0..6
tPinMode = (pmINPUT, pmOUTPUT, pmPWM_OUTPUT, pmGPIO_CLOCK, pmSOFT_PWM_OUTPUT,
            pmSOFT_TONE_OUTPUT, pmPWM_TONE_OUTPUT);

//digitalRead, digitalWrite => int 0..1
tLevel =(levLOW, levHIGH);

//set in pullUpDownControl => int 0..2
tPull = (pullOFF,	pullDOWN,	pullUP);

//set edge for Interrupt => int 0..3
tIntMode = (intSetup, intFalling, intRising, intBoth);

//set PWM => int 0..1
tPWMMode = (pwmMS, pwmBAL);

//set in pinModeAlt => int 0..7  -> FSEL9 0-7
tAltMode = (amIN, amOUT, amALT5, amALT4, amALT0, amALT1, amALT2, amALT3);

//-----------------------------------------------------------------------------

//Setup - Funktionen
//==================
// http://wiringpi.com/reference/setup/


function wiringPiSetup : longint; cdecl; external;
function wiringPiSetupGpio: longint; cdecl; external;
function wiringPiSetupPhys: longint; cdecl; external;
function wiringPiSetupSys: longint; cdecl; external;


// Core Functions
// ==============
// http://wiringpi.com/reference/core-functions/

procedure pinMode(pin:longint; mode: tPinMode); cdecl; external;
procedure pullUpDnControl(pin: longint; pud: tPull); cdecl; external;
procedure digitalWrite(pin: longint; value: tLevel); cdecl; external;
function  digitalRead(pin: longint): tLevel; cdecl; external;


//undocumented GPIO - functions ------------------------------------------------

procedure pinModeAlt (pin: longint; mode: tAltMode); cdecl; external;
function  getAlt(pi: longint): longint; cdecl; external;


//Interrupt and Thread ---------------------------------------------------------
//http://wiringpi.com/reference/priority-interrupts-and-threads/

function wiringPiISR(pin: longint; mode: tIntMode; pcallbback: pointer):longint; cdecl; external;
{
Lazarus:
-> pcallback has to be a global procedure
-> It starts a separate thread !!!
-> It's NOT synchronized with LCL  !!! use e.g. Application.QueueAsyncCall(...)
}



//RS232 ------------------------------------------------------------------------
//http://wiringpi.com/reference/serial-library/

function  serialOpen (device: pchar; baud: longint): longint; cdecl; external;
procedure serialClose (fd: longint); cdecl; external;
procedure serialPutchar (fd: longint; c: byte) ; cdecl; external;
procedure serialPuts (fd: longint; s: pchar) ; cdecl; external;
function  serialDataAvail (fd: longint): longint; cdecl; external;
function  serialGetchar (fd: longint): integer ; cdecl; external;
procedure serialFlush (fd: longint) ; cdecl; external;

//------------------------------------------------------------------------------

//HW - PWM - (root-permissions)
procedure pwmSetMode(mode: tPWMMode); cdecl; external;
procedure pwmSetRange(range: dword); cdecl; external;
procedure pwmSetClock(divisor: longint); cdecl; external;
procedure pwmWrite(pin: longint; value: longint); cdecl; external;

//SW - PWM  (user-permissions)
function softPwmCreate(pin: longint; initialValue: longint; pwmRange: longint): longint; cdecl; external;
procedure softPwmWrite(pin: longint; value: longint); cdecl; external;
{
 t(mark)    = value * 100µs
 t(Periode) = range * 100µs
 -> one Thread per pin
 -> only one create per pin on runtime, range not changeable
}

//Soft-Tone
function  softToneCreate (pin: longint): longint; cdecl; external;
procedure softToneWrite (pin: longint; freq: longint); cdecl; external;
{
 freq in Hz
 f <= 1kHz -> ok
 f >  1kHz -> bad accurancy
}

// -----------------------------------------------------------------------------

//undocumented procedure
procedure pwmToneWrite(pin: longint; freq: longint); cdecl; external;
procedure gpioClockSet(pin: longint; freq: longint); cdecl; external;

//------------------------------------------------------------------------------

//Pin-Translation

function wpiPinToGpio(wpiPin:longint):longint; cdecl; external;
function physPinToGpio(physPin:longint):longint; cdecl; external;

//------------------------------------------------------------------------------


function piGpioLayout(): longint; cdecl; external;


//------------------------------------------------------------------------------

implementation



end.
