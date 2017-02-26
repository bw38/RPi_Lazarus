unit rotary;

// Rotary Encoder -> ALPS EC11B02

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Forms, ExtCtrls, Graphics, Types,
  wiringpi
	;

type
TISREvent = procedure(x: longint) of object;


type
TRotary = class (TPaintbox)
  protected


  private
    sigA, sigB: integer;
  	err: integer; 			//allgemeine Fehlermeldung
    debounce: double;		//f. Zeitspann Kontaktentprellung
    center: TPoint; //Mittelpunkt
    bmp: tBitmap;

    FValue: integer;   //aktueller Wert
    FOnChanged: TNotifyEvent;
    FOutSize: integer;  //outer circle
    FInSize: integer;		//inner circle
    FMaxValue: integer;

    procedure Changed(x: longint);
    procedure SetOutSize(sz: integer);
    procedure SetInSize(sz: integer);
    procedure SetValue(val: integer);
    procedure SetView(val: integer);
	public
    clBackGround: TColor;
    clActive, clPassive: TColor;
    clText: TColor;

    property Value: integer read FValue write SetValue;
    property onChanged: tNotifyEvent read FOnChanged write FOnchanged;

    property outSize: integer read FOutSize write SetOutSize;
    property InSize: integer read FInSize Write SetInSize;

    property maxValue: integer read FMaxValue write FMaxValue;

		constructor create(aOwner: TComponent; _sigA, _sigB: integer; hwlink: integer);
    destructor destroy; override;

    procedure Paint; override;
end;


implementation



//*************************************************************************
//Callback-Procedure muss global definiert sein
//Sychronisation mit LCL erforderlich

//Drehimpulsgeber 0
var event0: tISREvent;
procedure Rotary0_Callback;
begin
  Application.QueueAsyncCall(event0, 0);
end;

//Drehimpulsgeber 1
var event1: tISREvent;
procedure Rotary1_Callback;
begin
  Application.QueueAsyncCall(event1, 0);
end;

//Drehimpulsgeber 2
var event2: tISREvent;
procedure Rotary2_Callback;
begin
  Application.QueueAsyncCall(event2, 0);
end;

//Drehimpulsgeber 3
var event3: tISREvent;
procedure Rotary3_Callback;
begin
  Application.QueueAsyncCall(event3, 0);
end;

//ggf erweitern
//************************************************************************



//WiringPi-Setup zuvor im Hauptprogramm !
constructor TRotary.create(aOwner: TComponent; _sigA, _sigB: integer; hwlink: integer);
begin
  inherited create(aOwner);
  sigA:= _sigA;
  sigB:= _sigB;
  pinmode(sigA, pmINPUT); //Signal A
  pullUpDnControl(sigA, pullUp);	//Eingang hochziehen -> 3,3V


  pinmode(sigB, pmINPUT); //Signal B
  pullUpDnControl(sigB, pullUp);

  FValue:= 0;
	debounce:= now;

  //Verbindung zur globalen Callback-Procedure
  //f. max 4 Signalgeber vorbereitet
  case hwlink of    //interrupt auf beiden Flanklen auslösen
  	0:  begin
      		event0:= @Self.Changed;
          err:= wiringPiISR(sigA, intBoth, @Rotary0_Callback);
    		end;
  	1:  begin
      		event1:= @Self.Changed;
          err:= wiringPiISR(sigA, intBoth, @Rotary1_Callback);
    		end;
    2:  begin
      		event2:= @Self.Changed;
          err:= wiringPiISR(sigA, intBoth, @Rotary2_Callback);
    		end;
   else begin
      		event3:= @Self.Changed;
          err:= wiringPiISR(sigA, intBoth, @Rotary3_Callback);
    		end;
  end;


  //Visuelle Componente per default verstecken
  visible:= false;
  width:= 0;
  height:= 0;
  maxValue:= 30;

  clBackGround:= clLTGRAY;
  clActive:= clBlue;
  clPassive:= clGray;
  clText:= clBlack;
  bmp:= tBitmap.Create;

end;


destructor TRotary.destroy;
begin
  bmp.Free;
  inherited destroy;
end;


procedure TRotary.Changed(x: longint);
begin
	if MilliSecondsBetween(debounce, now) > 20 then begin //entprellen
  	if digitalRead(sigA) = digitalRead(sigB)
    	then Value:= Value - 1 	// ccw -Counter Clockwise
    	else Value:= Value + 1;	// cw  -Clockwise
    debounce:= now;
    if assigned(onChanged) then onChanged(Self);
    SetView(Value);
  end;
end;

//------------------------------------------------------------------------

procedure TRotary.SetOutSize(sz: integer);
begin
  width:= sz;
  height:= sz;
  bmp.Width := sz;
  bmp.Height := sz;
  FOutSize:= sz;

  visible:= true;
  center.x:= width div 2 + 1;
  center.y:= height div 2 + 1;
end;

procedure TRotary.SetInSize(sz: integer);
begin
  FInSize:= sz;
end;

procedure TRotary.SetValue(val: integer);
begin
  //val begrenzen
  if val < 0 then val:= 0;
  if val > maxValue then val:= maxValue;
  FValue:= val;
  SetView(val);
end;



procedure TRotary.SetView(val: integer);

const aPassiv = 45;

var cw: integer;
  	a, b, amax, aStep: double;
    sz: TSize;
    s: string;

begin
  with bmp.Canvas do begin
		pen.Width := 0;
    brush.Color :=  clBackGround;
    FillRect(0,0,outSize,outSize);

    amax:= 360 - aPassiv;			//Winkel Regelumfang
    aStep:= amax / maxValue;	//Step-Angle

		pen.Color := clActive;
    brush.Color := clActive;
    a:= 270 - aPassiv/2; // - aGap_2;							//Nullpunkt
    b:= -aStep * Value;
    RadialPie(2, 2, outSize-2, outSize-2, round(a*16), round(b*16));

    pen.Color := clPassive;
    brush.Color := clPassive;
    a:= a + b;
    b:= -aStep * (maxValue-Value) ;
    RadialPie(2, 2, outSize-2, outSize-2, round(a*16), round(b*16));

    //Center-Circle
    cw:= FInSize div 2;
    pen.Color := clBackGround;
    brush.Color := clBackGround;
  	Ellipse(center.x-cw, center.y-cw, center.x+cw, center.y+cw);

    //Text
    s:= IntToStr(val);
    Font.Size := InSize div 4;
    sz:= TextExtent(s);
    TextOut(center.x - sz.cx div 2, center.y -sz.cy div 2, s);
  end;
   Paint();
end;

procedure TRotary.Paint();
begin
  Canvas.Draw(0,0, bmp);
end;

end.



