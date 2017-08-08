program gpioset;

{$mode delphi}

{
  GPIO für zur Ansteurung des pHAT ESP8266 setzen
  GPIO 0 (BCM.17) - ALT3 -> RTS0
  GPIO 2 (BCM.27) - OUT = 1

  GPIO 2 wird über die Console umgeschaltet
  # gpio write 2 1 -> Anwendung starten
  # gpio write 2 0 -> Programmiermodus

  RTS0 wird durch esptool.py gesteuert

  Raspberry Pi3, ZeroW
  08.08.2017
}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,cmem,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, wiringPi;


type
  TGPIOSetup = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TGPIOSetup }

procedure TGPIOSetup.DoRun;
var
  ErrorMsg: String;
  altX: tAltMode;
begin
  // quick check parameters
  ErrorMsg := CheckOptions('hr', 'help reset');
  if ErrorMsg <> '' then begin
    writeln(ErrorMsg);
    writeln('----------------------------------');
    WriteHelp();
    Terminate;
    Exit;
  end;


  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp();
    Terminate;
    Exit;
  end;

  //User-Code --------------------------------------

  //BCM.17 für 10ms low setzen
  if HasOption('r', 'reset') then begin
    wiringPiSetup();
    altX:= getAlt(0);
    pinModeAlt(0, amAlt0);
    pinMode(0, pmOUTPUT);
    digitalWrite(0, levLow);
    sleep(10);
    digitalWrite(0, levHigh);
    pinModeAlt(0, altX);
    Terminate;
    Exit;
  end;

  //Ohne Parameter
  wiringPiSetup();
	pinMode(2, pmOUTPUT);
  pinModeAlt(0, amAlt3);

  digitalWrite(2, levHigh);
  //------------------------------------------------
  // stop program loop
  Terminate;
end;

constructor TGPIOSetup.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

destructor TGPIOSetup.Destroy;
begin
  inherited Destroy;
end;

procedure TGPIOSetup.WriteHelp;
var s: string;
begin
  s:= ExtractFileName(ExeName);
  writeln('Usage: '+ ExeName+':');
  writeln('   ' + s + ' [ohne Parameter] => Initialisierung BCM.17 -> RTS0 (ALT3) / BCM.27 -> OUTPUT');
  writeln('   ' + s + ' -r --reset       => Reset-Impuls (10ms) an BCM.17');
  writeln('   ' + s + ' -h --help        => Helpfile');
end;

var
  Application: TGPIOSetup;
begin
  Application := TGPIOSetup.Create(nil);
  Application.Title := 'GPIOSetup';
  Application.Run;
  Application.Free;
end.

