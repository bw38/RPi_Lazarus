unit main;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  wiringPi;

type
TISREvent = procedure(x: longint) of object;

type

{ TForm1 }

TForm1 = class(TForm)
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
  	procedure Changed(x: longint);
  public

  end;

var Form1: TForm1;

implementation

{$R *.lfm}



//------------------------------------------------
//global Callback
var event0: tISREvent;
procedure Global_Callback;
begin
  //Send a message to MainThread
  Application.QueueAsyncCall(event0, 0);
end;
//----------------------------------------------

procedure TForm1.FormCreate(Sender: TObject);
var err: integer;
begin
  wiringPiSetup(); //Setup in WiringPi-Numbering
  pinmode(4, pmINPUT);
  pullUpDnControl(4, pullUp);	// -> 3,3V

  //Callback initialize
  event0:= Self.Changed;
  err:= wiringPiISR(4, intRising, @Global_Callback);
end;


procedure TForm1.Changed(x: longint);
begin
  Label1.Caption:= 'Interrupt detected';
end;

end.

