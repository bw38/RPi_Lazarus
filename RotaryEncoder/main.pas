unit main;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Math,
  ExtCtrls, Buttons, rotary, wiringpi
  ;
{ TForm1 }

type
  TForm1 = class(TForm)
    Panel1:TPanel;
    btUP:TSpeedButton;
    btDWN:TSpeedButton;
    Panel2: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure btUPClick(Sender:TObject);
    procedure btDWNClick(Sender:TObject);
  private
    R1, R2: TRotary;
    procedure RotaryChanged(Sender: TObject);

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
	//Setup in WiringPi-Numbering
  wiringPiSetup();
  R1:= TRotary.create(Self, 21, 22, 0); //sigA, sigB, HW-id (0..3)
  with R1 do begin
  	onChanged:= RotaryChanged;
  	Parent:= Panel1;
  	outSize:= min(Panel1.Width-10, Panel1.Height-10);
  	InSize := round(outSize * 0.5);
  	Left:= 5;
  	Top:= 5;
  	maxValue := 30;
    Value:= 0;
  end;  //R1

  R2:= TRotary.create(Self, 23, 24, 1);
  with R2 do begin
  	onChanged:= RotaryChanged;
  	Parent:= Panel2;
  	outSize:= min(Panel2.Width-10, Panel1.Height-10);
  	InSize := round(outSize * 0.5);
  	Left:= 5;
  	Top:= 5;
  	maxValue := 30;
    Value:= 15;
  end;

end;


//Rotary-Event
procedure TForm1.RotaryChanged(Sender: TObject);
begin
  if Sender = R1 then begin
    //.....
  end;
end;


//------------------------------------------------------------------------------
//Test-Helper
procedure TForm1.btUPClick(Sender:TObject);
begin
  R1.Value := R1.Value + 1;
end;

procedure TForm1.btDWNClick(Sender:TObject);
begin
  R1.Value := R1.Value - 1 ;
end;

end.

