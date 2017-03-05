unit ledpanel;

{ LED - Panel

 Nicht-Visuelle Componente zur Darstellung einer beliebigen Anzahl
 von zusammenhängenden LED

 Fest Größe: 25 x 25
  Elternelement im Haupt-Formular: bspw TPanel

Anwendung:
Einbinden:
Menu-> Projekt-> Projekteinstellungen-> Pfade-> andere Units (-Fu)-> ../Lib/LedPanel

uses: ..., ledpanel;

Deklaration;
	ledFrame1: tLEDFrame;

Initialisierung:
  ledFrame1:= tLedFrame.Create(parent_panel, maxLED);
  ledFrame1.Parent:= parent_panel;
//ledFrame1.Orientation := ledVert;		//Default: ledHorz
//ledFrame1.Space:= 10								//Default: 5

  //Positionierung im Eltern-Panel
	ledFrame1.Top := 5;
	ledFrame1.Left := 5;
	panel1.Width := ledframe1.Width + 10;
	panel1.Height := ledframe1.Height +10;

LED Schalten:
	ledFrame1.SetLED(index, ledstatus);	//s. tLedStatus
  ledframe1.SetAllGray();

}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls;

{ TLEDFrame }
const
	ledImgSize = 25;  // Images -> 23 x 23 -> margin = 1

type
  tLedOrientation = (ledHorz, ledVert);

  //LED-Zustände - Reihenfolge muss mit TImageList übereinstimmen
  tLedStatus = (ledDisabled,
  							ledGnOff, ledGnOn,
  							ledRtOff, ledRtOn,
                ledGeOff, ledGeOn,
                ledBlOff, ledBlOn);

type
  TLEDFrame = class(TFrame)
    imgLED: TImageList;

  private
    list: tList;

    FSpace: integer;
    FOrientation: tLEDOrientation;

    procedure SetSpace(m: integer);
    procedure SetLedOrientation(o: tLedOrientation);

    procedure Arrange();
    function GetImg(ix: integer): tImage;

  public
    property Space: integer read FSpace write SetSpace ;
    property Orientation: tLedOrientation read FOrientation write SetLedOrientation;

    constructor {%H-}Create(AOwner: tComponent; cnt: integer);
  	destructor Destroy(); override;

    procedure SetLED(ix: integer; status: tLedStatus);
    procedure SetAllGray();
  end;

implementation

{$R *.lfm}

constructor tLEDFrame.Create(AOwner: tComponent; cnt: integer);
var i: integer;
  	img: tImage;

begin
  inherited create(AOwner);
  if cnt < 1 then cnt:= 1; //min. eine LED
  list:= tList.Create;
  FSpace:= 5;
  FOrientation:= ledHorz;

  for i:= 0 to cnt-1 do begin
  	img:= tImage.Create(self);
    img.Parent:= Self;
    img.Width := ledImgSize;
    img.Height:= ledImgSize;
    list.Add(img);
  end;

  Arrange(); //neu Anordnen
end;

destructor tLEDFrame.Destroy();
var i: integer;
begin
  for i:= 0 to list.Count-1 do GetImg(i).Free;
  list.Free;
  inherited destroy;
end;

//-----------------------------------------------------------------------------
//Propeties

procedure tLEDFrame.SetSpace(m: integer);
begin
  FSpace:= m;
  Arrange();
end;

procedure tLEDFrame.SetLedOrientation(o: tLedOrientation);
begin
  FOrientation:= o;
  Arrange();
end;

//-----------------------------------------------------------------------------

//Anordnen der LED - Images
procedure tLEDFrame.Arrange();
var i, m, n: integer;
    img: tImage;
begin
  m:= Space;
  n:= list.Count;
	if Orientation = ledHorz then begin
  	width:= n * ledImgSize + (n-1) * m;
    height:= ledImgSize;
    for i:= 0 to n-1 do begin
      img:= GetImg(i);
      img.Left := i*ledImgSize + i*m;
      img.Top := 0;
      SetLED(i, ledDisabled);
    end;
  end else begin //vertikal
    height:= n * ledImgSize + (n-1) * m;
    width:= ledImgSize;
    for i:= 0 to n-1 do begin
      img:= GetImg(i);
      img.Top := i*ledImgSize + i*m;
      img.Left := 0;
      SetLED(i, ledDisabled);
    end;
  end;
end;

function tLEDFrame.GetImg(ix: integer): tImage;
begin
	result:= tImage(list.Items[ix]);
end;

procedure tLEDFrame.SetLED(ix: integer; status: tLedStatus);
var img: tImage;
    n: integer;
begin
  if (ix > -1) and (ix < list.Count) then begin
    img:= GetImg(ix);
    n:= integer(status);
  	imgLED.Draw(img.Canvas, 1, 1, n, true);
    img.Refresh;

  end;
end;

procedure tLEDFrame.SetAllGray();
var i: integer;
begin
  for i:= 0 to list.Count-1 do SetLED(i, ledDisabled);
end;

end.




