unit main;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, lNetComponents, lNet, Math, Types, DataFile;

type
  TDesign = record
    outFrame,
    outEdge,
    outBack,
    clockFace,
    clockValField,
    clockValTxt,
    scaleLine,
    scaleText,
    needle,
    txt,
    trend: tColor;
  end;


const ratio = 1.5; //width = hSize * ratio

dsgn_Dark: TDesign = ( // B / G / R
	outFrame:			TColor($808080);
  outEdge:			TColor($C0C0C0);
  outBack:			TColor($606000);
  clockFace:  	TColor($004040);
  clockValField:TColor($80F080);
  clockValTxt:	TColor($000000);
  scaleLine:		TColor($B0B0B0);
  scaleText:		TColor($E0F0E0);
  needle:     	TColor($0000FF);
  txt:					TColor($FFFFFF);
  trend:				TColor($FF0000);
);

type
  TForm1 = class(TForm)
    dlgColor:TColorDialog;
    Memo1:TMemo;
    SpeedButton2:TSpeedButton;
    SpeedButton1:TSpeedButton;
    procedure FormCreate(Sender:TObject);
    procedure FormDestroy(Sender:TObject);
    procedure SpeedButton1Click(Sender:TObject);
    procedure SpeedButton2Click(Sender:TObject);
    procedure srv1Accept(aSocket:TLSocket);
    procedure srv1Receive(aSocket:TLSocket);
  private
    bmp: tBitmap;
    spng: tMemoryStream;
    srv1:TLTCPComponent;

    dsgn: TDesign;
    procedure DrawGauge(hSize: integer);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender:TObject);
begin
	bmp:= tBitmap.Create;
  bmp.Width := 0;
  bmp.Height := 0;
  spng:= tMemoryStream.Create;
  srv1:= TLTCPComponent.Create(self);
  srv1.OnAccept := srv1Accept;
  srv1.OnReceive := srv1Receive;
  if not srv1.Listen(38180)  then Memo1.Lines.Add('Fehler');
end;



procedure TForm1.FormDestroy(Sender:TObject);
begin
  srv1.Listen(0, '127.0.0.1');
  srv1.Free;
  spng.Free;
  bmp.Free;
end;

//****************************************************************************


const level = 57;
			minLevel = 30;
      maxlevel = 60;
      temp = 16.25;
      mintemp = 12.2;
      maxtemp = 18.75;
      time = 5011.251;
      idtxt = 'Regentonne';
      interval = 30;


procedure TForm1.DrawGauge(hSize: integer);
var png : TPortableNetworkGraphic;
  w, h, ang, szval, sz, x, y, mxt: integer;
  mcf, pA, pE, pT: TPoint;
  rsA1, rsA2, rsE, rsT, rsN, a: real;
  can: tCanvas;
  rval: TRect;
  tsz: TSize;
  s: string;

  //Umrechnung in Prozent bezogen auf Gesamthöhe
  function XY(p: real): integer;
  begin
    result:= round(hSize * (p/100));
  end;

begin
  h:= hSize;
  w:= round(hSize * ratio);
	bmp.Width := w;
	bmp.Height := h;

  mcf.x := XY(50);  //Mittelpunkt Ziffernblatt
  mcf.y := XY(50);

  can:= bmp.Canvas;
  can.AntialiasingMode:=  amOn;
  can.Pen.Color := clBlack;
  can.Pen.Width := 0;
  can.Brush.Color := dsgn.outframe;
  can.Rectangle(0, 0, w-1 , h-1);
  can.Brush.Color := dsgn.outEdge;
  can.Rectangle(XY(2), XY(2), w-XY(2), h-XY(2));
  can.Brush.Color := dsgn.outBack;
  can.Rectangle(XY(4), XY(4), w-XY(4), h-XY(4));
  //Ziffernblatt
  can.Brush.Color := dsgn.outFrame;
  can.Ellipse(mcf.x-XY(42), mcf.y-XY(42), mcf.x +XY(42), mcf.y+XY(42));
  can.Brush.Color := dsgn.clockFace;
  can.Ellipse(mcf.x-XY(39), mcf.y-XY(39), mcf.x +XY(39), mcf.y+XY(39));
  //Skala
  rsA1:= XY(32); 	//Radien Skala
  rsA2:= XY(29);
  rsE:= XY(27);
  rsT:= XY(39);
  rsN:= XY(29);  //Needle

  can.Pen.Color := dsgn.scaleLine;
	can.Pen.Width := XY(1.2);
  can.Font.Size := XY(3.2);
  can.Font.Color := dsgn.scaleText;

  ang:= 0;			//Text Skala
  a:= 0.25*pi; 	//Anfangswinkel
  while ang <= 100 do begin
    if ang mod 2 = 0 then begin
	  	pA.x:= mcf.x - round(rsA1*sin(a));   //lange Striche 10%
  		pA.y:= mcf.y + round(rsA1*cos(a));
    end else begin
      pA.x:= mcf.x - round(rsA2*sin(a));   //kurze Striche 5%
  		pA.y:= mcf.y + round(rsA2*cos(a));
    end;
    pE.x:= mcf.x - round(rsE*sin(a));
  	pE.y:= mcf.y + round(rsE*cos(a));
  	can.Line(pA, pE);

		//Prozentwert Skala 0 / 50 / 100
    case ang of   //mit Feinjustage entspr. Textlänge
    	0: begin
    		pT.x:= mcf.x - round(rsT*sin(a-0.02*pi));
  			pT.y:= mcf.y + round(rsT*cos(a-0.02*pi));
    		can.Font.Orientation := round(RadToDeg(a)*(-10))+1800;
    		can.TextOut(pT.x, pT.y, IntToStr(ang)+'%');
    	end;
      50: begin
    		pT.x:= mcf.x - round(rsT*sin(a-0.03*pi));
  			pT.y:= mcf.y + round(rsT*cos(a-0.03*pi));
    		can.Font.Orientation := round(RadToDeg(a)*(-10))+1800;
    		can.TextOut(pT.x, pT.y, IntToStr(ang)+'%');
    	end;
      100: begin
    		pT.x:= mcf.x - round(rsT*sin(a-0.05*pi));
  			pT.y:= mcf.y + round(rsT*cos(a-0.05*pi));
    		can.Font.Orientation := round(RadToDeg(a)*(-10))+1800;
    		can.TextOut(pT.x, pT.y, IntToStr(ang)+'%');
    	end;
    end; //case

  	a:= a + 0.075*pi;
    ang:= ang + 5;
  end;

  //Feld f. mumerischen Wert im Ziffernblatt
  can.Brush.Color := dsgn.clockValField;
  szval:= XY(20);
  rval.Left := mcf.x - szval div 2;
  rval.Right := rval.Left + szval;

  rval.Top:= mcf.y + XY(20);
  rval.Bottom:= rval.Top + round(szval*0.6);
  can.Rectangle(rval);

  //Numerischer Wert im Ziffernblatt
  s:= IntToStr(level) + ' %';
  can.Font.Size := XY(3.5);
  can.Font.Color := dsgn.clockValTxt;
  can.Font.Orientation := 0;
  can.Brush.Color := dsgn.clockValField;
  tsz:= can.TextExtent(s);
  x:= (rval.Right-rval.Left) div 2 - tsz.cx div 2;
  y:= (rval.Bottom-rval.Top) div 2 - tsz.cy div 2;
  can.Textout(rval.Left+x, rval.Top+y, s);

  //Min- / Max-Level
  can.Pen.Color := dsgn.trend;
  can.Pen.Width:= XY(2.8);
  x:= round(rsN);
  can.Arc(
  	mcf.x - x, mcf.y - x, mcf.x + x, mcf.y + x, //Quadrat des Kreises
  	round(225 * 16 + minlevel * 2.7 * -16),			//auf 0% zurückdrehen (225°) - Minwert
    round((maxlevel-minlevel) * 2.7 * -16));		//Winkel Maxwert bezogen auf 270° = 100%

  //Nadel
  a:= level*0.015*pi + 0.25*pi;
  can.Pen.Color := dsgn.needle;
  can.Pen.Width := XY(1.8);
  pE.x:= mcf.x - round(rsN*sin(a));   //lange Striche 10%
  pE.y:= mcf.y + round(rsN*cos(a));
  can.Line(mcf, pE);
  can.Brush.Color := dsgn.needle;
  sz:= XY(8) div 2;
  can.Ellipse(mcf.x - sz, mcf.y - sz, mcf.x + sz, mcf.y + sz  );

  //Texte
  mxt:= XY(120); //Centerline der Textausgaben
  can.Brush.Color := dsgn.outBack;
  can.font.Color := dsgn.txt;

  //Name
  can.Font.Size := XY(5);
  tsz:= can.TextExtent(idtxt);
  y:= XY(8);
  can.TextOut(mxt-tsz.cx div 2, y, idtxt);
  //Timestamp
  s:= FormatDateTime('dd.mm.YY - hh:nn:ss', time);
  can.Font.Size := XY(4);;
  tsz:= can.TextExtent(s);
  y:= XY(20);
  can.TextOut(mxt-tsz.cx div 2, y, s);
  //Temperatur
  s:= 'Temp: ' + FloatToStrF(temp, ffFixed, 7,1) + ' °C';
  tsz:= can.TextExtent(s);
  y:= XY(45);
  can.TextOut(mxt-tsz.cx div 2, y, s);
  //Min-Max-emp
  s:= '[ ' +
  	FloatToStrF(mintemp, ffFixed, 7,1) + ' / ' +
  	FloatToStrF(maxtemp, ffFixed, 7,1) + ' °C ]';
  tsz:= can.TextExtent(s);
  y:= XY(55);
  can.TextOut(mxt-tsz.cx div 2, y, s);
  //Interval
  s:= 'Inteval: ' + IntToStr(interval) + 'min';
  y:= XY(85);
  can.TextOut(mxt-tsz.cx div 2, y, s);

  png:= TPortableNetworkGraphic.Create;
  try
    png.Assign(bmp);
    spng.Clear;
    png.SaveToStream(spng);
    memo1.Lines.Add(IntToStr(spng.Size)+' Bytes');

  finally
    png.Free;
  end;
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
procedure TForm1.SpeedButton1Click(Sender:TObject);
var df: tDatafile;
begin
  dsgn:= dsgn_Dark;
df:= tDataFile.create('iog_colors.ini');
  dsgn.clockFace := 		df.ReadInteger('colors', 'clockFace', dsgn.clockFace);
  dsgn.clockValField := df.ReadInteger('colors', 'clockValField', dsgn.clockValField);
  dsgn.clockValTxt := 	df.ReadInteger('colors', 'clockValTxt', dsgn.clockValTxt);
  dsgn.needle := 				df.ReadInteger('colors', 'needle', dsgn.needle);
  dsgn.outBack := 			df.ReadInteger('colors', 'outBack', dsgn.outBack);
  dsgn.outEdge := 			df.ReadInteger('colors', 'outEdge', dsgn.outEdge);
  dsgn.outFrame := 			df.ReadInteger('colors', 'outFrame', dsgn.outFrame);
  dsgn.scaleLine := 		df.ReadInteger('colors', 'scaleLine', dsgn.scaleLine );
  dsgn.scaleText := 		df.ReadInteger('colors', 'scaleText', dsgn.scaleText);
  dsgn.txt := 					df.ReadInteger('colors', 'txt', dsgn.txt);
  dsgn.trend := 				df.ReadInteger('colors', 'trend', dsgn.trend);
  DrawGauge(300);
  df.Free;
end;


procedure TForm1.SpeedButton2Click(Sender:TObject);
begin

end;

procedure TForm1.srv1Accept(aSocket:TLSocket);
begin
  Memo1.Append('Verbindung zum Client: ' + aSocket.LocalAddress + ' hergestellt');
end;




procedure TForm1.srv1Receive(aSocket:TLSocket);
var msg: string;
   ar2: array of byte;
   sz: dword;
begin
	aSocket.GetMessage(msg);
  memo1.Lines.Add(msg);

  sz:= spng.Size;
  setlength(ar2, sz);
  spng.Position := 0;
  spng.Read(ar2[0], sz);
  aSocket.Send(ar2[0], sz);
  aSocket.Disconnect();
end;



end.

