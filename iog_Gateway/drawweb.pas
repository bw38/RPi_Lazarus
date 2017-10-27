unit drawweb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lNet, Graphics, Types, typedef,
  FPImage, FPCanvas, FPImgCanv, FPWritePNG, ftfont;

type
enumGaugeType = (gtLevelMeter, gtThermoMeter);

type
TDrawWidget = class (TThread)

private
  metertype: enumGaugeType;
  spng: TMemoryStream;	//png zur Darstellung im Web-Interface
  img : TFPMemoryImage;
  can : TFPImageCanvas;
  ar2: array of byte;
protected
  procedure Execute; override;
public
  //Grafikhöhe
  hSize: integer;
  //Daten senden an:
  aSocket: TLSocket;
  //Messwerte
	dw_level: integer;
  dw_minLevel: integer;
  dw_maxLevel: integer;
  dw_temp: real;
  dw_minTemp: real;
  dw_maxTemp: real;
  dw_idTxt: string;
  dw_interval: word;
  dw_sensortime: TDateTime;
  dw_ubatt: real;

	constructor Create(mt: enumGaugeType);
	destructor Destroy; override;


end;

implementation


constructor TDrawWidget.Create(mt: enumGaugeType);
begin
	FreeOnTerminate := true;
  inherited Create(true);		//Createsuspended
  spng:= TMemoryStream.Create;
  metertype:= mt;	//Darzustellende Grafik
  //Werte müssen durch Aufrufer gesetzt werden
  //Tread starten durch Aufrufer
end;


destructor TDrawWidget.Destroy;
begin
  spng.Free;
  inherited Destroy;
end;


procedure TDrawWidget.Execute;
var sz: integer;
{$I widget_level.inc} //Auslagerung in separate Dateien
begin
  case metertype of
    gtLevelMeter: draw_widget_level(hSize);

  end;

	sz:= spng.Size;
 	setlength(ar2, sz);
 	spng.Position := 0;
 	spng.Read(ar2[0], sz);
	aSocket.Send(ar2[0], sz);
  aSocket.Disconnect();
  setlength(ar2, 0);
end;

end.

