unit data;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typedef;

type

tPayload = record
  uid: word;
  typ: word;
  version: byte;
  revision: byte;

  Batterie: word;
  RunTime: dword;
  cntTxShot: word;

  Temperature: word;
  StatusTemp: byte;
  ResolutionTemp: byte;

  Level: word;
  StatusLevel: byte;
end;

tDataSet = class
	TimeStamp: tDateTime;
  Batterie: word;				//Batteriespannung in mV
  RunTime: dword;				//Zeit in ms seit letztem Battiewechsel
  cntTxShot: word;      //Sendevesuche

  Temperature: word;    //Temp = x / 16
  StatusTemp: byte;     //1 - Ok
  ResolutionTemp: byte;

  Level: word;
  StatusLevel: byte;

end;


tSensor = class
private
  data: tList;

public
  uid: word;			//Informationen des Sensors
  typ: word;
  version, revision: byte;

  DeepSleep: dword;	//Schlafzeit nach nächstem Telegramm

  constructor create(uid_, typ_: word; ver_, rev_: byte);
  destructor destroy;  override;
  function get(ix: integer): tDataSet;
  function getlast: tDataset;
  function addDataSet(ds: tDataSet): integer;
end;


tSensorList = class
private
	sensors: tList;

public
  constructor create();
  destructor destroy;  override;
  function get(ix: integer): tSensor;
  function indexOf(uid: word): integer;
  function addData(pl: tPayLoad) : integer;
end;

implementation


//------------------------------------------------------------------------------

constructor tSensor.create(uid_, typ_: word; ver_, rev_: byte);
begin
  inherited create;
  data:= tList.Create;
  uid:= uid_;
  typ:= typ_;
  version:= ver_;
  revision:= rev_;
  DeepSleep:= 300 * SEK;    //default 5Min
end;

destructor tSensor.destroy;
var i: integer;
begin
  for i:= 0 to data.Count-1 do get(i).Free;
  data.Free;
  inherited destroy;
end;

//Rückgabe der typisierten Instanz
function tSensor.get(ix: integer): tDataSet;
begin
	result:= nil;
  if (ix >= 0) and (ix < data.Count) then result:= tDataSet(data[ix]);
end;

//zuletzt geschriebener Datensatz des Sensors
function tSensor.getlast: tDataset;
begin
	result:= tDataSet(data[data.Count-1]);
end;

function tSensor.addDataSet(ds: tDataSet): integer;
begin
  result:= data.Add(ds);
end;

//-----------------------------------------------------------------------------

constructor tSensorList.create();
begin
  inherited create;
  sensors:= tList.Create;

end;

destructor tSensorList.destroy;
var i: integer;
begin
  for i:= 0 to sensors.Count-1 do get(i).Free;
  inherited destroy;
end;

//Rückgabe der typisierten Instanz
function tSensorList.get(ix: integer): tSensor;
begin
	result:= nil;
  if (ix >= 0) and (ix < sensors.Count) then result:= tSensor(sensors[ix]);
end;

function tSensorList.indexOf(uid: word): integer;
var ix: integer;
begin
	result:= -1;
  ix:= 0;
  while ix < sensors.Count do begin
  	if get(ix).uid = uid then begin
      result:= ix;
      break;
    end;
    inc(ix);
  end;
end;

//Zentrale Funktion zum Hinzufügen von Sensoren und Datensätzen
function tSensorList.addData(pl: tPayLoad): integer;
var sen: tSensor;
    ds: tDataset;
  	ix: integer;
begin
  ix:= indexOf(pl.uid);
	if ix < 0 then begin //neuer Sensor
  	sen:= tSensor.create(pl.uid, pl.typ, pl.version, pl.revision);
    ix:= sensors.Add(sen);
  end;
  ds:= tDataset.Create();
  ds.TimeStamp:= now;

  ds.Batterie:= pl.Batterie;
  ds.RunTime:= pl.RunTime;
  ds.cntTxShot:= pl.cntTxShot;

  ds.Temperature:= pl.Temperature;
  ds.StatusTemp:= pl.StatusTemp;
  ds.ResolutionTemp:= pl.ResolutionTemp;

  ds.Level:= pl.Level;
  ds.StatusLevel:= pl.StatusLevel;

  get(ix).addDataSet(ds);	//Datensatz zur Liste des Sensors hinzufügen
  result:= ix;  					//Index des Sensors

end;

end.

