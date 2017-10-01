unit sensor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typedef, debug, fptimer, math;

type

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
  TempRes: byte;		//Auflösung der nächsten Temperaturmessung 9..12

  Channel: byte;

  //Variablen f. intelligenten Kanalwechsel
  TimeStamp : tDateTime; 				//Zeitpunkt des letzten Telegramms


  constructor create(uid_, typ_: word; ver_, rev_: byte);
  destructor destroy;  override;
  function get(ix: integer): tDataSet;
  function getlast: tDataset;
  function addDataSet(ds: tDataSet): integer;
end;

//Callback type
TChangeChannel = procedure(ch: byte) of object;

tSensorList = class
private
	sensors: tList;
  FMasterChannel: byte;
  FOnChangeChannel: TChangeChannel;

  procedure FSetMasterChannel(ch: byte);
public

  property MasterChannel: byte read FMasterChannel write FSetMasterChannel;
  property onSetMasterChannel: TChangeChannel read FOnChangeChannel write FOnChangeChannel;

  constructor create();
  destructor destroy;  override;

  function get(ix: integer): tSensor;
  function indexOf(uid: word): integer;
  function addData(pl: tPayLoad): integer;

  function saveData(pl: tPayLoad): boolean;
  function loadData(date: tDateTime): boolean;


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
  TempRes:= 9;
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
  //Anzahel der Datensätzen je Sensor im Speicher begrenzen
  if data.Count >= 1000 then data.Delete(0);
  result:= data.Add(ds);
end;

//-----------------------------------------------------------------------------

constructor tSensorList.create();
begin
  inherited create;
  sensors:= tList.Create;
  FOnChangeChannel:= nil;
end;

destructor tSensorList.destroy;
var i: integer;
begin
  for i:= 0 to sensors.Count-1 do get(i).Free;
  inherited destroy;
end;


procedure tSensorList.FSetMasterChannel(ch: byte);
begin
  if ch in [1..13] then begin
  	FMasterChannel:= ch;
    config_write_channel(MasterChannel);
  	//ESP via C-Frame steuern
  	if assigned(FOnChangeChannel) then FOnChangeChannel(ch);
  end;
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
    sen.Channel := MasterChannel;
    ix:= sensors.Add(sen);
  end;
  ds:= tDataset.Create();
  ds.TimeStamp:= pl.Timestamp;

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



//Datensatz speichern
function tSensorList.saveData(pl: tPayLoad): boolean;
var stream: tFileStream;
    sdir, fn: string;
begin
  result:= false;
  sdir:= getFilePath();
  fn:= getDSFileName(now);  //File mit aktuellem Datum
  try
  	if not DirectoryExists(sdir)
  		then CreateDir(sdir);
 	 if FileExists(sdir + fn)
  	  then stream := TFileStream.Create(sdir + fn, fmOpenWrite)
    	else stream := TFileStream.Create(sdir + fn, fmCreate);

  	stream.Position := stream.Size;
  	stream.WriteBuffer(pl, sizeof(pl));
    result:= true;
  finally
  	if assigned(stream) then stream.Free;
  end;
end;

function tSensorList.loadData(date: tDateTime): boolean;
var sdir, fn: string;
    stream: tFileStream;
    pl: tPayload;
begin
  result:= false;
  stream:= nil;
	sdir:= getFilePath();
  fn:= getDSFileName(date);
  //pl initialisieren !!!!!!!!!!!!!!!!!!!!!
  try
  	if FileExists(sdir+fn) then begin
    	stream := TFileStream.Create(sdir + fn, fmOpenRead);
      while stream.Position < stream.Size do begin
      	stream.ReadBuffer(pl, sizeof(pl));
        addData(pl);
        // writeDS2cmdLine(pl);
      end;
      result:= true;
    end;
  finally
  	if assigned(stream) then stream.Free;
  end;
end;


end.

