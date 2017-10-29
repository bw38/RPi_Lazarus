unit sensor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typedef, debug, fptimer, math, Graphics, Types, drawweb,
  FPImage, FPCanvas, FPWritePNG, ftfont, lNet;

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

//Listeneintrag zur Pegelconvertierung
tLevelconvline = class
	fcnt, pLevel, hLevel: word;
end;

type
tSensor = class
private
  data: tList;
  levelconv: tList;		//f. Levelsensor, Convertierung Zählwert => Level in Prozent
  dwdg: TDrawWidget;	//zur graphischen Darstellung
  FSensorName: string;

  function convLevel(fcnt: word): word;
  function GetLevel(): integer;
  function GetMinLevel(): integer;
  function GetMaxLevel(): integer;
  function GetTemperature(): real;
  function GetMinTemperature(): real;
  function GetMaxTemperature(): real;
  function GetInterval(): word;
  function GetTimeStamp: TDateTime;
  function GetUBatt: real;
  function GetOnTime(): integer;
  function GetVersion(): string;
  function GetCntTxShot(): integer;
  function GetErrTxShot(): integer;

public
  uid: word;			//Informationen des Sensors
  sensors: word;	//Bitmap Fühlertypen
  version, revision: byte;

  DeepSleep: dword;	//Schlafzeit nach nächstem Telegramm
  TempRes: byte;		//Auflösung der nächsten Temperaturmessung 9..12

  Channel: byte;

  //Variablen f. intelligenten Kanalwechsel
  TimeStamp : tDateTime; 				//Zeitpunkt des letzten Telegramms



  constructor create(uid_, sensors_: word);
  destructor destroy;  override;
  function get(ix: integer): tDataSet;
  function getlast: tDataset;
  function addDataSet(ds: tDataSet): integer;

  property Level: integer read GetLevel;
  property minLevel: integer read GetMinLevel;
  property maxLevel: integer read GetMaxLevel;

  property Temp: real read GetTemperature;
  property minTemp: real read GetMinTemperature;
  property maxTemp: real read GetMaxTemperature;

  property idTxt: string read FSensorName write FSensorName;
  property Interval: word read GetInterval;

  property sensortime: TDateTime read GetTimestamp;
	property ubatt: real read GetUBatt;

  property UpTime: integer read GetOnTime;        //Wachzeitzeit in sek
  property strVersion: string read GetVersion;    //Version.Revision
  property cntTxShot: integer read GetCntTxShot;  // aktuelle Sendeversuche
  property errTxShot: integer read GetErrTxShot;	//Summe Fehlversuche

  procedure DrawWidget(hSize: integer; aSocket: TLSocket); //Web-Widget zeichnen
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
  function getByUID(uid: word): tSensor;
  function indexOf(uid: word): integer;
  function addData(pl: tPayLoad): integer;

  function saveData(pl: tPayLoad): boolean;
  function loadData(date: tDateTime): boolean;


end;

implementation


//------------------------------------------------------------------------------

constructor tSensor.create(uid_, sensors_: word);
var fn: string;
    sl, slh: tStringList;
    i: integer;
    lcl: tLevelConvLine;
    conf: confSensor;

begin
  inherited create;
  data:= tList.Create;
  uid:= uid_;
  sensors:= sensors_;

  //Sensor-Configuration aus Datei lesen
  conf:= config_read_sensor(uid);
  DeepSleep:= conf.interval * SEK;
  TempRes:= conf.tempres;
  FSensorName:= conf.name;

  levelconv:= tList.Create;
  if sensors and 2 = 2 then begin	//Levelsensor
  	//Umrechnungstabelle laden  /home/pi/.iog/sensors/xxxxx.level
    //Tabellen werden je Levelsensor manuell erstellt
    fn:= getFilePath(pathConfig) +  IntToStr(uid)+'.level';
    if FileExists(fn) then begin
    	sl:= tStringList.Create;
      slh:= tStringList.Create;
      slh.Delimiter := #9;	//Trennzeichen Tabulator
      sl.LoadFromFile(fn); 	//Zählwert - Füllstand in Prozent - Füllstand in cm
      try
      	for i:= 0 to sl.Count-1 do begin
      		slh.Clear;
	        slh.DelimitedText := sl[i];
          if slh.Count = 0 then begin end //Leerzeile, keine Aktion
          else if pos('#', TrimLeft(slh[0])) = 1 then begin end //Kommentarzeile
          else if slh.Count = 3 then begin //jede Zeile genau 3 numerische werte
            try
           		lcl:= tLevelConvLine.Create;
            	lcl.fcnt   := StrToInt(slh[0]);
            	lcl.pLevel := StrToInt(slh[1]);
            	lcl.hLevel := StrToInt(slh[2]);
            	levelconv.Add(lcl);
            except
             	lcl.free;
              WriteLogMessage('Fehler in ' + fn);
            end;
          end else begin	//Formaltfehler in derr Konvertierungdtabelle
    	     	WriteLogMessage('Fehler in ' + fn);
      	   	break;
          end;
      	end;
       except
       	levelconv.Clear;	//im Fehlerfall Liste löschen
        WriteLogMessage('Fehler in ' + fn);
       end;
      slh.Free;
      sl.Free;
    end;
  end;
end;

destructor tSensor.destroy;
var i: integer;
begin
  for i:= 0 to levelconv.Count -1 do tLevelConvLine(levelconv[i]).Free;
  levelconv.Free;
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
  result:= nil;
  if data.Count > 0 then result:= tDataSet(data[data.Count-1]);
end;

function tSensor.addDataSet(ds: tDataSet): integer;
begin
	if assigned(getLast) then
  	//Datensätze ab 0:00 Uhr im Speicher
  	if frac(getLast.TimeStamp) > frac(now) then data.Clear;
  result:= data.Add(ds);
end;

//Füllstand in Prozent, Umrechnung mit Convertierungsliste des sensors
function tSensor.convLevel(fcnt: word): word;
var m, diff: word;
    line: tLevelConvLine;
    i, res: integer;
begin
	res:= -1;
	diff:= $FFFF;
  i:= levelconv.Count;
  for i:= 0 to levelconv.Count-1 do begin
  	line:= tLevelConvLine(levelconv[i]);
    m:= abs(fcnt - line.fcnt);
    if m < diff then begin
    	diff:= m;
      res:= line.pLevel;
    end;
  end;
  result:= res;
end;

function tSensor.GetLevel(): integer;
begin
	result:= convLevel(getLast.Level);
end;

function tSensor.GetMinLevel(): integer;
var i, x: integer;
begin
  x:= -1;	//max Zählwert
	for i:= 0 to data.Count-1 do x:= max(get(i).Level, x);
  result:= convLevel(x);
end;

function tSensor.GetMaxLevel(): integer;
var i, x: integer;
begin
  x:= maxint; //min Zählwert
	for i:= 0 to data.Count-1 do x:= min(get(i).Level, x);
  result:= convLevel(x);
end;

function tSensor.GetTemperature(): real;
begin
  result:= getLast.Temperature / 16;
end;

function tSensor.GetMinTemperature(): real;
var i, x: integer;
begin
  x:= maxint;
	for i:= 0 to data.Count-1 do x:= min(get(i).Temperature, x);
  result:= x / 16;
end;

function tSensor.GetMaxTemperature(): real;
var i, x: integer;
begin
  x:= -1000;
	for i:= 0 to data.Count-1 do x:= max(get(i).Temperature, x);
  result:= x / 16;
end;

//Ruhezeit sdes Sensors in Minuten
function tSensor.GetInterval(): word;
begin
  result:= round(DeepSleep / (60 * Sek));
end;

function tSensor.GetUBatt(): real;
begin
  result:= getLast.Batterie / 1000;
end;

function tSensor.GetOnTime(): integer;
begin
  result:= round(getLast.RunTime / 1000);
end;

function tSensor.GetVersion(): string;
begin
	result:= IntToStr(version) + '.' + IntToStr(revision);
end;

function tSensor.GetCntTxShot(): integer;
begin
  result:= getLast.cntTxShot;
end;

function tSensor.GetTimeStamp: TDateTime;
begin
  result:= getLast.TimeStamp;
end;

function tSensor.GetErrTxShot(): integer;
var i: integer;
begin
  result:= 0;
  for i:= 0 to data.Count-1 do result:= result + (get(i).cntTxShot -1);
end;

//Web-Widget zeichnen und im Memorystream speichern
procedure tSensor.DrawWidget(hSize: integer; aSocket: TLSocket);
begin
  dwdg:= TDrawWidget.Create(gtLevelMeter);
  dwdg.hSize:= hSize;
  dwdg.aSocket:= aSocket;
  //Messerte übergeben
  dwdg.dw_level:= level;
  dwdg.dw_minlevel:= minLevel;
  dwdg.dw_maxLevel:= maxLevel;
  dwdg.dw_temp:= temp;
  dwdg.dw_minTemp:= minTemp;
  dwdg.dw_maxTemp:= maxTemp;
  dwdg.dw_idTxt:= idTxt;
  dwdg.dw_interval:= interval;
  dwdg.dw_sensortime:= sensortime;
  dwdg.dw_ubatt:= ubatt;
  dwdg.dw_UpTime := UpTime;
  dwdg.dw_strVersion := strVersion;
  dwdg.dw_cntTxShot := cntTxShot;
  dwdg.dw_errTxShot := errTxShot;
  dwdg.Start;
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


function tSensorList.getByUID(uid: word): tSensor;
begin
  result:= self.get(self.indexof(uid));
end;

//Zentrale Funktion zum Hinzufügen von Sensoren und Datensätzen
function tSensorList.addData(pl: tPayLoad): integer;
var sen: tSensor;
    ds: tDataset;
  	ix: integer;
begin
  ix:= indexOf(pl.uid);
	if ix < 0 then begin //neuer Sensor
  	sen:= tSensor.create(pl.uid, pl.sensors);
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

  get(ix).version:= pl.version;
  get(ix).revision:= pl.revision;
  get(ix).addDataSet(ds);	//Datensatz zur Liste des Sensors hinzufügen
  result:= ix;  					//Index des Sensors

end;



//Datensatz speichern
function tSensorList.saveData(pl: tPayLoad): boolean;
var stream: tFileStream;
    sdir, fn: string;
begin
  result:= false;
  sdir:= getFilePath(pathData);
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
	sdir:= getFilePath(pathData);
  fn:= getDSFileName(date);

  FillChar(pl, sizeof(pl), #0); //pl initialisieren
  try
  	if FileExists(sdir+fn) then begin
    	stream := TFileStream.Create(sdir + fn, fmOpenRead);
      while stream.Position < stream.Size do begin
      	stream.ReadBuffer(pl, sizeof(pl));
        addData(pl);
        //writeDS2cmdLine(pl);
      end;
      result:= true;
    end;
  finally
  	if assigned(stream) then stream.Free;
  end;
end;


end.

