unit typedef;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DataFile, Graphics;

const
  SEK = 1000000;  //µs
  DAY = 24*60*60; //sek je Tag


type
  tCRC = packed record
    case integer of
    0: (val: word);
    1: (data: array[0..1] of byte);
  end;

type
 EMyException = class(Exception);

  //Datenpaket STLPU -> STLPR
  //sychron halten mit iog_sensors.h
  tRxPayload = packed record
    case integer of
    0: (
		val8X,        //+0
		mac6,
		fMeasure,
		fTemperature,
		version,
		revision,
		rstReason,
		temp_res,
		val8A,
		val8B,
		val8C,
		val8D,
		val8E,
		val8F,
		val8G,
		val8H  : byte;

		u3p3,							//+16
		temperature,
		cnt_TxShoot,
		sensors,
		val16B,
		val16C,
		val16D,
		val16E  : word;

		devTime,					//+32
		devOnTime,
		lastResponse,
		mainValue,
		val32B,
		val32C,
		val32D,
		val32E,
		val32F,
		val32G,
		val32H,
		val32I,
		val32J,
		val32K,
		val32L,
		val32M,
		val32N,
		val32O,
		val32P,
		val32Q,
		val32R,
		val32S,
		val32T   : dword;

		devUID,					//+124
		crc16: word;		//+126
    );

    1: (
    data: array[0..127] of byte;
    );
  end;

  //Daten STLPR -> STLPU   (RPi -> esp-gw)
	tTxPayload = packed record
    case integer of
    0: (
		val8X,			//+0
		wifi_channel,
		mac6,
		fast_motion,
		temp_res,
		val8A,
		val8B,
		val8C,
		val8D,
		val8E,
		val8F,
		val8G,
		val8H,
		val8I,
		val8J,
		val8K: byte;

		dsTime,		//+16
		val32A,
		val32B,
		val32C,
		val32D,
		val32E,
		val32F,
		val32G,
		val32H,
		val32I,
		val32J: dword;

		devUID,        //+60
		val16A: word;  //+62
    );
    1: (
    data: array[0..63] of byte;
    );
  end;

  //Datensatz zum Speichern
 tPayload = packed record
  uid: word;
  sensors: word;	//Bitmaske fühlertypen im Gerät 
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

  Timestamp : tDateTime;
end;


//Farben der Web-Widgets
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

const
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

var dsgn: TDesign;

type
confSensor = record
  name: string;
  interval: integer;
  tempres: integer;
end;

type
enumPaths = (pathConfig, pathData, pathLog, pathIog);


procedure config_write_channel(ch: byte);   //Datafile
function  config_read_channel(): byte;
function  config_read_sensor(uid: word): confSensor;

function getDSFileName(date: tDateTime): string;
function getFilePath(toPath: enumPaths): string;
procedure writeLogMessage(log: string);

function crc16(s: AnsiString):word;



implementation

//Configdata Datafile
procedure config_write_channel(ch: byte);
var fData: tDatafile;
begin
	fData:= tDataFile.create(getFilePath(pathConfig) + 'iog_sensors.conf');
  fData.WriteInteger('Wifi', 'Channel',ch);
  fData.Free;
end;


function  config_read_channel(): byte;
var fData: tDatafile;
begin
	fData:= tDataFile.create(getFilePath(pathConfig) + 'iog_sensors.conf');
  result:= fData.ReadInteger('Wifi', 'Channel', 7); //Default Channel 7
  fData.Free;
end;

//Configurationsdaten des Sensors - nur lesen
function  config_read_sensor(uid: word): confSensor;
var fData: tDatafile;
    s: string;
begin
  s:= IntToStr(uid);
	fData:= tDataFile.create(getFilePath(pathConfig) + 'iog_sensors.conf');
  result.name := fData.ReadString(s, 'name', 'unknow');
  result.interval := fData.ReadInteger(s, 'interval', 1800); //x Sek
  result.tempres := fData.ReadInteger(s, 'tempres', 9);
  fData.Free;
end;

//------------------------------------------------------------------------------

//Name der Datensatzdatei YYYY-mm-dd.iog
function getDSFileName(date: tDateTime): string;
begin
  result:= FormatDateTime('YYYY-mm-dd', date) + '.iog'
end;

//Datensätze im HomeDir
function getFilePath(toPath: enumPaths): string;
begin
  case toPath of
  	pathData: 	result:= GetUserDir() + '.iog/data/';
    pathConfig: result:= GetUserDir() + '.iog/config/';
    pathLog:		result:= GetUserDir() + '.iog/log/';
    pathIog:		result:= GetUserDir() + '.iog/'
  end;
end;

//Log-Datei schreiben
procedure writeLogMessage(log: string);
var lf: textfile;	//Logfile
		fn: string;
begin
  fn:= getFilePath(pathLog) + 'iog_msg.log';
 	AssignFile(lf, fn);
	try
		if FileExists(fn) then append(lf)	else rewrite(lf);
    writeln(lf, log);
	finally
		CloseFile(lf);
	end;
  writeln(log+#13); 					//Terminalausgabe
end;


//Adaption der CRC16-Function aus esp-source
function crc16(s: AnsiString):word;
const POLY = $8408;
var i, j, len: integer;
    crc, data: word;

begin
	crc:= $ffff;
  len:= length(s);
	if (len > 0) then begin

		for j:= 1 to len do begin
      data:= $ff and byte(s[j]);
			for i:= 0 to 7 do begin
        if ((crc and $0001) xor (data and $0001)) <> 0
        	then crc:= (crc shr 1) xor POLY
					else crc:= crc shr 1;
        data:= data shr 1;
			end;

		end;
  end;
	crc:= not(crc);
	data:= crc;
	result:= (crc shl 8) or (data shr 8 and $ff);
end;

end.

