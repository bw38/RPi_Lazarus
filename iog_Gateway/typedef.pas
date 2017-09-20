unit typedef;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  SEK = 1000000;

type

  tCRC = packed record
    case integer of
    0: (val: word);
    1: (data: array[0..1] of byte);
  end;

  //Datenpaket STLPU -> STLPR
  //sychron halten mit iog_sensors.h
  tRxPayload = packed record
    case integer of
    0: (
    devTyp,        //+0
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
		val16A,
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
		devTyp,			//+0
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


function crc16(s: AnsiString):word;

implementation





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

