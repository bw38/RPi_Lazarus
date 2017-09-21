unit typedef;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  //Datenpaket vom STL_PU
  //sychron halten mit iog_sensors.h
  trxPayload = packed record
    case integer of
    0: (
    devID,
		devCnt,
		fMeasure,
		fTemperature,
		cnt_ChanScan,
		cnt_TxShoot,
		rstReason,
		val8A: byte;

		u3p3,									//+8
		temperature,
		val16B,
		val16C : word;

		devTime,							//+16
		devOnTime,
		val32A,
		val32B,
		val32C,
		val32D,
		val32E,
		val32F : dword;

		lastResponse,
		val32V,
		val32W: dword;

		devUID,		//+60
		crc16: word;		//+62
    );

    1: (
    data: array[0..63] of byte;
    );
  end;


  tCRC = packed record
    case integer of
    0: (val: word);
    1: (data: array[0..1] of byte);
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

