program ioggw;

{$mode delphi}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads, cmem,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  crt, piSerial, typedef, data;

  { tIogGateway }

type
  tIogGateway = class(TCustomApplication)
  protected
    rs232: TRS232;
    rxLine: AnsiString;
    rxPL: tRxPayload;
    txPL: tTxPayload;

    procedure OpenRS232();
    procedure RxData(s: AnsiString);
    procedure sendFrame(pl: array of byte; typ: char);
    procedure terminatedRS232(Sender: tObject);

    procedure DoRun; override;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;


procedure tIogGateway.OpenRS232();
var res: integer;
begin
  rs232:= TRS232.create();   					 //UART0 - 115200bd 8N1
  if assigned(rs232) then begin

    rs232.Device := '/dev/ttyAMA0';  // [UART0] - Bluetooth on Pi3
    rs232.Baudrate := B921600;       // getestet mit B921600
		rs232.FlowControl := fcNone;

    rs232.onDataRcvd:= RxData;   		 // Rx-event
    rs232.OnTerminate := terminatedRS232;
  	res:= rs232.Open();
    writeln('Open Rs232, fd: ' + IntToStr(res));

    rs232.SetRTS(false);  //release esp-reset
    rs232.useSynchronize := false; //wg console-mode

  end else writeln('Error on open Serial-Port');
end;

//Rx-Thread closed
procedure tIogGateway.terminatedRS232(Sender: tObject);
begin
  //...
  rs232:= nil;
end;

//Data received
procedure tIogGateway.RxData(s: AnsiString);
var	i, n, sz: integer;
    rxCRC: tCRC;

begin
  for i:= 1 to length(s) do begin
    rxline:= rxline + s[i];		//zeichenweise Bearbeitung
    if s[i] = #10 then begin  //Frameabschluss mit \n
      sz:= sizeof(rxPL.data);

      //gültiger A-Frame ?  73 Bytes (incl CRC und nl)
      n:=  length(rxline);
      if (n >= sz + 9) and	//Databytes + Overhead
  			 (rxLine[n - (sz+8)] = '$') and      //Fix - daten prüfen
         (rxLine[n - (sz+7)] = 'J') and
         (rxLine[n - (sz+6)] = 'R') and
         (rxLine[n - (sz+5)] = '*') and
         (rxLine[n - (sz+4)] = 'A') and
         (rxLine[n- 3] = 'A')
      then begin

        move (rxLine[n-2], rxCRC.data, 2);
        if rxCRC.val = crc16(copy(rxline, n - (sz+8), sz+6)) then begin
          //Datenpaket des esp8266 aus Frame heraukopieren
      		move (rxLine[n - (sz+3)], rxPL.data, sz);

          FillChar(txPL, sizeof(txPL), 0);	//Sendepuffer löschen

          //Sensortyp ermitteln
          if (rxPL.devTyp = 1) then begin //Levelsensor
            //anpassen !!!!!!
            txPL.devUID:= rxPL.devUID; //Sensor UID
            txPL.mac6 := rxPL.mac6;
            txPL.dsTime:= dword(180  * SEK);
            txPL.fast_motion := 0;
            txPL.temp_res := 10;

            sendFrame(txPL.data, 'A');
          end else
          if (rxPL.devTyp = 2) then begin //Temperatursensor

            txPL.devUID:= rxPL.devUID; //Sensor UID
            txPL.mac6 := rxPL.mac6;
            txPL.dsTime:= dword(180  * SEK);
            txPL.fast_motion := 30;
            txPL.temp_res := 10;

            sendFrame(txPL.data, 'A');
          end;
          	s:= 'UID:      ' + IntToStr(rxPL.devUID) + #13;
            writeln(S);

          	s:= 'Level:    ' + IntToStr(rxPL.mainValue) + #13;
            writeln(s);
            s:= 'Temp:     ' + FloatToStr(rxPL.temperature / 16.0) + ' : '
            								 + IntToStr(rxPL.fTemperature)  + #13;
          	writeln(s);
          	s:= 'Batterie: ' + FloatToStr(rxPL.u3p3 / 1024) + #13;
          	writeln(s);
            s:= 'Response: ' + IntToStr(rxPL.lastResponse)+' µs' + #13;
            writeln(s);
            s:= 'TxShoot:  ' + IntToStr(rxpl.cnt_TxShoot) + #13;
            writeln(s);
            s:= 'DevTime:  ' + IntToStr(rxpl.devTime div 1000) + ' ms' + #13;
            writeln(s);
            s:= 'Version:  ' + IntToStr(rxpl.version) + '.' + IntToStr(rxpl.revision) + #13;
            writeln(s);
            writeln ('************************' + #13);



        end else begin
 						 //CRC-Fehler -> Meldung zurückschicken
        end;
			  if n>sz+9 then begin //ggf log-messages ausgeben
        	s:= copy(rxLine, 1, n - (sz+9));
          writeln(s);
        end;


        rxline:= '';
      end;

    end;  // if #10
  end;    //for
end;


procedure tIogGateway.sendFrame(pl: array of byte; typ: char);
var s: string;
    crc : tCRC;
begin
	setlength(s, sizeof(pl));
	move(pl[0], s[1], length(s));
  s:= '$RJ*' + typ + s + typ;
  crc.val := crc16(s);
  s:= s + char(crc.data[0]) + char(crc.data[1]) + #10;
  crc.val := length(s);
  rs232.WriteData(s);
end;

//----------------------------------------------------------------------------


procedure tIogGateway.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg := CheckOptions('h', 'help');
  if ErrorMsg <> '' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;



  if UpperCase(ReadKey) = 'Q' then Terminate;
end;

constructor tIogGateway.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;

  OpenRS232();
end;




destructor tIogGateway.Destroy;
begin
  rs232.Destroy;
  inherited Destroy;
end;

procedure tIogGateway.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: tIogGateway;
begin
  Application := tIogGateway.Create(nil);
  Application.Run;
  Application.Free;
end.

