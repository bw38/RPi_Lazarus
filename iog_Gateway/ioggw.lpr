program ioggw;

{$mode delphi}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads, cmem,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  crt, piSerial, typedef, sensor, debug, DataFile, lNet;

  { tIogGateway }

type
  tIogGateway = class(TCustomApplication)
  protected
    rs232: TRS232;
    rxLine: AnsiString;
    rxPL: tRxPayload;
    txPL: tTxPayload;
    lSensors: TSensorList;	//Wurzel der Datensätze des aktuellen Tages
    tcpRxLine: string;

    procedure OpenRS232();
    procedure RxData(s: AnsiString);
    procedure sendFrame(pl: array of byte; typ: char);
    procedure terminatedRS232(Sender: tObject);

    procedure OpenTCP();

    procedure SetWifiChannel(ch: byte);

    procedure DoRun; override;

  private
    fcUART: byte; //Flowcontrol Rx-Uart
    cFTyp: char;
    lenPL, cnt: byte;
    cix: byte;
    rxCRC: tCRC;

    srv1: TLTCP;

    procedure OnSrvError(const msg: string; aSocket: TLSocket);
    procedure OnSrvAccept(aSocket: TLSocket);
    procedure OnSrvReceive(aSocket: TLSocket);
    procedure OnSrvDisconnect(aSocket: TLSocket);

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
    //rs232.Baudrate := B115200;
		rs232.FlowControl := fcNone;

    rs232.onDataRcvd:= RxData;   		 // Rx-event
    rs232.OnTerminate := terminatedRS232;
  	res:= rs232.Open();
    writeln('Open Rs232, fd: ' + IntToStr(res));
    if res < 0 then Terminate;
    rs232.SetRTS(false);  //release esp-reset
    rs232.useSynchronize := false; //wg console-mode

  end else begin
    writeln('Error on open Serial-Port');
    Terminate;
  end;

end;

//Rx-Thread closed
procedure tIogGateway.terminatedRS232(Sender: tObject);
begin
  //...
  rs232:= nil;
end;

//Data received via UART v. ESP8266
procedure tIogGateway.RxData(s: AnsiString);
var	i, ix: integer;
    pl: tPayLoad;
    ds: dword;
    tres, chWifi: byte;
    sh: AnsiString;

begin
  for i:= 1 to length(s) do begin
    rxline:= rxline + s[i];		//zeichenweise Bearbeitung
   // if s[i] in [' ' .. #127] then write(s[i]);
      //FlowControl UART-Rx-Frame
			// $ J R * [cFTyp][lenPL][Payload][cFTyp][lCRC](hCRC][\n]
			// 0 1 2 3    4      5       6       7      8    9    10
      case fcUART of
      	0: if s[i] = '$' then begin
           	inc(fcUART);
           end;
        1: if s[i] = 'J' then inc(fcUART) else fcUART:= 0;
        2: if s[i] = 'R' then inc(fcUART) else fcUART:= 0;
        3: if s[i] = '*' then begin
             inc(fcUART);
             rxline:= '$JR*';
           end else begin
             fcUART:= 0;
             rxLine:= '';
           end;
        4: begin cFTyp:= s[i]; inc(fcUART); end;
        5: begin lenPL:= byte(s[i]); inc(fcUART); if lenPL = 0 then inc(fcUART); cnt:= 0; end;
        6: begin inc(cnt); if cnt >= lenPL then inc(fcUART); end;
        7: if s[i] = cFTyp then inc(fcUART) else fcUART:= 0;
        8: begin rxCRC.data[0]:= byte(s[i]); inc(fcUART); end;
        9: begin rxCRC.data[1]:= byte(s[i]); inc(fcUART); end;
       10: begin
       			if ((s[i] = #10) and (rxCRC.val = crc16(copy(rxLine, 1, lenPL+7)))) then begin
              //Auswertung Frametyp
              if cFTyp = 'A' then begin
   	  		 			//Datenpaket des esp8266 aus Frame heraukopieren
    						move (rxLine[7], rxPL.data, lenPL);
			      		FillChar(txPL, sizeof(txPL), 0);	//Sendepuffer löschen

      					//Datensatz erstellen u. zur Liste hinzufügen
      					pl.uid := 				rxPL.devUID;
      					pl.typ := 				rxPL.devTyp;
      					pl.version := 		rxPL.version;
      					pl.revision := 		rxPL.revision;

      					pl.Batterie:= 		round(rxPL.u3p3 / 1.024);
  							pl.RunTime:=			rxPL.devOnTime;
  							pl.cntTxShot:= 		rxPL.cnt_TxShoot;

            		pl.Temperature:=	rxPL.temperature;
  							pl.StatusTemp:= 	rxPL.fTemperature;
								pl.ResolutionTemp:= rxPL.temp_res;

  							pl.Level:= 				rxPL.mainValue;
  							pl.StatusLevel:=	rxPL.fMeasure;

      					pl.Timestamp := now;

      					ix:= lSensors.addData(pl);       	//ix des Listeneintrages
      					ds:= lSensors.get(ix).DeepSleep; 	//nächste Schlafdauer in µs
      					tres:= lSensors.get(ix).TempRes; 	//f. nächste Temperaturmessung
            		chWifi:= lSensors.get(ix).Channel; //Wifi-Kanal f. nächste Kommunkation
                lSensors.get(ix).TimeStamp:= now;

      					//Gateway Response - "A"-Frame
      					txPL.devUID:= rxPL.devUID; //Sensor UID
      					txPL.mac6 := rxPL.mac6;
      					txPL.dsTime:= ds;
      					txPL.fast_motion := 0;
      					txPL.temp_res := tres;
            		txPL.wifi_channel := chWifi;

      					sendFrame(txPL.data, 'A');

      					writeDS2cmdline(pl);
            		lSensors.saveData(pl);
        			end; // if cFTyp = 'A'


       				if cFTyp = 'C' then begin
            		SetWifiChannel(lSensors.MasterChannel); //Abfrage Kanal, ohne Payload
            		//aktuellen Channel sofort zurücksenden
              end;

              if cFTyp = 'W' then begin
                sh:= FormatDateTime(
                	'YYYY-MM-dd hh:nn:ss.zzz', now) + ' - Warn:  ' + pchar(@rxLine[7]);
								writeLogMessage(sh);  //Logfile
              end;

              if cFTyp = 'E' then begin
                sh:= FormatDateTime(
                	'YYYY-MM-dd hh:nn:ss.zzz', now) + ' - Error: ' + pchar(@rxLine[7]);
								writeLogMessage(sh);  //Logfile
              end;

              if cFTyp = 'L' then begin
                sh:= FormatDateTime(
                	'YYYY-MM-dd hh:nn:ss.zzz', now) + ' - Log:   ' + pchar(@rxLine[7]);
								writeLogMessage(sh);  //Logfile
              end;

						end;  	//if nl and crc ok
           	rxline:= '';
           	fcUART:= 0;
					end; //fcUART = 10


      	end;  //case
  end;    //for
end;


procedure tIogGateway.sendFrame(pl: array of byte; typ: char);
var s: string;
    crc : tCRC;
begin
	setlength(s, sizeof(pl));
	move(pl[0], s[1], length(s));
  s:= '$RJ*' + typ + char(sizeof(pl)) + s + typ;
  crc.val := crc16(s);
  s:= s + char(crc.data[0]) + char(crc.data[1]) + #10;
  crc.val := length(s);
  rs232.WriteData(s);
end;


//"C"-Frame an Gateway zum Kanalwechsel
procedure tIoGGateway.SetWifiChannel(ch: byte);
begin
	sendFrame(ch, 'C');
end;

//----------------------------------------------------------------------------
//TCP-Server

procedure tIoGGateway.OpenTCP();
begin
  tcpRxLine:= '';
  srv1:= TLTCP.create(nil);
  srv1.OnError := OnSrvError;     // assign all callbacks
  srv1.OnReceive := OnSrvReceive;
  srv1.OnDisconnect := OnSrvDisconnect;
  srv1.OnAccept := OnSrvAccept;
  srv1.Timeout := 100; // responsive enough, but won't hog cpu
  srv1.ReuseAddress := True;

  if not srv1.Listen(38180, '0.0.0.0') then begin
    writeln('Error on Open TCP-Server');
    Terminate;
  end;
end;


procedure tIoGGateway.OnSrvError(const msg: string; aSocket: TLSocket);
begin
  Writeln(msg);  // if error occured, write it explicitly
end;

procedure tIoGGateway.OnSrvDisconnect(aSocket: TLSocket);
begin
  Writeln('Lost connection'); // write info if connection was lost
end;

procedure tIoGGateway.OnSrvAccept(aSocket: TLSocket);
begin
  Writeln('Connection accepted from ', aSocket.PeerAddress); // on accept, write whom we accepted
end;


procedure tIoGGateway.OnSrvReceive(aSocket: TLSocket);
var
  msg: AnsiString;
  i, uid, sz: Integer;
  ar2: array of byte;
  sl: tStringList;
  sen: tSensor;

begin
  aSocket.GetMessage(msg);
  for i:= 1 to length(msg) do begin
    if msg[i] <> #10
      then tcpRxLine:= tcpRxLine + msg[i]
      else begin
      	sl:= tStringList.Create;
        sl.Delimiter := ' ';
        sl.DelimitedText := tcpRxLine;
        tcpRxLine:= '';
    		//Auswertung
        try
      		if sl[0] = '§WDG' then begin //Widget zeichnen, 1- Sensornummer, 2- Höhe in pixeln
          	if sl.Count <> 3 then raise EMyException.create('Error extern TCP-Command');
            uid:= StrToInt(sl[1]);
            sz:= StrToInt(sl[2]);
            sen:= lSensors.getByUID(uid);
            sen.DrawWidget(sz);
            sz:= sen.spng.Size;
            setlength(ar2, sz);
            sen.spng.Position := 0;
            sen.spng.Read(ar2[0], sz);
  					aSocket.Send(ar2[0], sz);
  					aSocket.Disconnect();
            setlength(ar2, 0);
        	end;

        finally
    			sl.Free;
        end;
  		end; //if
  end; //for
end;




//----------------------------------------------------------------------------


procedure tIogGateway.DoRun;
var
  ErrorMsg: String;
  s: string;
  chan: byte;
  c: char;
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

  //MainLoop
  while not Terminated do begin
      srv1.CallAction();
  	CheckSynchronize();  //Timer-Events (u.a.?)
    sleep(20);
    if keypressed then begin  //Tastatur nonblocking abfragen
      c:= (ReadKey);
    	if c = 'Q' then Terminate;
    	//Manuelle Kanalumschaltung
    	if c = 'C' then begin
        write('Neuer Wifi-Kanal: ');
        ReadLn(s);
        try
          chan:= StrToInt(s);
          if not (chan in [1..13])
	         	then raise EMyException.create('test');
          lSensors.MasterChannel:= chan;
        except
          Writeln('Eingabefehler !!!')
        end;

      end;
    end;
  end;


end;

//-----------------------------------------------------------------------------

constructor tIogGateway.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:= True;
  OpenRS232();
  lSensors:= tSensorList.create(); //Datenhandling
  lSensors.onSetMasterChannel:= SetWifiChannel; //Callback-Procedure
  lSensors.MasterChannel:= config_read_channel();
  lSensors.loadData(now);	//Daten des aktuellen Tages laden


  OpenTCP();

  rxLine:= '';
  dsgn:= dsgn_Dark;
end;


destructor tIogGateway.Destroy;
begin
  lSensors.Free;
  srv1.Free;
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

