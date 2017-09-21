unit main;

{******************************************************************************
Demo for using own Lib rs232.pas on Raspberry for UART0 / UART1
===============================================================

Activate UART1: (/dev/serial0 or /dev/ttyS0
-------------------------------------------
sudo raspi-config -> enable serialport
sudo nano /boot/cmdline.txt
	=> Remove Entry "console=serial0,115200"



Joerg DL7VMD
*******************************************************************************}


{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, piSerial, typedef;



type

  { TForm1 }

  TForm1 = class(TForm)
    Edit1: TEdit;
    Memo1: TMemo;
    btTxLine: TSpeedButton;
    btSetRTS: TSpeedButton;
    SpeedButton1: TSpeedButton;
    procedure btSetRTSClick(Sender: TObject);
    procedure btTxLineClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
		rs232: TRS232;
    rxline: AnsiString;
    rxPL : tRxPayload;

    procedure OpenRS232();

    procedure RxData(s: AnsiString);
    procedure terminatedRS232(Sender: tObject);

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }


procedure TForm1.FormCreate(Sender: TObject);
begin

  OpenRS232();
end;

procedure TForm1.OpenRS232();
var res: integer;
begin
  rs232:= TRS232.create();   					 //UART0 - 115200bd 8N1
  if assigned(rs232) then begin

    rs232.Device := '/dev/ttyAMA0';  // [UART0] - Bluetooth on Pi3
    rs232.Baudrate := B230400;         // [B115200] Standard-Baudrates s. piRS232.pas
		rs232.FlowControl := fcNone;

    rs232.onDataRcvd:= RxData;   		 // Rx-event
    rs232.OnTerminate := terminatedRS232;
  	res:= rs232.Open();
    rs232.SetRTS(false);
    Memo1.Lines.Add('Open Rs232, fd: ' + IntToStr(res));

    btTxLine.Enabled := true;
  end else Memo1.Lines.Add('Error on open Serial-Port');
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
	FreeAndNil(rs232);
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
var sx: AnsiString;
  	crc: word;

begin
   sx:= '$JR*A';
   crc:= crc16(sx);
   sx:= '';
end;

// RS232 - Event --------------------------------------------------------------

//Data received
procedure TForm1.RxData(s: AnsiString);
var	i, n: integer;
    rxCRC: tCRC;

begin
  for i:= 1 to length(s) do begin
    rxline:= rxline + s[i];		//zeichenweise Bearbeitung
    if s[i] = #10 then begin  //Frameabschluss mit \n

      //gültiger A-Frame ?  73 Bytes (incl CRC und nl)
      n:=  length(rxline);
      if (n >= 73) and
  			 (rxLine[n-72] = '$') and      //Fix - daten prüfen
         (rxLine[n-71] = 'J') and
         (rxLine[n-70] = 'R') and
         (rxLine[n-69] = '*') and
         (rxLine[n-68] = 'A') and
         (rxLine[n- 3] = 'A')
      then begin
        move (rxLine[n-2], rxCRC.data, 2);
        if rxCRC.val = crc16(copy(rxline, n-72, 70)) then begin
          //Datenpaket des esp8266 aus Frame heraukopieren
      		move (rxLine[n-67], rxPL.data, 64);

          if (rxPL.devID = 1) then begin //Levelsensor

          end else
          if (rxPL.devID = 2) then begin //Temperatursensor

          end;

          memo1.Clear;
          s:= 'Temperature: ' +floattostr(rxPL.temperature / 16.0);
          memo1.Lines.Add(s);
          s:= 'Batterie: '+ floattostr(rxPL.u3p3 / 1024);
          memo1.Lines.Add(s);

        end else begin
 						 //CRC-Fehler -> Meldung zurückschicken
        end;
			  if n>73 then begin //ggf log-messages speichern
        	s:= copy(rxLine, 1, n-73)
        end;


        rxline:= '';
      end;
      	Memo1.Lines.Add(rxline);

    end;  // if #10
  end;    //for
end;


//Rx-Thread closed
procedure TForm1.terminatedRS232(Sender: tObject);
begin
  Memo1.Lines.Add('Terminated');
  //...
  rs232:= nil;
end;

//------------------------------------------------------------------------------

procedure TForm1.btTxLineClick(Sender: TObject);
begin
  rs232.WriteData(edit1.Text + #13#10);
  edit1.Text := 'Test: ' + TimeToStr(now);
end;


procedure TForm1.btSetRTSClick(Sender: TObject);
begin
	rs232.SetRTS(btSetRTS.Down);
end;


end.


