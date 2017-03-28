unit main;

{******************************************************************************
Demo for using LazSerial on Raspberry for USB-Serial-Adapter / (UART0 /UART1)
Restrictions for internal UART0/1
 - Flowcontrol doesn't work
 - Configuration P1.8 / P1.10 before program starts

Joerg DL7VMD
*******************************************************************************}


{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, LazSerial, synaser;



type

  { TForm1 }

  TForm1 = class(TForm)
    btSetup: TSpeedButton;
    Edit1: TEdit;
    Memo1: TMemo;
    btTxLine: TSpeedButton;
    btSetRTS: TSpeedButton;
    procedure btSetRTSClick(Sender: TObject);
    procedure btSetupClick(Sender: TObject);
    procedure btTxLineClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
		serial: TLazSerial;
    rxline: string;

    procedure SerialRxData(Sender: TObject);
    procedure StatusChanged(Sender: TObject; Reason: THookSerialReason; const Value: string);
    procedure OpenDevice();
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }


procedure TForm1.FormCreate(Sender: TObject);
begin
  serial:= TLazSerial.Create(Self);

  //USB-Serial-Adapter 115200 8N1
  serial.Device := '/dev/ttyUSB0';

  serial.BaudRate := br115200;    //typedef s. lazserial.pas
  serial.DataBits := db8bits;
  serial.Parity := pNone;
  serial.StopBits := sbOne;
  //serial.FlowControl := fcNone;
  serial.FlowControl := fcHardware;
  serial.OnRxData := SerialRxData;
  serial.OnStatus := StatusChanged;
  serial.RcvLineCRLF := false;   //true -> wait Rx for crlf and cut it

  OpenDevice();
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
	serial.Free;
end;

procedure TForm1.OpenDevice();
begin
  try
  	serial.Open;
  except
    Showmessage('Could not open: ' + serial.Device);
  end;
end;

procedure TForm1.SerialRxData(Sender: TObject);
var	i: integer;
  	s: string;
begin
  if serial.DataAvailable then begin
    s:= serial.ReadData;
    for i:= 1 to length(s) do begin
      if s[i] = #10 then begin
        Memo1.Lines.Add(rxline);
        rxline:= '';
      end else begin
      	rxline:= rxline + s[i] ;
      end;
    end;
  end;
end;

procedure TForm1.StatusChanged(Sender: TObject; Reason: THookSerialReason; const Value: string);
begin
  case Reason of    //def.in synaser.pas
    HR_SerialClose: btTxLine.Enabled := false;
  	HR_Connect: 		btTxLine.Enabled := true;
    HR_CanRead:     begin end;
    HR_CanWrite:    begin end;
    HR_ReadCount:   begin end;
    HR_WriteCount:  begin end;
    HR_Wait:        begin end;
  end;
end;

procedure TForm1.btSetupClick(Sender: TObject);
begin
  if serial.Active then serial.Close;
  serial.ShowSetupDialog;
  OpenDevice();
end;


procedure TForm1.btTxLineClick(Sender: TObject);
begin
  serial.WriteData(edit1.Text + #13#10);
end;


procedure TForm1.btSetRTSClick(Sender: TObject);
begin
	serial.SetRTS(btSetRTS.Down);
end;


end.

