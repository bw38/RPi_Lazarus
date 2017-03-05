unit main;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, wiringpi, {LazSerial,} piRS232, DateUtils;

{ TForm1 }

type
  TForm1 = class(TForm)
    btCloseRS232: TSpeedButton;
    btSend: TButton;
    edRxLine: TEdit;
    edTxLine: TEdit;

    Memo1: TMemo;
    btRs232Open: TSpeedButton;
    btStopTx: TSpeedButton;
    btOpenRS232: TSpeedButton;
    btTxRs232: TSpeedButton;
    btSetRTS: TSpeedButton;
    btGetCTS: TSpeedButton;

    procedure btCloseRS232Click(Sender: TObject);
    procedure btGetCTSClick(Sender: TObject);
    procedure btSetRTSClick(Sender: TObject);
    procedure btOpenRS232Click(Sender: TObject);
    procedure btSendClick(Sender: TObject);
    procedure btStopTxClick(Sender: TObject);
    procedure btRs232OpenClick(Sender: TObject);
    procedure btTxRs232Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure SerialRxData(Sender: TObject);
  private
    err: longint;
    rxline: string;
//    Serial: TLazSerial;

    rs232: TRS232;  //serial class

    procedure RxData(line: ansistring);
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
  //Setup in WiringPi-Numbering
  wiringPiSetup();

  //Serielle Schnittstelle (LazSerail)
  //Serial:= tLazSerial.Create(Self);

end;


procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(rs232);


//	if Serial.Active then Serial.Close;
//  Serial.Free;
end;


//*************************************************************************

//serielle Schnittstelle initialisieren
procedure TForm1.btRs232OpenClick(Sender: TObject);
var fd: longint;
begin
 { rxline:= '';
  Serial.Device := '/dev/ttyS0';
  Serial.BaudRate := br115200;
  Serial.OnRxData := @SerialRxData;
  Serial.FlowControl := fcHardware;
  Serial.ShowSetupDialog;
  Serial.Open;
  }

end;


procedure TForm1.SerialRxData(Sender: TObject);
var s: string;
  	i: integer;
begin
{
  s:= Serial.ReadData;
  for i:= 1 to length(s) do begin
    if s[i] <> #10
    	then edRxLine.Text := edRxLine.Text + s[i]
    	else begin
  			Memo1.Lines.Add(edRxLine.Text);
        edRxLine.Text := '';
      end;
  end; }
end;


procedure TForm1.btSendClick(Sender: TObject);
begin   {
  if Serial.Active then begin
  	Serial.WriteData(edTxLine.Text + #10);
  end else
  	Memo1.Lines.Add('ttyXXX nicht initialisiert');
}end;






//********************************************************************
//Example for Serial Port using wiringPi-Wrapper an piRS232.pas
//for using UART0 on RPi3 -> switch off Bluetooth !!!

procedure TForm1.btOpenRS232Click(Sender: TObject);
var res: integer;
begin
  rs232:= TRS232.create();   					 //UART0 - 115200bd 8N1
  if assigned(rs232) then begin

    rs232.Baudrate := B9600;       // [B115200] Standard-Baudrates s. piRS232.pas
    //rs232.DataBits := db7;					 // [db8], db7, db6, db5
    //rs232.Parity := paEven;          // [paNone], paEven, paOdd
    //rs232.Stopbits := sbTwo;         // [sbOne], sbTow

    //rs232.Device := '/dev/ttyAMA0';  // [UART0] - Bluetooth on Pi3
    //rs232.Device := '/dev/ttyS0';    // mini-UART1
    //rs232.Device := '/dev/ttyUSB0';  // external USB-Adapter

    rs232.FlowControl := fcHardware; // [fcNone], fcHardware

    rs232.onDataRcvd:= @RxData;   		 // Rx-event
    rs232.OnTerminate := @terminatedRS232;
  	res:= rs232.Open();
    Memo1.Lines.Add('Open Rs232, fd: ' + IntToStr(res));

    //Test
//    pinMode(0, OUTPUT);
//    digitalWrite(0, LOW);


  end else ShowMessage('Error on open Serial-Port');
end;


procedure TForm1.btCloseRS232Click(Sender: TObject);
begin
  rs232.Terminate;
  Memo1.Lines.Add('Terminate');
end;


//Rx-Event
procedure TForm1.RxData(line: ansistring);
begin
  //Rx-Code in Main-Thread
	//Memo1.Lines.Add(line);
end;

procedure TForm1.terminatedRS232(Sender: tObject);
begin
  Memo1.Lines.Add('Terminated');
  rs232:= nil;
end;

procedure TForm1.btTxRs232Click(Sender: TObject);
var sl: tStringList;
    i, j, res: integer;
    s: ansistring;
begin
  {
  sl:= tStringList.Create;
  sl.Delimiter := ' ';
  sl.LoadFromFile('Test.txt');

  for j:= 0 to 3 do begin
 	 for i:= 0 to sl.Count-1 do begin
   	res:= rs232.WriteData(sl[i]);
  	Memo1.Lines.Add(IntToStr(res));
    btTest1Click(Self);
   end;
  end;
  sl.Free;
   }
  digitalWrite(4, levLow);
  for i:= 0 to 2500 do s:= s + 'U';
  digitalWrite(4, levHigh);
  res:= rs232.WriteData(s);
  Memo1.Lines.Add(IntToStr(res));
  digitalWrite(4, levLow);
  sleep(10);
  s:= '';
  for i:= 0 to 2500 do s:= s + 'U';
  digitalWrite(4, levHigh);
  res:= rs232.WriteData(s);
  Memo1.Lines.Add(IntToStr(res));
  digitalWrite(4, levLow);
   {
 	rs232.WriteData('ABC'+#10);
  rs232.WriteData('DEF'+#10);
  rs232.WriteData('GHI'+#10);
//  digitalWrite(0, HIGH);
  rs232.WriteData('JKL'+#10);
  i:= rs232.WriteData('MNO'+#10);
//  sleep(1);
  digitalWrite(0, LOW);
  }
  {
  digitalWrite(0, HIGH);
	if assigned(rs232) then rs232.WriteData(edTxLine.Text);
  sleep(1);
  digitalWrite(0, LOW);
  }
end;



procedure TForm1.btStopTxClick(Sender: TObject);
begin
  rs232.SuspendTx(tSpeedButton(Sender).Down);
end;



procedure TForm1.btSetRTSClick(Sender: TObject);
begin
	if assigned(rs232) then rs232.SetRTS(btSetRTS.Down);
end;

procedure TForm1.btGetCTSClick(Sender: TObject);
var s: string;
begin
	if assigned(rs232) then
    if rs232.GetCTS()
      then s:= 'true'
      else s:= 'false';

  Memo1.Lines.Add('CTS: ' + s);
end;

//------------------------------------------------------------------------------
end.

