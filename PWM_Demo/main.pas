unit main;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  Spin, StdCtrls, ExtCtrls, baseunix,
  wiringpi;

{ TForm1 }

type
  TForm1 = class(TForm)
    btHwPwm: TSpeedButton;
    btSwPwm: TSpeedButton;
    btSetTonePin: TSpeedButton;
    btSwPwmValue: TSpeedButton;
    btSetFreq: TSpeedButton;
    edSwPin: TSpinEdit;
    edTonePin: TSpinEdit;
    edSwRange: TSpinEdit;
    edToneFreq: TSpinEdit;
    edSwValue: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edClockdiv: TSpinEdit;
    edRange: TSpinEdit;
    edValue: TSpinEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    rgMode: TRadioGroup;
    procedure btHwPwmClick(Sender: TObject);
    procedure btSetFreqClick(Sender: TObject);
    procedure btSetTonePinClick(Sender: TObject);
    procedure btSwPwmClick(Sender: TObject);
    procedure btSwPwmValueClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}


procedure TForm1.FormCreate(Sender: TObject);
var uid: longint;
begin
  uid:= fpGetUid();
  label4.Caption := 'User-ID: ' + IntToStr(uid);
  if uid <> 0 then btHWPWM.Enabled := false;
  //Setup in WiringPi-Numbering
  wiringPiSetup();
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin

end;




//PWM --------------------------------------------------------------------------

procedure TForm1.btHwPwmClick(Sender: TObject);
begin
  //Initialisierung HW-PWM GPIO.1
  pinMode(1, pmPWM_OUTPUT); //schaltet nach ALT5
  if rgMode.ItemIndex = 0
  	then pwmSetMode(pwmMS) //tPWMMode = (pwmMS, pwmBAL); Mark:Space || Balanced
    else pwmSetMode(pwmBAL);
  pwmSetClock(edClockDiv.Value);  //Clock = 19.2MHz div x
  pwmSetRange(edRange.Value);
  pwmWrite(1, edValue.Value);
end;


//------------------------------------------------------------------------------

procedure TForm1.btSwPwmClick(Sender: TObject);
begin
  softPwmCreate(edSwPin.Value, edSwValue.Value,  edSwRange.Value);
end;

procedure TForm1.btSwPwmValueClick(Sender: TObject);
begin
  softPwmWrite(edSwPin.Value, edSwValue.Value);
end;

//------------------------------------------------------------------------------

procedure TForm1.btSetTonePinClick(Sender: TObject);
begin
  softToneCreate (edTonePin.Value);
end;


procedure TForm1.btSetFreqClick(Sender: TObject);
begin
  softToneWrite (edTonePin.Value, edToneFreq.Value);
end;


end.

