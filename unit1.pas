unit Unit1;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  lNetComponents;

{ TForm1 }

type
  TForm1 = class(TForm)

  private

  public
    LTCPComponent1: TLTCPComponent;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

