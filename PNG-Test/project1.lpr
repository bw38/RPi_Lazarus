program project1;

{$mode delphi}

uses

  cthreads,  cmem,

  Interfaces, // this includes the LCL widgetset
  Forms, main, lnetvisual;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
